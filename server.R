library(shiny)
library(ggplot2)
library(HDF5Array)
library(reshape2)
library(cowplot)
library(ggrepel)
library(DT)

#load palettes etc.
source("helper.R")

# taken from iSEE
subsetPointsByGrid <- function(X, Y, resolution=200, seed = 42) {
  set.seed(seed)
  # Avoid integer overflow when computing ids.
  resolution <- max(resolution, 1L)
  resolution <- min(resolution, sqrt(.Machine$integer.max))
  resolution <- as.integer(resolution)
  
  # X and Y MUST be numeric.
  rangeX <- range(X)
  rangeY <- range(Y)
  
  binX <- (rangeX[2] - rangeX[1])/resolution
  xid <- (X - rangeX[1])/binX
  xid <- as.integer(xid)
  
  binY <- (rangeY[2] - rangeY[1])/resolution
  yid <- (Y - rangeY[1])/binY
  yid <- as.integer(yid)
  
  # Getting unique IDs, provided resolution^2 < .Machine$integer.max
  # We use fromLast=TRUE as the last points get plotted on top.
  id <- xid + yid * resolution 
  !duplicated(id, fromLast=TRUE)
}

link = HDF5Array(file = "counts.hdf5", name = "logcounts")

load("data.RData")
#ensure factor levels are correct for plotting
meta$celltype = factor(meta$celltype,
                       levels = names(celltype_colours),
                       ordered = TRUE)

meta$stage = factor(meta$stage,
                    levels = names(stage_colours),
                    ordered = TRUE)

meta$theiler = factor(meta$theiler,
                      levels = names(theiler_colours),
                      ordered = TRUE)

meta$cluster = factor(meta$cluster)
meta$cluster.stage = factor(meta$cluster.stage)
meta$cluster.theiler = factor(meta$cluster.theiler)


# endo_meta = readRDS("endo_meta.rds")
# haem_meta = readRDS("haem_meta.rds")

shinyServer(
  function(input, output, session){
    #### FUNCTIONS TO GET DATA
    
    get_meta = reactive({
      out = meta
      
      #select stage
      out = switch(substr(input$stage,1,1),
             "a" = out,
             "T" = out[out$theiler == input$stage,],
             "E" = out[out$stage == input$stage,],
             "m" = out[out$stage == input$stage,])
      
      return(out)
    })
    
    get_coord = reactive({
  
      if(input$coord_type == "tsne"){
        coord = tsnes[[input$stage]]
      } else {
        coord = umaps[[input$stage]]
      }
                                    
    
      coord = as.data.frame(coord)
      names(coord) = c("X", "Y")
      return(coord)
    })
    
    get_clusters = reactive({
      meta = get_meta()
      method = input$colourby
      out = meta[, method]
      return(out)
    })
    
    get_cluster_centroids = reactive({
      coords = get_coord()
      cluster = get_clusters()
      
      df = data.frame(X = sapply(unique(cluster), 
                                 function(x) median(coords[cluster == x, 1])),
                      Y = sapply(unique(cluster), 
                                 function(x) median(coords[cluster == x, 2])),
                      num = unique(cluster))
      return(df)
      
    })
    
    get_full_count_gene = function(gene = "Ttr"){
      count = as.numeric(link[,match(as.character(gene), as.character(genes[,2]))])
      return(count)
    })
    
    get_count_gene = function(gene = "Hbb-bh1"){
      #get the gene count into memory
      get_full_count_gene(gene)
      #subsetting is much quicker now
      return(count[meta$cell %in% get_meta()$cell])
    }
    
    get_count = reactive({
      return(get_count_gene(input$gene))
    })
    

    
    get_subset = reactive({
      coord = get_coord()
      if(input$subset){
        return(subsetPointsByGrid(coord[,1], coord[,2], input$subset_degree))
      } else {
        return(rep(TRUE, nrow(coord)))
      }
    })
    
    #### SELCTIZE SPEEDUP
    updateSelectizeInput(session = session, inputId = 'gene', choices = genes[,2], server = TRUE, selected = "T") #DONT remove T, appears to be a bug that it vanishes
  
    
    #### OVERALL VIS
    
    plotOverview = reactive({
      
      
      allowed = get_subset()
      #scramble, and subset if asked
      new_order = sample(length(allowed), length(allowed))
      
      plot = ggplot(data = get_coord()[new_order,], 
                    mapping = aes(x = X, 
                                  y = Y, 
                                  col = get_clusters()[new_order])) +
        geom_point(size = 1, 
                   alpha = 0.9) +
        # scale_colour_Publication(name = input$colourby, drop = FALSE) +
        ggtitle(switch(input$stage, "all" = "Whole dataset", input$stage)) +
        guides(colour = guide_legend(override.aes = list(size=7, 
                                                         alpha = 1))) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank()) +
        coord_fixed(ratio = 0.8) +
        labs(caption = "A summary of the selected cells. Change colouring of points using the sidebar.")
      
      if(input$numbers){
        centroids = get_cluster_centroids()
        centroids$num = gsub(" ", "\n", centroids$num)
        
        plot = plot + geom_label_repel(data = centroids, 
                                       mapping = aes(x = X, 
                                                     y = Y, 
                                                     label = num), 
                                       col = "black", 
                                       alpha = 0.8, 
                                       size = 4)
      }
      
      palette = switch(input$colourby,
                       "celltype" = celltype_palette,
                       "stage" = stage_palette_col,
                       "theiler" = theiler_palette,
                       scale_colour_Publication(name = ""))
      plot = plot + palette
      
      return(plot)
    })
    
    output$data = output$data_dummy = renderPlot({
      plotOverview()
    })
    
    output$downloadOverview <- downloadHandler(
      filename = function() { paste0("overview_", input$stage, "-cells.pdf") },
      content = function(file) {
        pdf(file = NULL)
        ggsave(file, plot = plotOverview(), device = "pdf", width = big_plot_width, height = big_plot_height)
      }
    )
    
    output$stage_contribution = renderPlot({
        tab = table(get_clusters(), get_meta()$stage)
        fractions = sweep(tab, 1, rowSums(tab), "/")
        #to determine the ordering of bars
        frac_nomixed = fractions[,colnames(fractions)!="mixed_gastrulation", drop = FALSE]
        means = apply(frac_nomixed, 1, function(x) sum(x * 1:length(x)))

        melt = melt(fractions)
        
        
        plot = ggplot(melt, aes(x = factor(Var1, levels = names(means)[order(means)]), y = value, fill = Var2)) +
          geom_bar(stat = "identity") +
          labs(y = "Fraction of cells") +
          stage_palette_fill +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1),
                axis.title.x = element_blank())
        
        return(plot)
        
    })
    
    
    
    #### GENE EXPRESSION PLOTS
    
    makeGenePlot = function(gene_name, gene_counts, x_coord, y_coord){
      
      order = order(gene_counts)
      
      plot = ggplot(mapping = aes(x = x_coord[order], y = y_coord[order], col = gene_counts[order])) +
        geom_point(size = 1) +
        scale_color_gradient2(name = "Log2\nnormalised\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(gene_counts)/2) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank()) +
        coord_fixed(ratio = 0.8)
      
      if(max(gene_counts) == 0){
        plot = plot +
          scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(gene_counts)/2)
      }
      
      return(plot)
      
    }
      
    
    plotMainGeneVis = reactive({ 
      
      dat = get_coord()
      count = get_count()
      
      allowed = get_subset()
      
      dat = dat[allowed,]
      count = count[allowed]
      
      return(
        makeGenePlot(gene_name = input$gene,
                   gene_count = count,
                   x_coord = dat[,1],
                   y_coord = dat[,2]) + 
        ggtitle(paste0(switch(input$stage, "all" = "Whole dataset", input$stage), 
                       ", ", 
                       input$gene))
        )
    })
    
    output$gene = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
              "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      return(plotMainGeneVis())

    })
    
    output$downloadGeneTSNE <- downloadHandler(
      filename = function() { paste0(input$gene, "-gene_", input$stage, "-cells.pdf") },
      content = function(file) {
        pdf(file = NULL)
        ggsave(file, plot = plotGeneTSNE(), device = "pdf", width = narrower_plot_width, height = big_plot_height)
      }
    )
    
    plotGeneViolin = reactive({
      
      clust.sizes = table(get_clusters())
      
      plot = ggplot(mapping =  aes(x = get_clusters(), 
                             y = get_count(), 
                             fill = get_clusters())) +
        geom_violin(scale = "width") +
        labs(y = "Log2 normalised count") + 
        ggtitle(paste0(switch(input$stage, "all" = "Whole dataset", input$stage), ", ", input$gene)) +
        theme(axis.title = element_text(face = "bold", size = 12),
              axis.text.y = element_text(size = 12, face = "bold"),
              axis.text.x = element_text(size = 12, face = "bold", angle = 90, hjust = 1, vjust = 0.5),
              legend.position = "none",
              axis.title.x = element_blank()) +
        annotate("text", 
                 x = factor(names(clust.sizes)), 
                 y = rep_len(c(max(get_count())*1.1, max(get_count()) * 1.2), 
                             length.out = length(clust.sizes)), 
                 label = as.vector(clust.sizes))
      
      plot = plot + switch(input$colourby,
                           "celltype" = celltype_palette_fill,
                           scale_colour_Publication(name = ""))

      
      return(plot)
    })
    
    output$gene_violin = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )

      
     return(plotGeneViolin())
      
    })
    
    output$downloadGeneViolin <- downloadHandler(
      filename = function() { paste0(input$gene, "-gene_", input$stage, "-cells_violin.pdf") },
      content = function(file) {
        pdf(file = NULL)
        ggsave(file, plot = plotGeneViolin(), device = "pdf", width = big_plot_width, height = big_plot_height)
      }
    )
  
    
    # CELLTYPE MARKERS
    
    get_markers = reactive({
      tab = markers_celltype[[input$celltype]]
      tab = tab[order(tab$IUT.p),]
      genes_mark = genes[match(rownames(tab), genes[,1]), 2]
      df = data.frame(MGI = genes_mark, p.value = tab[,1], FDR = p.adjust(tab[,1], method = "fdr"))
      return(df)
    })
    
    output$celltype_markers = renderDataTable({
      return(datatable(get_markers(), selection = "single"))
    })
    
    output$celltype_presence_plot = renderPlot({
      coords = get_coord()
      meta = get_meta()
      order = order(meta$celltype == input$celltype)
      
      p = ggplot(as.data.frame(coords)[order,], aes(x = X, y = Y, col = (meta$celltype == input$celltype)[order])) +
        geom_point() +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "darkgrey")) +
        theme(legend.position = "none",
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.line = element_blank())
      
      
      return(p)
    })
    
    output$celltype_gene_plot = renderPlot({
      row = input$celltype_markers_row_last_clicked
      validate(
        need(!is.null(row),
           "Please click on a gene in the marker table" )
      )
      gene= get_markers()[row, "MGI"]
      
      
      
      return(makeGenePlot(gene_name = gene,
                          gene_counts = get_count_gene(gene),
                          x_coord= get_coord()[,1],
                          y_coord = get_coord()[,2]))
      
    })
    
    # # ENDODERM PLOTS
    # get_endo_count = reactive({
    #   count = as.numeric(link[,match(as.character(input$gene), as.character(genes[,2]))])
    #   return(count[meta$cell %in% endo_meta$cell])
    # })
    # 
    # output$endo_pc1 = renderPlot({
    #   
    #   validate(
    #     need(input$gene %in% genes[,2],
    #          "Please select a gene; if you have already selected one, this gene is not in our annotation." )
    #   )
    #   
    #   pdf = data.frame(X = endo_meta$all_PC1,
    #                    Y = endo_meta$all_PC2,
    #                    expr = get_endo_count())
    #   
    #   pdf = pdf[order(pdf$expr),]
    #   
    #   p = ggplot(pdf, aes(x = X, y = Y, col = expr)) +
    #     geom_point(size = 2) +
    #     scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(pdf$expr)/2) +
    #     ggtitle(input$gene) +
    #     labs(x = "PC1", y = "PC2")
    # 
    #   if(max(pdf$expr) == 0){
    #     p = p +
    #       scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(pdf$expr)/2)
    #   }
    #   
    #   return(p)
    #   
    # })
    # 
    # output$endo_pc3 = renderPlot({
    #   
    #   validate(
    #     need(input$gene %in% genes[,2],
    #          "Please select a gene; if you have already selected one, this gene is not in our annotation." )
    #   )
    #   
    #   pdf = data.frame(X = endo_meta$all_PC3,
    #                    Y = endo_meta$all_PC2,
    #                    expr = get_endo_count())
    #   
    #   pdf = pdf[order(pdf$expr),]
    #   
    #   
    #   p = ggplot(pdf, aes(x = X, y = Y, col = expr)) +
    #     geom_point(size = 2) +
    #     scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(pdf$expr)/2) +
    #     ggtitle(input$gene) +
    #     labs(x = "PC1", y = "PC2")
    # 
    #   if(max(pdf$expr) == 0){
    #     p = p +
    #       scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(pdf$expr)/2)
    #   }
    #   
    #   return(p)
    # })
    # 
    # gut_clust_cols = c("Immature gut" = "black", "Pharyngeal endoderm" = "gray", "Foregut" = "#D7191C", "Midgut" = "#FDAE61", "Hind/midgut" = "#ABDDA4", "Hindgut" = "#2B83BA")
    # 
    # 
    # output$endo_late_ref = renderPlot({
    #   pdf = endo_meta
    #   pdf = pdf[!is.na(endo_meta$late_DC1),]
    #   
    #   p = ggplot(pdf, aes(x = late_DC1, y = late_DC2, col = gut_cluster)) +
    #     geom_point(size = 2) +
    #     scale_colour_manual(values = gut_clust_cols, 
    #                         name = "Cluster") +
    #     ggtitle("TS12 endoderm cells") +
    #     labs(x = "DC1", y = "DC2") +
    #     guides(colour = guide_legend(override.aes = list(size=10, 
    #                                                      alpha = 1)))
    #   
    #   return(p)
    # })
    # 
    # output$endo_late_gene = renderPlot({
    #   
    #   
    #   validate(
    #     need(input$gene %in% genes[,2],
    #          "Please select a gene; if you have already selected one, this gene is not in our annotation." )
    #   )
    #   
    #   
    #   pdf = data.frame(X = endo_meta$late_DC1,
    #                    Y = endo_meta$late_DC2,
    #                    expr = get_endo_count())
    #   pdf = pdf[!is.na(endo_meta$late_DC1),]
    #   
    #   pdf = pdf[order(pdf$expr),]
    #   
    #   
    #   p = ggplot(pdf, aes(x = X, y = Y, col = expr)) +
    #     geom_point(size = 2) +
    #     scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(pdf$expr)/2) +
    #     ggtitle(input$gene) +
    #     labs(x = "DC1", y = "DC2")
    #   
    #   if(max(pdf$expr) == 0){
    #     p = p +
    #       scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(pdf$expr)/2)
    #   }
    #   
    #   return(p)
    # })
    # 
    # output$endo_gut_axis = renderPlot({
    #   pdf = endo_meta
    #   pdf = pdf[!is.na(endo_meta$gut_DC1),]
    #   
    #   p = ggplot(pdf, aes(x = gut_DC1, fill = gut_cluster)) +
    #     geom_density(alpha = 0.5) +
    #     scale_fill_manual(values = gut_clust_cols, 
    #                         name = "Cluster") +
    #     ggtitle("Embryonic gut axis") +
    #     labs(x = "DC1", y = "Density")
    #   
    #   return(p)
    # })
    # 
    # output$endo_gut_gene = renderPlot({
    #   
    #   
    #   validate(
    #     need(input$gene %in% genes[,2],
    #          "Please select a gene; if you have already selected one, this gene is not in our annotation." )
    #   )
    #   
    #   
    #   pdf = data.frame(X = endo_meta$gut_DC1,
    #                    expr = get_endo_count())
    #   pdf = pdf[!is.na(endo_meta$gut_DC1),]
    #   
    #   p = ggplot(pdf, aes(x = X, y = expr)) +
    #     geom_point(size = 1, col = "darkgrey") +
    #     geom_smooth(se = FALSE, method = "loess", col = "black") +
    #     ggtitle(input$gene) +
    #     labs(x = "DC1", y = "log2 count")
    #   
    #   return(p)
    # })
    # 
    # output$endo_traj_gene = renderPlot({
    #   
    #   
    #   validate(
    #     need(input$gene %in% genes[,2],
    #          "Please select a gene; if you have already selected one, this gene is not in our annotation." )
    #   )
    #   
    #   
    #   
    #   pdf = data.frame(X = endo_meta$ve_hind_dpt,
    #                    expr = get_endo_count())
    #   pdf = pdf[!is.na(endo_meta$ve_hind_dpt),]
    #   
    #   p = ggplot(pdf, aes(x = X, y = expr)) +
    #     geom_point(size = 1, col = "darkgrey") +
    #     geom_smooth(se = FALSE, method = "loess", col = "black") +
    #     ggtitle(input$gene) +
    #     labs(x = "VE-Hindgut DPT", y = "log2 count")
    #   
    #   return(p)
    # })
    # 
    # # ENDOTHELIUM PLOTS
    # 
    # haem_colours <- c(
    #   'D' = '#00264d',
    #   'G'= '#cc6600',
    #   'C'= '#3377ff',
    #   'B'= "#cce6ff",
    #   'A'="#ff8000",
    #   'F'= "#ffff00",
    #   'E' = "#ffd11a",
    #   'I'= "#b3b300",
    #   'L'="#ff4da6",
    #   'K'="#ffbf80",
    #   'J'="#996633",
    #   'H'="#009900"
    #   
    # )
    # 
    # haem_labels = c(
    #   names(haem_colours),
    #   unique(haem_meta$cluster.ann)
    # )
    # names(haem_labels) = haem_labels
    # haem_labels[which(haem_labels == "Early posterior mesoderm")] = "Early posterior\nmesoderm"
    #     
    # get_haem_count = reactive({
    #   count = as.numeric(link[,match(as.character(input$gene), as.character(genes[,2]))])
    #   return(count[meta$cell %in% haem_meta$cell])
    # })
    # 
    # output$haem_clusters = renderPlot({
    #   pdf = haem_meta
    #   unq = unique(pdf$haem.clust)
    #   pdf$haem.clust = factor(pdf$haem.clust, levels = unq[order(-nchar(unq), unq)])
    #   pdf = pdf[sample(nrow(pdf), nrow(pdf)),]
    #   
    #   p = ggplot(pdf, aes(x = -haem.X, y = haem.Y, col = haem.clust)) +
    #     geom_point(size = 1) +
    #     scale_color_manual(values = c(celltype_colours, haem_colours), 
    #                        name = "Cluster",
    #                        labels = haem_labels) +
    #     guides(colour = guide_legend(override.aes = list(size=9, 
    #                                                      alpha = 1),
    #                                  ncol = 2)) +
    #     theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
    #   
    #   return(p)
    # })
    # 
    # output$haem_gene = renderPlot({
    #   
    #   validate(
    #     need(input$gene %in% genes[,2],
    #          "Please select a gene; if you have already selected one, this gene is not in our annotation." )
    #   )
    #   
    #   pdf = haem_meta
    #   pdf$expr = get_haem_count()
    #   pdf = pdf[order(pdf$expr),]
    #   
    #   p = ggplot(pdf, aes(x = -haem.X, y = haem.Y, col = expr)) +
    #     geom_point(size = 1) +
    #     scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(pdf$expr)/2) +
    #     theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
    #   
    #   if(max(pdf$expr) == 0){
    #     p = p +
    #       scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(pdf$expr)/2)
    #   }
    #   
    #   return(p)
    # })
    # 
    # output$haem_clusters_zoomed = renderPlot({
    #   pdf = haem_meta
    #   pdf = pdf[pdf$zoom,]
    #   unq = unique(pdf$haem.clust)
    #   pdf$haem.clust = factor(pdf$haem.clust, levels = unq[order(-nchar(unq), unq)])
    #   pdf = pdf[sample(nrow(pdf), nrow(pdf)),]
    #   
    #   p = ggplot(pdf, aes(x = -haem.X, y = haem.Y, col = haem.clust)) +
    #     geom_point(size = 1) +
    #     scale_color_manual(values = c(celltype_colours, haem_colours), name = "Cluster") +
    #     guides(colour = guide_legend(override.aes = list(size=9, 
    #                                                      alpha = 1),
    #                                  ncol = 2)) +
    #     theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
    #   
    #   return(p)
    # })
    # 
    # output$haem_gene_zoomed = renderPlot({
    #   validate(
    #     need(input$gene %in% genes[,2],
    #          "Please select a gene; if you have already selected one, this gene is not in our annotation." )
    #   )
    #   
    #   pdf = haem_meta
    #   pdf$expr = get_haem_count()
    #   pdf = pdf[pdf$zoom,]
    #   pdf = pdf[order(pdf$expr),]
    #   
    #   p = ggplot(pdf, aes(x = -haem.X, y = haem.Y, col = expr)) +
    #     geom_point(size = 1) +
    #     scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(pdf$expr)/2) +
    #     theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
    #   
    #   if(max(pdf$expr) == 0){
    #     p = p +
    #       scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(pdf$expr)/2)
    #   }
    #   
    #   return(p)
    # })
    

  }
)