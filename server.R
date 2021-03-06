library(shiny)
library(ggplot2)
library(HDF5Array)
library(reshape2)
library(cowplot)
library(ggrepel)
library(DT)
library(plotly)


#load palettes etc.
source("helper.R")

tour <- read.delim("tour.txt",
                   sep=";", stringsAsFactors=FALSE,row.names=NULL)


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
meta$sample = factor(meta$sample)


endo_meta = readRDS("endo_meta.rds")
blood_meta = readRDS("blood_meta.rds")
markers = readRDS("markers.rds")
markers_de = readRDS("markers_de.rds")


shinyServer(
  function(input, output, session){
    
    introjs(session, options=list(steps=tour))
    
    
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
    
    get_present_celltypes = reactive({
      return(unique(get_meta()$celltype)[order(match(unique(get_meta()$celltype),
                                               names(celltype_colours)))])
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
    
    
    
    get_count_gene = function(gene = "Hbb-bh1"){
      #get the gene count into memory
      count = as.numeric(link[,match(as.character(gene), as.character(genes[,2]))])
      #subsetting is much quicker now
      return(count[meta$cell %in% get_meta()$cell])
    }
    
    get_count = reactive({
      return(get_count_gene(input$gene))
    })
    

    
    get_subset = reactive({
      coord = get_coord()
      if(input$subset){
        return(subsetPointsByGrid(coord[,1], coord[,2], 100))#input$subset_degree))
      } else {
        return(rep(TRUE, nrow(coord)))
      }
    })
    
    get_subset_force = reactive({
      coord = get_coord()
      return(subsetPointsByGrid(coord[,1], coord[,2], 75))
    })
    
    #### SELCTIZE SPEEDUP
    updateSelectizeInput(session = session, inputId = 'gene', choices = genes[,2], server = TRUE, selected = "T") #DONT remove T, appears to be a bug that it vanishes
  
    
    #### REACTIVE UI ELEMENTS
    output$marker_celltype = renderUI({
      selectInput("celltype", "Cell type", get_present_celltypes())
    })
    
    output$de_celltype_1 = renderUI({
      selectInput("celltype1", "Cell type 1", get_present_celltypes())
    })
    
    output$de_celltype_2 = renderUI({
      selectInput("celltype2", "Cell type 2", get_present_celltypes(), selected = "Visceral endoderm")
    })
    
    
    #### OVERALL VIS
    
    plotOverview = reactive({
      
      
      allowed = get_subset()
      #scramble, and subset if asked
      coords = get_coord()[allowed,]
      color = get_clusters()[allowed]
      
      new_order = sample(length(color), length(color))
      
      plot = ggplot(data = coords[new_order,], 
                    mapping = aes(x = X, 
                                  y = Y, 
                                  col = color[new_order])) +
        geom_point(size = 1, 
                   alpha = 0.9) +
        # scale_colour_Publication(name = input$colourby, drop = FALSE) +
        ggtitle(switch(input$stage, "all" = "Whole dataset", input$stage)) +
        guides(colour = guide_legend(override.aes = list(size=7, 
                                                         alpha = 1))) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank()) +
        coord_fixed(ratio = 0.8) +
        labs(caption = "A summary of the selected cells is shown. Various options can be selected using the sidebar.")
      
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
                       "stage" = stage_palette,
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
        dev.off()
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
                axis.title.x = element_blank()) +
          labs(caption = "The contribution of cells from different timepoints to each cluster is shown. Bars are ordered from youngest mean age to oldest.")
        
        return(plot)
        
    })
    
    
    
    #### GENE EXPRESSION PLOTS
    
    makeGenePlot = function(gene_name, gene_counts, x_coord, y_coord){
      
      order = order(gene_counts)
      
      plot = ggplot(mapping = aes(x = x_coord[order], y = y_coord[order], col = gene_counts[order])) +
        geom_point(size = 1) +
        scale_color_gradient2(name = "Log2\nnormalised\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(gene_counts)/2) +
        theme(axis.title = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank()
              ) +
        coord_fixed(ratio = 0.8) +
        ggtitle(gene_name)
      
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
                       input$gene)) +
          labs(caption = "Gene expression information is overlaid on the main visualisation.\nPoints are plotted in order such that the highest-expressing cells are the most visible (\"on top\").")
        )
    })
    
    output$gene_plot = renderPlot({
      
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
        ggsave(file, plot = plotMainGeneVis(), device = "pdf", width = narrower_plot_width, height = big_plot_height)
        dev.off()
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
                 label = as.vector(clust.sizes)) +
        labs(caption = "Gene expression for each of the celltypes of clusters is shown. Violins are width-normalised. Numbers above each violin show the number of cells in the group")
      
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
        dev.off()
      }
    )
  
    
    #### CELLTYPE MARKERS
    
    get_markers = reactive({
      
      validate(
        need(!is.null(input$stage) & !is.null(input$celltype),
             "Loading..." )
      )
      
      tab = markers[[input$stage]][[input$celltype]]
      tab = tab[order(tab$p.value),]
      genes_mark = genes[match(rownames(tab), genes[,1]), 2]
      df = data.frame(MGI = genes_mark, p.value = tab[,1], FDR = tab[,2])
      return(df)
    })
    
    output$celltype_markers = renderDataTable({
      return(datatable(get_markers(), selection = "single"))
    })
    
    get_markers_discrim = reactive({
      
      validate(
        need(!is.null(input$celltype1) & !is.null(input$celltype2),
             "Loading..." )
      )
      
      validate(
        need(input$celltype1 != input$celltype2,
             "The selected celltypes must not be the same" )
      )
      
      tab = markers_de[[input$stage]][[input$celltype1]]
      tab = tab[order(abs(tab[, paste0("logFC.", input$celltype2)]), decreasing = TRUE),]
      tab = tab[!is.na(tab[, paste0("logFC.", input$celltype2)]),]
      
      df = data.frame(row.names = rownames(tab),
                      MGI = genes[match(rownames(tab), genes[,1]),2],
                      log2FC = tab[, paste0("logFC.", input$celltype2)])

      return(df)
    })
    
    output$celltype_markers_discrim = renderDataTable({
      return(datatable(get_markers_discrim(), selection = "single"))
    })
    
    
    
    output$celltype_presence_plot = renderPlot({
      if(input$stage == "all"){
        allowed = get_subset_force()
      } else {
        allowed = get_subset()
      }
      
      coords = get_coord()[allowed,]
      meta = get_meta()[allowed,]

      order = order(meta$celltype == input$celltype)
      
      p = ggplot(as.data.frame(coords)[order,], aes(x = X, y = Y, col = (meta$celltype == input$celltype)[order])) +
        geom_point() +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "darkgrey")) +
        theme(legend.position = "none",
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.line = element_blank()) +
        labs(caption = "Regions of the overview visualisation where the\nselected cells are present (red points) are shown.")
      
      
      return(p)
    })
    
    output$celltype_presence_plot_discrim = renderPlot({
      
      validate(
        need(!is.null(input$celltype1) & !is.null(input$celltype2),
             "Loading..." )
      )
      
      validate(
        need(input$celltype1 != input$celltype2,
             "The selected celltypes must not be the same" )
      )
      
      if(input$stage == "all"){
        allowed = get_subset_force()
      } else {
        allowed = get_subset()
      }
      
      coords = get_coord()[allowed,]
      meta = get_meta()[allowed,]
      
      cols = sapply(meta$celltype, function(x){
        if(x == input$celltype1){
          return("celltype1")
        } else if (x == input$celltype2){
          return("celltype2")
        } else {
          return("other")
        }
      })
      order = order(cols, decreasing = TRUE)
      
      
      p = ggplot(as.data.frame(coords)[order,], aes(x = X, y = Y, col = cols[order])) +
        geom_point() +
        scale_color_manual(values = c("celltype1" = "red", "celltype2" = "blue", "other" = "darkgrey")) +
        theme(legend.position = "none",
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.line = element_blank()) +
        labs(caption = "Regions of the overview visualisation where the\nselected cells are present (red - celltype 1; blue - celltype 2).")
      
      
      return(p)
    })
    
    output$celltype_gene_plot = renderPlot({
      row = input$celltype_markers_row_last_clicked
      validate(
        need(!is.null(row),
           "Please click on a gene in the marker table" )
      )
      gene= get_markers()[row, "MGI"]
      
      allowed = get_subset()
      
      return(makeGenePlot(gene_name = gene,
                          gene_counts = get_count_gene(gene)[allowed],
                          x_coord= get_coord()[allowed,1],
                          y_coord = get_coord()[allowed,2])+
               labs(caption = "Gene expression is overlaid on the overview plot."))
      
    })
    
    output$celltype_gene_plot_discrim = renderPlot({
      row = input$celltype_markers_discrim_row_last_clicked
      validate(
        need(!is.null(row),
             "Please click on a gene in the marker table" )
      )
      gene= get_markers_discrim()[row, "MGI"]
      
      allowed = get_subset()
      
      return(makeGenePlot(gene_name = gene,
                          gene_counts = get_count_gene(gene)[allowed],
                          x_coord= get_coord()[allowed,1],
                          y_coord = get_coord()[allowed,2])+
               labs(caption = "Gene expression is overlaid on the overview plot."))
      
    })
    
    #### ENDODERM PLOTS
    get_count_gene_endo = function(gene = "Hbb-bh1"){
      #get the gene count into memory
      count = as.numeric(link[,match(as.character(gene), as.character(genes[,2]))])
      #subsetting is much quicker now
      return(count[meta$cell %in% endo_meta$cell])
    }
    
    get_count_endo = reactive({
      return(get_count_gene_endo(input$gene))
    })
    
    
    

    output$endo_gephi_celltype = renderPlot({
      ro = sample(nrow(endo_meta), nrow(endo_meta))

      p = ggplot(endo_meta[ro,], aes(x = gephiX, y = gephiY, col = celltype)) +
        geom_point(size = 2) +
        celltype_palette +
        labs(x = "gephiX", y = "gephiY") +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank()) +
        coord_fixed(ratio = 0.8) +
        guides(colour = guide_legend(override.aes = list(size=7)))      
      return(p)

    })
    
    output$endo_gephi_stage = renderPlot({
      ro = sample(nrow(endo_meta), nrow(endo_meta))

      p = ggplot(endo_meta[ro,], aes(x = gephiX, y = gephiY, col = stage)) +
        geom_point(size = 2) +
        stage_palette +
        labs(x = "gephiX", y = "gephiY") +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank()) +
        coord_fixed(ratio = 0.8) +
        guides(colour = guide_legend(override.aes = list(size=7)))   
      
      return(p)
      
    })
    
    output$endo_gephi_gene = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      expr = get_count_endo()

      p = makeGenePlot(gene_name = input$gene, gene_counts = expr, x_coord = endo_meta$gephiX, y_coord = endo_meta$gephiY)
      
      return(p)
      
    })
    
    output$gut_clusters = renderPlot({
      p = ggplot(endo_meta[!is.na(endo_meta$cluster_gut),], aes(x = gutX, y= gutY, col = cluster_gut)) +
        geom_point() +
        gut_palette +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank()) +
        coord_fixed(ratio = 0.8) +
        guides(colour = guide_legend(override.aes = list(size=7)))   
      
      return(p)
    })
    
    output$gut_gene = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      expr = get_count_endo()[!is.na(endo_meta$cluster_gut)]
      sub = endo_meta[!is.na(endo_meta$cluster_gut),]
      
      p = makeGenePlot(gene_name = input$gene, gene_counts = expr, x_coord = sub$gutX, y_coord = sub$gutY)
      
      return(p)
      
    })
    
    output$gut_boxplot = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      expr = get_count_endo()[!is.na(endo_meta$cluster_gut)]
      sub = endo_meta[!is.na(endo_meta$cluster_gut),]
      
      p = ggplot(data = sub, mapping = aes(x = cluster_gut, y = expr, fill = cluster_gut)) +
        geom_boxplot() +
        gut_palette_fill +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              axis.title.x = element_blank()) +
        labs(y = "log2 normalised count") +
        theme(legend.position = "none")
      
      return(p)
    })
    
    output$gut_density_1 = renderPlot({
      
      
      sub = endo_meta[!is.na(endo_meta$gutDPT),]
      
      p = ggplot(sub, aes(x = gutDPT, fill = cluster_gut)) +
        geom_density(alpha = 0.7) +
        gut_palette_fill +
        labs(x = "DPT", y = "Density")
      
      return(p)
    })
    
    output$gut_density_2 = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      sub = endo_meta[!is.na(endo_meta$gutDPT),]
      
      expr = get_count_endo()[!is.na(endo_meta$gutDPT)]
      
      p = ggplot(mapping = aes(x = sub$gutDPT, y= expr)) +
        geom_point() +
        labs(x = "DPT", y = paste0("log2 normalised count - ", input$gene)) +
        geom_smooth(method = "loess", col = "coral", se = FALSE)
      
      return(p)
    })
    
    output$hg1_traj_1 = renderPlot({
      
      sub = endo_meta[!is.na(endo_meta$trajectory),]
      sub = sub[sub$trajectory == "VE",]
      
      p = ggplot(sub, mapping = aes(x = stage, y= traj_dpt, col = stage)) +
        geom_jitter(width = 0.3, height = 0) +
        stage_palette +
        coord_flip() +
        labs(y = "DPT") +
        theme(axis.title.y = element_blank()) +
        ggtitle("Visceral endo. to Hindgut 1")
      
      return(p)
    })
    
    output$hg1_traj_2 = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      sub = endo_meta[!is.na(endo_meta$trajectory),]
      sub = sub[sub$trajectory == "VE",]
      
      
      expr = get_count_endo()[endo_meta$cell %in% sub$cell]
      
      p = ggplot(mapping = aes(x = sub$traj_dpt, y= expr)) +
        geom_point() +
        labs(x = "DPT", y = paste0("log2 normalised count - ", input$gene)) +
        geom_smooth(method = "loess", col = "coral", se = FALSE)
      
      return(p)
    })
    
    output$hg2_traj_1 = renderPlot({
      
      sub = endo_meta[!is.na(endo_meta$trajectory),]
      sub = sub[sub$trajectory == "DE",]
      
      p = ggplot(sub, mapping = aes(x = stage, y= traj_dpt, col = stage)) +
        geom_jitter(width = 0.3, height = 0) +
        stage_palette +
        coord_flip() +
        labs(y = "DPT") +
        theme(axis.title.y = element_blank()) +
        ggtitle("Definitive endo. to Hindgut 2")
      
     return(p)
    })
    
    output$hg2_traj_2 = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      sub = endo_meta[!is.na(endo_meta$trajectory),]
      sub = sub[sub$trajectory == "DE",]
      
      expr = get_count_endo()[endo_meta$cell %in% sub$cell]
      
      p = ggplot(mapping = aes(x = sub$traj_dpt, y= expr)) +
        geom_point() +
        labs(x = "DPT", y = paste0("log2 normalised count - ", input$gene)) +
        geom_smooth(method = "loess", col = "coral", se = FALSE)
      
      return(p)
    })
    
    
    # ENDOTHELIUM PLOTS

    
    get_count_gene_blood = function(gene = "Hbb-bh1"){
      #get the gene count into memory
      count = as.numeric(link[,match(as.character(gene), as.character(genes[,2]))])
      #subsetting is much quicker now
      return(count[meta$cell %in% blood_meta$cell])
    }
    
    get_count_blood = reactive({
      return(get_count_gene_blood(input$gene))
    })
    
    
    
    output$blood_gephi_celltype = renderPlot({
      ro = sample(nrow(blood_meta), nrow(blood_meta))
      
      p = ggplot(blood_meta[ro,], aes(x = scaledX, y = scaledY, col = celltype)) +
        geom_point(size = 1) +
        celltype_palette +
        labs(x = "gephiX", y = "gephiY") +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank()) +
        coord_fixed(ratio = 0.8) +
        guides(colour = guide_legend(override.aes = list(size=7)))      
      return(p)
      
    })
    
    output$blood_gephi_subcluster = renderPlot({
      ro = sample(nrow(blood_meta), nrow(blood_meta))
      
      p = ggplot(blood_meta[ro,], aes(x = scaledX, y = scaledY, col = subcluster)) +
        geom_point(size = 1) +
        blood_palette +
        labs(x = "gephiX", y = "gephiY") +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank()) +
        coord_fixed(ratio = 0.8) +
        guides(colour = guide_legend(override.aes = list(size=7)))      
      return(p)
      
    })
    
    output$blood_gephi_stage = renderPlot({
      ro = sample(nrow(blood_meta), nrow(blood_meta))
      
      p = ggplot(blood_meta[ro,], aes(x = scaledX, y = scaledY, col = stage)) +
        geom_point(size = 1) +
        stage_palette +
        labs(x = "gephiX", y = "gephiY") +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank()) +
        coord_fixed(ratio = 0.8) +
        guides(colour = guide_legend(override.aes = list(size=7)))      
      return(p)
      
    })
    
    output$blood_gephi_gene = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      expr = get_count_blood()
      
      p = makeGenePlot(gene_name = input$gene, gene_counts = expr, x_coord = blood_meta$scaledX, y_coord = blood_meta$scaledY)
      
      return(p)
      
    })
    
    output$blood_boxplot = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      expr = get_count_blood()[!is.na(blood_meta$subcluster)]
      sub = blood_meta[!is.na(blood_meta$subcluster),]
      
      p = ggplot(data = sub, mapping = aes(x = subcluster, y = expr, fill = subcluster)) +
        geom_boxplot() +
        blood_palette_fill +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              axis.title.x = element_blank()) +
        labs(y = "log2 normalised count") +
        theme(legend.position = "none")
      
      return(p)
    })
    
    output$blood_zoom_subcluster = renderPlot({
      
      met = blood_meta[(blood_meta$gephiX > -4000) & 
                         (blood_meta$gephiY > -2500) &
                         (blood_meta$gephiX < 1500) &
                         (blood_meta$gephiY < 1500),]
      ro = sample(nrow(met), nrow(met))
      
      p = ggplot(met[ro,], aes(x = scaledX, y = scaledY, col = subcluster)) +
        geom_point(size = 1) +
        blood_palette +
        labs(x = "gephiX", y = "gephiY") +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank()) +
        coord_fixed(ratio = 1.2) +
        guides(colour = guide_legend(override.aes = list(size=7), ncol = 2))      
      return(p)
      
    })
    
    output$blood_zoom_gene = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      met = blood_meta[(blood_meta$gephiX > -4000) & 
                         (blood_meta$gephiY > -2500) &
                         (blood_meta$gephiX < 1500) &
                         (blood_meta$gephiY < 1500),]
      
      expr = get_count_blood()
      expr = expr[blood_meta$cell %in% met$cell]
      
      p = makeGenePlot(gene_name = input$gene, gene_counts = expr, x_coord = met$scaledX, y_coord = met$scaledY) +
        coord_fixed(ratio = 1.2)
      return(p)
      
    })
    
    ## 3D UMAP
    
    umap_3d = readRDS("3d_umap.rds")
    
    output$umap_3d = renderPlotly({
      pdf(NULL)
      plot_df = umap_3d
      p = plot_ly() %>%
        add_markers(data = plot_df, x = ~x, y = ~y, z = ~z, 
                    marker = list(color = ~col, sizemode = "diameter", size = 3, opacity = 1)) %>%
        layout(title = "")
            
      #some error from too many pngs is happening
      #https://stackoverflow.com/questions/38383248/plotly-graphs-shows-error-too-many-open-devices-in-shiny/38900061
      #suggests this fix
      dev.off()
      
      return(p)
    })
    
    # output$umap_3d_stage = renderPlotly({
    #   pdf(NULL)
    #   
    #   plot_df = umap_3d
    #   p = plot_ly() %>%
    #     add_markers(data = plot_df, x = ~x, y = ~y, z = ~z, 
    #                 marker = list(color = ~stage, sizemode = "diameter", size = 3, opacity = 1)) %>%
    #     layout(title = "")
    #   
    #   #some error from too many pngs is happening
    #   #https://stackoverflow.com/questions/38383248/plotly-graphs-shows-error-too-many-open-devices-in-shiny/38900061
    #   #suggests this fix
    #   dev.off()
    #   
    #   return(p)
    # })
    
    observeEvent(input$help,
                 introjs(session, options=list(steps=tour)
                 )
    )


  }
)