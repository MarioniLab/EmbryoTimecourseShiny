library(shiny)
library(ggplot2)
library(HDF5Array)
library(reshape2)
library(cowplot)
library(ggrepel)
library(DT)

big_plot_width = 9 * 1.5
big_plot_height = 5 * 1.5
narrower_plot_width = 6.5 * 1.5

# COLOURS
celltype_colours = c("Epiblast" = "#635547",
                     "Primitive Streak" = "#DABE99",
                     "Caudal epiblast" = "#9e6762",
                     
                     "PGC" = "#FACB12",
                     
                     "Anterior Primitive Streak" = "#c19f70",
                     "Notochord" = "#0F4A9C",
                     "Def. endoderm" = "#F397C0",
                     "Gut" = "#EF5A9D",
                     
                     "Nascent mesoderm" = "#C594BF",
                     "Mixed mesoderm" = "#DFCDE4",
                     "Intermediate mesoderm" = "#139992",
                     "Caudal Mesoderm" = "#3F84AA",
                     "Paraxial mesoderm" = "#8DB5CE",
                     "Somitic mesoderm" = "#005579",
                     "Pharyngeal mesoderm" = "#C9EBFB",
                     "Cardiomyocytes" = "#B51D8D",
                     "Allantois" = "#532C8A",
                     "ExE mesoderm" = "#8870ad",
                     "Mesenchyme" = "#cc7818",
                     
                     "Haematoendothelial progenitors" = "#FBBE92",
                     "Endothelium" = "#ff891c",
                     "Blood progenitors 1" = "#f9decf",
                     "Blood progenitors 2" = "#c9a997",
                     "Erythroid1" = "#C72228",
                     "Erythroid2" = "#f79083",
                     "Erythroid3" = "#EF4E22",
                     
                     "NMP" = "#8EC792",
                     
                     "Rostral neurectoderm" = "#65A83E",
                     "Caudal neurectoderm" = "#354E23",
                     "Neural crest" = "#C3C388",
                     "Forebrain/Midbrain/Hindbrain" = "#647a4f",
                     "Spinal cord" = "#CDE088",
                     
                     "Surface ectoderm" = "#f7f79e",
                     
                     "Visceral endoderm" = "#F6BFCB",
                     "ExE endoderm" = "#7F6874",
                     "ExE ectoderm" = "#989898",
                     "Parietal endoderm" = "#1A1A1A"
                     
)

stage_colours = c("E6.5" = "#D53E4F",
                  "E6.75" = "#F46D43",
                  "E7.0" = "#FDAE61",
                  "E7.25" = "#FEE08B",
                  "E7.5" = "#FFFFBF",
                  "E7.75" = "#E6F598",
                  "E8.0" = "#ABDDA4",
                  "E8.25" = "#66C2A5",
                  "E8.5" = "#3288BD",
                  "mixed_gastrulation" = "#A9A9A9")

stage_labels = names(stage_colours)
names(stage_labels) = names(stage_colours)
stage_labels[10] = "Mixed"

scale_colour_Publication <- function(...){
  # require(scales)
  discrete_scale("colour", "Publication",
                 scales::manual_pal(values = c(
                   "#000000", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
                   "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
                   "#5A0007", "#809693", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
                   "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
                   "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
                   "#372101", "#FFB500", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
                   "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
                   "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
                   "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
                   "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
                   "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
                   "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
                   "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C")), ...)
}

scale_fill_Publication <- function(...){
  # require(scales)
  discrete_scale("fill", "Publication",
                 scales::manual_pal(values = c(
                   "#000000", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
                   "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
                   "#5A0007", "#809693", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
                   "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
                   "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
                   "#372101", "#FFB500", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
                   "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
                   "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
                   "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
                   "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
                   "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
                   "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
                   "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C")), ...)
}

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
      method =  input$colourby
      return(meta[, method])
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
    
    get_count = reactive({
      
      #get the gene count into memory
      count = as.numeric(link[,match(as.character(input$gene), as.character(genes[,2]))])
      #subsetting is much quicker now
      return(count[meta$cell %in% get_meta()$cell])
      
    })
    
    get_count_gene = function(gene = "Hbb-bh1"){
      
      #get the gene count into memory
      count = as.numeric(link[,match(as.character(gene), as.character(genes[,2]))])
      #subsetting is much quicker now
      return(count[meta$cell %in% get_meta()$cell])
      
    }
    
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
      
      
      unq = as.character(unique(get_meta()[, input$colourby]))
      if(grepl("E[6-8]", unq[1]) | grepl("cluster.ann", input$colourby)){
        factor_levels = unq[order(unq)]
      } else {
        factor_levels = unq[order(nchar(unq), unq)]
        
      }
      allowed = get_subset()
      #scramble, and subset if asked
      new_order = sample(length(allowed), length(allowed))
      
      plot = ggplot(data = get_coord()[new_order,], 
                    mapping = aes(x = X, 
                                  y = Y, 
                                  col = factor(get_meta()[new_order,input$colourby], 
                                               levels = factor_levels))) +
        geom_point(size = 1, 
                   alpha = 0.9) +
        scale_colour_Publication(name = input$colourby, drop = FALSE) +
        ggtitle(input$stage) +
        guides(colour = guide_legend(override.aes = list(size=9, 
                                                         alpha = 1))) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
      
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
      
      # if(input$colourby == "cluster.ann0"){
      #   plot = plot + scale_color_manual(values = top_colours, drop = FALSE, name = "") + 
      #     guides(colour = guide_legend(override.aes = list(size=9, 
      #                                                      alpha = 1),
      #                                  ncol = 2))
      # }
      
      if(input$colourby == "celltype"){
        plot = plot + scale_color_manual(values = celltype_colours, drop = FALSE, name = "") +
          guides(colour = guide_legend(override.aes = list(size=9, 
                                                           alpha = 1),
                                       ncol = 2))
      }
      
      if(input$colourby == "stage" | input$colourby == "theiler"){
        plot = plot + scale_color_manual(values = c(scales::brewer_pal(palette = "Spectral")(length(factor_levels)-1), "darkgrey"), 
                                         name = "")
      }
      
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
        frac_nomixed = fractions[,colnames(fractions)!="mixed_gastrulation", drop = FALSE]
        means = apply(frac_nomixed, 1, function(x) sum(x * 1:length(x)))

        melt = melt(fractions)
        
        palette = c(scales::brewer_pal(palette = "Spectral")(length(unique(meta$stage))-1), "darkgrey")
        names(palette) = unique(meta$stage)[order(unique(meta$stage))]
        
        plot = ggplot(melt, aes(x = factor(Var1, levels = names(means)[order(means)]), y = value, fill = Var2)) +
          geom_bar(stat = "identity") +
          labs(x = "Cluster", y = "Fraction of cells") +
          scale_fill_manual(values = palette, name= "") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))
        
        return(plot)
        
    })
    
    
    
    #### GENE EXPRESSION PLOTS
    
    makeGenePlot = function(gene){
      dat = get_coord()
      count = as.vector(get_count_gene(gene))
      
      
      allowed = get_subset()
      #order so that highest expressing cells are not hidden behind others
      dat = dat[order(count),]
      allowed = allowed[order(count)]
      count = count[order(count)]
      
      plot = ggplot(data = dat[allowed,],
                    mapping = aes(x = X, y = Y, col = count[allowed])) +
        geom_point(size = 1) +
        scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(count)/2) +
        ggtitle(gene) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
      
      if(max(count) == 0){
        plot = plot +
          scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(count)/2)
      }
      
      return(plot)
    }
    
    plotGeneTSNE = reactive({      
      makeGenePlot(gene = input$gene) + ggtitle(paste0(input$stage, " cells, ", input$gene))
    })
    
    output$gene = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
              "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      return(plotGeneTSNE())

    })
    
    output$downloadGeneTSNE <- downloadHandler(
      filename = function() { paste0(input$gene, "-gene_", input$stage, "-cells.pdf") },
      content = function(file) {
        pdf(file = NULL)
        ggsave(file, plot = plotGeneTSNE(), device = "pdf", width = narrower_plot_width, height = big_plot_height)
      }
    )
    
    plotGeneViolin = reactive({
      pdf = data.frame(count = get_count(), cluster = get_clusters())
      
      names = c("sample" = "Sample",
                "cluster.stage" = "Stage (Ex.x) cluster",
                "cluster.theiler" = "Theiler stage clusters",
                "stage" = "Developmental stage",
                "cluster" = "All-data clusters",
                "celltype" = "Predicted cell type")
      
      clust.sizes = table(get_clusters())
      
      plot = ggplot(pdf, aes(x = factor(cluster, levels = unique(cluster[order(cluster)])), 
                             y = count, 
                             fill = factor(cluster, levels = unique(cluster[order(cluster)])))) +
        geom_violin(scale = "width") +
        scale_fill_manual(values = celltype_colours, name = paste(input$gene, input$colourby, sep = ", ")) +
        labs(x = "Cluster number", y = "Log2 count") + 
        ggtitle(paste(input$gene, "-", input$stage, "cells")) +
        theme(axis.title = element_text(face = "bold", size = 12),
              axis.text = element_text(size = 12, face = "bold"),
              legend.position = "none",
              axis.title.x = element_blank()) +
        annotate("text", 
                 x = factor(names(clust.sizes)), 
                 y = rep(c(max(get_count())*1.05, max(get_count()) * 1.1), 
                         round(length(clust.sizes)/2))[1:length(clust.sizes)], 
                 label = as.vector(clust.sizes))
      
      if(input$colourby == "cluster.ann"){
        plot= plot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      }
      
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
      df = data.frame(MGI = genes_mark, p.value = tab[,1], FDR = p.adjust(tab[,1], method = "fdr"))#[1:input$n.genes,]
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
           "Please select a gene in the marker table" )
      )
      gene= get_markers()[row, "MGI"]
      
      
      return(makeGenePlot(gene))
      
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