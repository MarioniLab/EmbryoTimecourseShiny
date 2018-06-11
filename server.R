library(shiny)
library(ggplot2)
library(HDF5Array)
library(reshape2)
library(cowplot)
library(ggrepel)

# COLOURS
top_colours = c("PS/mesendoderm" = "#efd5a0",#grey-brown ###
                "ExE ectoderm 2" = "grey20",#darkgrey###
                "Epiblast" = "#663300",#dark brown###
                "Neural crest" = "palegreen3",#light green
                "Late parax. mesoderm" = "royalblue3",#blue
                "Neuroectoderm" = "greenyellow",#midgreen
                "ExE mesoderm" = "purple3",#purple
                "Hemato-endothelial" = "orange",#orange
                "Neural tube" = "olivedrab",#darkgreen
                "ExE ectoderm 1" = "grey60",#light grey
                "NMPs" = "#FAFF0A",#yellow
                "Mixed mesoderm" = "navy",#navy
                "Early parax. mesoderm" = "steelblue1",#lightblue
                "Parietal endoderm" = "grey10",#???
                "AVE/def. endo/notochord" = "coral2",#dark goldenrod
                "ExE endoderm" = "plum4",#plum ###
                "Mesoderm progenitors" = "turquoise",#skyblue
                "Erythroid 1" = "firebrick3",#darkred
                "Erythroid 2" = "red4",
                "Visceral endoderm" = "lightpink1")#pink


celltype_colours = c(
  "Epiblast"	= "#683612",
  "Primitive Streak"	= "#DABE99", #was PS/mesendoderm
  "PGC"	= "#FACB12",
  "Early mixed mesoderm"	= "#C594BF",
  "Early posterior mesoderm"	= "#DFCDE4",#was Early ExE mesoderm
  "ExE mesoderm"	= "#7253A2",
  "Allantois"	= "#532C8A",
  "Endothelium"	= "#B3793B",
  "Haemato-endothelial progenitors"	= "#FBBE92",#was Hemato-
  "Erythroid 1"	= "#C72228",
  "Erythroid 2"	= "#EF4E22",
  "Cardiac mesenchyme"	= "#F7901D",
  "Cardiomyocytes"	= "#B51D8D",
  "Early paraxial mesoderm"	= "#3F84AA",
  "Pharyngeal mesoderm"	= "#C9EBFB",#was "Late mixed mesoderm
  "Intermediate mesoderm"	= "#139992",
  "Late paraxial mesoderm"	= "#8DB5CE", #was Late parax. mesoderm
  "Somites"	= "#005579",
  "Early neurectoderm"	= "#A0CC47",
  "Forebrain"	= "#65A83E",
  "Midbrain/Hindbrain"	= "#354E23",
  "Cranial neural crest"	= "#C3C388",#was Pre-migratory Neural Crest?
  "Trunk neural crest"	= "#77783C",#was Neural crest
  "Placodes"	= "#BBDCA8",
  "NMP"	= "#8EC792",
  "Spinal cord"	= "#CDE088",
  "Surface ectoderm"	= "#FFF574",
  "Notochord"	= "#0F4A9C",
  "Def. endoderm"	= "#F397C0",
  "Foregut"	= "#EF5A9D",
  "Midgut/Hindgut"	= "#CE4E82",
  "Visceral endoderm"	= "#F6BFCB",
  "Parietal endoderm"	= "#1A1A1A",
  "ExE endoderm"	= "#7F6874",
  "ExE ectoderm 1"	= "#989898",
  "ExE ectoderm 2"	= "#333333")

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour", "Publication",
                 manual_pal(values = c(
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
  library(scales)
  discrete_scale("fill", "Publication",
                 manual_pal(values = c(
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

load("data_mini.RData")
endo_meta = readRDS("endo_meta.rds")
haem_meta = readRDS("haem_meta.rds")

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
  
      
      coord = tsnes[[input$stage]]
                                    
    
      coord = as.data.frame(coord)
      names(coord) = c("X", "Y")
      return(coord)
    })
    
    get_clusters = reactive({
      meta = get_meta()
      method = ifelse(grepl("cluster", input$colourby), input$colourby, "cluster.ann")
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
    
    get_subset = reactive({
      coord = get_coord()
      if(input$subset){
        return(subsetPointsByGrid(coord[,1], coord[,2], input$subset_degree))
      } else {
        return(rep(TRUE, nrow(coord)))
      }
    })
    
    #### SELCTIZE SPEEDUP
    updateSelectizeInput(session = session, inputId = 'gene', choices = genes[,2], server = TRUE)
    
    #### OVERALL VIS
    
    output$data = output$data_dummy = renderPlot({
      
      
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
      
      if(input$colourby == "cluster.ann0"){
        plot = plot + scale_color_manual(values = top_colours, drop = FALSE, name = "") + 
          guides(colour = guide_legend(override.aes = list(size=9, 
                                                           alpha = 1),
                                       ncol = 2))
      }
      
      if(input$colourby == "cluster.ann"){
        plot = plot + scale_color_manual(values = celltype_colours, drop = FALSE, name = "") +
          guides(colour = guide_legend(override.aes = list(size=9, 
                                                           alpha = 1),
                                       ncol = 2))
      }
      
      if(input$colourby == "stage" | input$colourby == "theiler"){
        plot = plot + scale_color_manual(values = c(brewer_pal(palette = "Spectral")(length(factor_levels)-1), "darkgrey"), name = "")
      }

      return(plot)
    })
    
    output$stage_contribution = renderPlot({

        
        tab = table(get_clusters(), get_meta()$stage)
        fractions = sweep(tab, 1, rowSums(tab), "/")
        frac_nomixed = fractions[,colnames(fractions)!="mixed_gastrulation", drop = FALSE]
        means = apply(frac_nomixed, 1, function(x) sum(x * 1:length(x)))

        melt = melt(fractions)
        
        palette = c(brewer_pal(palette = "Spectral")(length(unique(meta$stage))-1), "darkgrey")
        names(palette) = unique(meta$stage)[order(unique(meta$stage))]
        
        plot = ggplot(melt, aes(x = factor(Var1, levels = names(means)[order(means)]), y = value, fill = Var2)) +
          geom_bar(stat = "identity") +
          labs(x = "Cluster", y = "Fraction of cells") +
          scale_fill_manual(values = palette, name= "") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1))
        
        return(plot)
        
    })
    
    
    
    #### GENE EXPRESSION PLOTS
    
    output$gene = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
              "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      
      dat = get_coord()
      count = as.vector(get_count())
      

      allowed = get_subset()
      #order so that highest expressing cells are not hidden behind others
      dat = dat[order(count),]
      allowed = allowed[order(count)]
      count = count[order(count)]
      
      plot = ggplot(data = dat[allowed,],
                    mapping = aes(x = X, y = Y, col = count[allowed])) +
        geom_point(size = 1) +
        scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(count)/2) +
        ggtitle(paste0(input$stage, " cells, ", input$gene)) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())

      if(max(count) == 0){
        plot = plot +
          scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(count)/2)
      }
      
      return(plot)

    })
    
    output$gene_violin = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )

      
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
    
  
    
    # CELLTYPE MARKERS
    output$celltype_markers = renderTable({

      
      tab = markers_celltype[[input$celltype]]
      tab = tab[order(tab$IUT.p),]
      genes_mark = genes[match(rownames(tab), genes[,1]), 2]
      return(data.frame(genes = genes_mark, p.unadj = tab[,1])[1:input$n.genes,])
    })
    
    output$celltype_presence_plot = renderPlot({
      coords = tsnes$all
      meta = meta
      order = order(meta$cluster.ann == input$celltype)
      
      p = ggplot(as.data.frame(coords)[order,], aes(x = V1, y = V2, col = (meta$cluster.ann == input$celltype)[order])) +
        geom_point() +
        scale_color_manual(values = c("TRUE" = "navy", "FALSE" = "darkgrey")) +
        theme(legend.position = "none",
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.line = element_blank())
      
      
      return(p)
    })
    
    # ENDODERM PLOTS
    get_endo_count = reactive({
      count = as.numeric(link[,match(as.character(input$gene), as.character(genes[,2]))])
      return(count[meta$cell %in% endo_meta$cell])
    })
    
    output$endo_pc1 = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      pdf = data.frame(X = endo_meta$all_PC1,
                       Y = endo_meta$all_PC2,
                       expr = get_endo_count())
      
      pdf = pdf[order(pdf$expr),]
      
      p = ggplot(pdf, aes(x = X, y = Y, col = expr)) +
        geom_point(size = 2) +
        scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(pdf$expr)/2) +
        ggtitle(input$gene) +
        labs(x = "PC1", y = "PC2")

      if(max(pdf$expr) == 0){
        p = p +
          scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(pdf$expr)/2)
      }
      
      return(p)
      
    })
    
    output$endo_pc3 = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      pdf = data.frame(X = endo_meta$all_PC3,
                       Y = endo_meta$all_PC2,
                       expr = get_endo_count())
      
      pdf = pdf[order(pdf$expr),]
      
      
      p = ggplot(pdf, aes(x = X, y = Y, col = expr)) +
        geom_point(size = 2) +
        scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(pdf$expr)/2) +
        ggtitle(input$gene) +
        labs(x = "PC1", y = "PC2")

      if(max(pdf$expr) == 0){
        p = p +
          scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(pdf$expr)/2)
      }
      
      return(p)
    })
    
    gut_clust_cols = c("Immature gut" = "black", "Pharyngeal endoderm" = "gray", "Foregut" = "#D7191C", "Midgut" = "#FDAE61", "Hind/midgut" = "#ABDDA4", "Hindgut" = "#2B83BA")

    
    output$endo_late_ref = renderPlot({
      pdf = endo_meta
      pdf = pdf[!is.na(endo_meta$late_DC1),]
      
      p = ggplot(pdf, aes(x = late_DC1, y = late_DC2, col = gut_cluster)) +
        geom_point(size = 2) +
        scale_colour_manual(values = gut_clust_cols, 
                            name = "Cluster") +
        ggtitle("TS12 endoderm cells") +
        labs(x = "DC1", y = "DC2") +
        guides(colour = guide_legend(override.aes = list(size=10, 
                                                         alpha = 1)))
      
      return(p)
    })
    
    output$endo_late_gene = renderPlot({
      
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      
      pdf = data.frame(X = endo_meta$late_DC1,
                       Y = endo_meta$late_DC2,
                       expr = get_endo_count())
      pdf = pdf[!is.na(endo_meta$late_DC1),]
      
      pdf = pdf[order(pdf$expr),]
      
      
      p = ggplot(pdf, aes(x = X, y = Y, col = expr)) +
        geom_point(size = 2) +
        scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(pdf$expr)/2) +
        ggtitle(input$gene) +
        labs(x = "DC1", y = "DC2")
      
      if(max(pdf$expr) == 0){
        p = p +
          scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(pdf$expr)/2)
      }
      
      return(p)
    })
    
    output$endo_gut_axis = renderPlot({
      pdf = endo_meta
      pdf = pdf[!is.na(endo_meta$gut_DC1),]
      
      p = ggplot(pdf, aes(x = gut_DC1, fill = gut_cluster)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = gut_clust_cols, 
                            name = "Cluster") +
        ggtitle("Embryonic gut axis") +
        labs(x = "DC1", y = "Density")
      
      return(p)
    })
    
    output$endo_gut_gene = renderPlot({
      
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      
      pdf = data.frame(X = endo_meta$gut_DC1,
                       expr = get_endo_count())
      pdf = pdf[!is.na(endo_meta$gut_DC1),]
      
      p = ggplot(pdf, aes(x = X, y = expr)) +
        geom_point(size = 1, col = "darkgrey") +
        geom_smooth(se = FALSE, method = "loess", col = "black") +
        ggtitle(input$gene) +
        labs(x = "DC1", y = "log2 count")
      
      return(p)
    })
    
    output$endo_traj_gene = renderPlot({
      
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      
      
      pdf = data.frame(X = endo_meta$ve_hind_dpt,
                       expr = get_endo_count())
      pdf = pdf[!is.na(endo_meta$ve_hind_dpt),]
      
      p = ggplot(pdf, aes(x = X, y = expr)) +
        geom_point(size = 1, col = "darkgrey") +
        geom_smooth(se = FALSE, method = "loess", col = "black") +
        ggtitle(input$gene) +
        labs(x = "VE-Hindgut DPT", y = "log2 count")
      
      return(p)
    })
    
    # ENDOTHELIUM PLOTS
    
    haem_colours <- c(
      'D' = '#00264d',
      'G'= '#cc6600',
      'C'= '#3377ff',
      'B'= "#cce6ff",
      'A'="#ff8000",
      'F'= "#ffff00",
      'E' = "#ffd11a",
      'I'= "#b3b300",
      'L'="#ff4da6",
      'K'="#ffbf80",
      'J'="#996633",
      'H'="#009900"
      
    )
    
    haem_labels = c(
      names(haem_colours),
      unique(haem_meta$cluster.ann)
    )
    names(haem_labels) = haem_labels
    haem_labels[which(haem_labels == "Early posterior mesoderm")] = "Early posterior\nmesoderm"
        
    get_haem_count = reactive({
      count = as.numeric(link[,match(as.character(input$gene), as.character(genes[,2]))])
      return(count[meta$cell %in% haem_meta$cell])
    })
    
    output$haem_clusters = renderPlot({
      pdf = haem_meta
      unq = unique(pdf$haem.clust)
      pdf$haem.clust = factor(pdf$haem.clust, levels = unq[order(-nchar(unq), unq)])
      pdf = pdf[sample(nrow(pdf), nrow(pdf)),]
      
      p = ggplot(pdf, aes(x = -haem.X, y = haem.Y, col = haem.clust)) +
        geom_point(size = 1) +
        scale_color_manual(values = c(celltype_colours, haem_colours), 
                           name = "Cluster",
                           labels = haem_labels) +
        guides(colour = guide_legend(override.aes = list(size=9, 
                                                         alpha = 1),
                                     ncol = 2)) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
      
      return(p)
    })
    
    output$haem_gene = renderPlot({
      
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      pdf = haem_meta
      pdf$expr = get_haem_count()
      pdf = pdf[order(pdf$expr),]
      
      p = ggplot(pdf, aes(x = -haem.X, y = haem.Y, col = expr)) +
        geom_point(size = 1) +
        scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(pdf$expr)/2) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
      
      if(max(pdf$expr) == 0){
        p = p +
          scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(pdf$expr)/2)
      }
      
      return(p)
    })
    
    output$haem_clusters_zoomed = renderPlot({
      pdf = haem_meta
      pdf = pdf[pdf$zoom,]
      unq = unique(pdf$haem.clust)
      pdf$haem.clust = factor(pdf$haem.clust, levels = unq[order(-nchar(unq), unq)])
      pdf = pdf[sample(nrow(pdf), nrow(pdf)),]
      
      p = ggplot(pdf, aes(x = -haem.X, y = haem.Y, col = haem.clust)) +
        geom_point(size = 1) +
        scale_color_manual(values = c(celltype_colours, haem_colours), name = "Cluster") +
        guides(colour = guide_legend(override.aes = list(size=9, 
                                                         alpha = 1),
                                     ncol = 2)) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
      
      return(p)
    })
    
    output$haem_gene_zoomed = renderPlot({
      validate(
        need(input$gene %in% genes[,2],
             "Please select a gene; if you have already selected one, this gene is not in our annotation." )
      )
      
      pdf = haem_meta
      pdf$expr = get_haem_count()
      pdf = pdf[pdf$zoom,]
      pdf = pdf[order(pdf$expr),]
      
      p = ggplot(pdf, aes(x = -haem.X, y = haem.Y, col = expr)) +
        geom_point(size = 1) +
        scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(pdf$expr)/2) +
        theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
      
      if(max(pdf$expr) == 0){
        p = p +
          scale_color_gradient2(name = "Log2\ncounts", mid = "gray75", low = "gray75", high = "gray75", midpoint = max(pdf$expr)/2)
      }
      
      return(p)
    })
    

  }
)