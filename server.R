library(shiny)
library(ggplot2)
library(HDF5Array)
library(viridis)
library(reshape2)

# CLUSTER TYPES
all_names = c("Mesendoderm",
              "Ex. Emb. Ect. late",
              "Epiblast",
              "Cardiac Mesenchyme",
              "Neural crest",
              "Late parax/somit. meso.",
              "Neuroectoderm",
              "Allantois",
              "Endothelium",
              "Neural tube",
              "Ex. Emb. Ect. early",
              "Hem-endo",
              "NMP",
              "Somites",
              "Early Parax. Meso",
              "Cardiac",
              "Ex. Emb. Tissue",
              "AVE/Def. End",
              "PE",
              "Ex. Emb. Ect. mid",
              "Mesoderm proj.",
              "Notochord",
              "Erythroid",
              "Older erythroid")
names(all_names) = 1:length(all_names)

legend_order = match(c("Epiblast",
                       "Mesendoderm", 
                       "Mesoderm proj.",
                       "Early Parax. Meso",
                       "Late parax/somit. meso.",
                       "Somites",
                       "NMP",
                       "Cardiac",
                       "Cardiac Mesenchyme",
                       "Endothelium",
                       "Hem-endo",
                       "Erythroid",
                       "Older erythroid",
                       "AVE/Def. End",
                       "PE",
                       "Neuroectoderm",
                       "Neural crest",
                       "Neural tube",
                       "Notochord",
                       "Allantois",
                       "Ex. Emb. Ect. early",
                       "Ex. Emb. Ect. mid",
                       "Ex. Emb. Ect. late",
                       "Ex. Emb. Tissue"
), all_names)

# COLOURS
all_colours = c("Mesendoderm" = "darkorange2",#light Brown
                "Ex. Emb. Ect. late" = "grey20",#dark Grey
                "Epiblast" = "burlywood2",#beige
                "Cardiac Mesenchyme" = "thistle3",#light pink
                "Neural crest" = "palegreen3",#light green
                "Late parax/somit. meso." = "royalblue3",#blue
                "Neuroectoderm" = "greenyellow",#midgreen
                "Allantois" = "purple3",#purple
                "Endothelium" = "tomato1",#lightred
                "Neural tube" = "olivedrab",#darkgreen
                "Ex. Emb. Ect. early" = "grey60",#light grey
                "Hem-endo" = "firebrick1",#mid red
                "NMP" = "orange",#orange
                "Somites" = "navy",#navy
                "Early Parax. Meso" = "steelblue3",#lightblue
                "Cardiac" = "pink4",#dark pink
                "Ex. Emb. Tissue" = "grey10",#???
                "AVE/Def. End" = "coral2",#dark goldenrod
                "PE" = "chocolate4",#goldenrod
                "Ex. Emb. Ect. mid" = "grey40",#mid grey
                "Mesoderm proj." = "cadetblue1",#skyblue
                "Notochord"="black",#black
                "Erythroid" = "firebrick3",#darkred
                "Older erythroid" = "red4")#scarlet
names(all_colours) = 1:length(all_colours)


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

link = HDF5Array(file = "counts.hdf5", name = "logcounts")
# link = HDF5Array(file = "~/Desktop/test_shiny_chim/counts.hdf5", name = "logcounts")



load("data.RData")
# load("~/Desktop/test_shiny_chim/data.RData")
#REMOVE
# genes = genes_all

shinyServer(
  function(input, output){
    #### HELPFUL BITS
    run = reactive({return(input$password == "gastrulationisgreat")})
    dummy_plot = ggplot(mapping = aes(x = 1:2, y = 1:2)) + annotate("text", x = 1.5, y= 1.5, label = "password locked")
    
    #### CHANGE VARIABLES ACCORDING TO USER INPUT
    
    get_meta = reactive({
      #subset embryonic
      out = switch(input$embryonic+1,
                   meta,
                   meta[meta$embryonic,])
      
      #select stage
      out = switch(substr(input$stage,1,1),
             "a" = out,
             "T" = out[out$theiler == input$stage,],
             "E" = out[out$stage == input$stage,])
      
      return(out)
    })
    
    get_coord = reactive({
  
      
      coord = switch(input$coord,
                     tsne = switch(input$embryonic + 1,
                                   tsnes[[input$stage]],
                                   tsnes_emb[[input$stage]]),
                     graph = switch(input$embryonic + 1,
                                    layouts[[input$stage]],
                                    layouts_emb[[input$stage]]))
    
      coord = as.data.frame(coord)
      names(coord) = c("X", "Y")
      return(coord)
    })
    
    get_clusters = reactive({
      meta = get_meta()
      method = ifelse(grepl("cluster", input$colourby), input$colourby, "cluster")
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
      
      # print(input$gene)
      # print(match(as.character(input$gene), as.character(genes[,2])))
      
      return(link[meta$cell %in% get_meta()$cell,
                  match(as.character(input$gene), as.character(genes[,2]))])
      
    })
    
    #### MAKE PLOTS
    
    output$data = output$data_dummy = renderPlot({
      if(!run()){
        return(dummy_plot)
      }
      
      
      unq = as.character(unique(meta[, input$colourby]))
      factor_levels = unq[order(nchar(unq), unq)]
      
      new_order = sample(nrow(get_coord()), nrow(get_coord()))
      if(input$annot)
        factor_levels = legend_order
      
      plot = ggplot(data = get_coord()[new_order,], 
                    mapping = aes(x = X, 
                                  y = Y, 
                                  col = factor(get_meta()[new_order,input$colourby], 
                                               levels = factor_levels))) +
        geom_point(size = 2, 
                   alpha = 0.9) +
        scale_colour_Publication(name = input$colourby, drop = FALSE) +
        ggtitle(input$stage) +
        guides(colour = guide_legend(override.aes = list(size=10, 
                                                         alpha = 1))) +
        theme_bw()
      
      if(input$numbers){
        plot = plot + geom_label(data = get_cluster_centroids(), 
                                 mapping = aes(x = X, 
                                               y = Y, 
                                               label = num), 
                                 col = "black", 
                                 alpha = 0.8, 
                                 size = 7)
      }
      
      if(input$annot){
        plot = plot + scale_color_manual(values = all_colours, labels = all_names, drop = FALSE, name = "")
      }

      return(plot)
    })
    
    output$stage_contribution = renderPlot({
        if(!run()){
          return(dummy_plot)
        }
        
        tab = table(get_clusters(), get_meta()$stage)
        fractions = sweep(tab, 1, rowSums(tab), "/")
        means = apply(fractions, 1, function(x) sum(x * 1:length(x)))
        
        melt = melt(fractions)
        
        plot = ggplot(melt, aes(x = factor(Var1, levels = names(means)[order(means)]), y = value, fill = Var2)) +
          geom_bar(stat = "identity") +
          labs(x = "Cluster", y = "Fraction of cells") +
          theme_bw() +
          scale_fill_Publication(name= "")
        
        return(plot)
        
    })
    
    output$doublets = renderPlot({
      if(!run()){
        return(dummy_plot)
      }
      
      coord = get_coord()
      doub = get_meta()$doub.clust
      
      coord = coord[order(doub),]
      doub = doub[order(doub)]
      
      plot = ggplot(data = coord, 
                    mapping = aes(x = X, 
                                  y = Y, 
                                  col = factor(doub))) +
        geom_point(size = 2, 
                   alpha = 0.9) +
        scale_colour_manual(name = "Doublet call", values = c("TRUE" = "navy", "FALSE" = "darkgrey")) +
        ggtitle(input$stage) +
        guides(colour = guide_legend(override.aes = list(size=10, 
                                                         alpha = 1))) +
        theme_bw()
      
      if(input$numbers){
        plot = plot + geom_label(data = get_cluster_centroids(), 
                                 mapping = aes(x = X, 
                                               y = Y, 
                                               label = num), 
                                 col = "black", 
                                 alpha = 0.8, 
                                 size = 7)
      }
      return(plot)
    })
    
    output$libs = renderPlot({
      if(!run()){
        return(dummy_plot)
      }
      
      
      plot = ggplot(data = get_coord(), 
                    mapping = aes(x = X, 
                                  y = Y, 
                                  col = log10(libs[meta$cell %in% get_meta()$cell]))) +
        geom_point(size = 2, 
                   alpha = 0.9) +
        scale_colour_viridis(name = "log10 Library size") +
        ggtitle(input$stage) +
        theme_bw()
      
      if(input$numbers){
        plot = plot + geom_label(data = get_cluster_centroids(), 
                                 mapping = aes(x = X, 
                                               y = Y, 
                                               label = num), 
                                 col = "black", 
                                 alpha = 0.8, 
                                 size = 7)
      }
      return(plot)
    })
    
    output$gene = renderPlot({
      
      if(!run()){
        return(dummy_plot)
      }
      
      dat = get_coord()
      count = as.vector(get_count())
      
      dat = dat[order(count),]
      count = count[order(count)]
      
      plot = ggplot(data = dat,
                    mapping = aes(x = X, y = Y, col = count)) +
        geom_point(size = 2, alpha = 1) +
        scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(get_count())/2) +
        ggtitle(paste(input$stage, input$gene, sep = ", ")) +
        theme_bw()
      
      return(plot)
    })
    
    output$gene_violin = renderPlot({
      
      if(!run()){
        return(dummy_plot)
      }
      
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
        theme_bw() +
        scale_fill_Publication(name = paste(input$gene, input$colourby, sep = ", ")) +
        labs(x = "Cluster number", y = "Log2 count") + 
        ggtitle(as.character(names[ifelse(grepl("cluster", input$colourby), input$colourby, "cluster")])) +
        theme(axis.title = element_text(face = "bold", size = 12),
              axis.text = element_text(size = 10)) +
        annotate("text", 
                 x = factor(names(clust.sizes)), 
                 y = rep(c(max(get_count())*1.05, max(get_count()) * 1.1), 
                         round(length(clust.sizes)/2))[1:length(clust.sizes)], 
                 label = as.vector(clust.sizes))
      
      return(plot)
      
    })
    
    
    output$cluster1select = renderUI({
      selectInput("cluster1", "Cluster 1", choices = unique(get_clusters())[order(unique(get_clusters()))])
    })
    
    # output$cluster2select = renderUI({
    #   selectInput("cluster2", "Cluster 2", choices = unique(get_clusters()))
    # })
    
    # output$intercluster = renderTable({
    #   
    #   tab = de.markers[[input$cluster1]]
    #   broke = strsplit(names(tab), split = ".", fixed = TRUE)
    #   clusts = sapply(broke[3:length(broke)], function(x) x[2])
    #   target = which(clusts == input$cluster2) + 2
    #   
    #   tab = tab[order(abs(tab[,target]), decreasing = TRUE),]
    #   tab$gene = genes[match(rownames(tab), genes[,1]),2]
    #   
    #   return(tab[1:input$n.genes, c(ncol(tab), target)])
    #   
    # })
    
    
    #MARKER SECTION
    
    output$subset_marker = renderUI({
      options = switch(input$marker_cluster_type,
                       "all" = c("all"),
                       "theiler" = paste0("TS", 9:12),
                       "timepoint" = unique(meta$stage)[order(unique(meta$stage))])
      selectInput("marker_data_subset", "Data subset", choices = options)
    })
    
    output$cluster_marker = renderUI({
      column = switch(input$marker_cluster_type,
                      "all" = "cluster",
                      "theiler" = "cluster.theiler",
                      "timepoint" = "cluster.stage")
      
      keep = switch(input$marker_cluster_type,
                    "all" = rep(TRUE, nrow(meta)),
                    "theiler" = meta$theiler == input$marker_data_subset,
                    "timepoint" = meta$stage == input$marker_data_subset)
          
      clusters = unique(meta[keep, column])
      clusters = as.numeric(as.character(clusters))
      clusters = clusters[order(clusters)]
      selectInput("cluster_marker", "Cluster number", choices = clusters)
    })
    
    output$markers = renderTable({
      
      type = input$marker_data_subset
      
      tab = markers[[type]][[as.character(input$cluster_marker)]]
      genes_mark = genes[match(rownames(tab), genes[,1]), 2]
      return(data.frame(genes = genes_mark, p.unadj = tab[,1])[1:input$n.genes,])
      
    })
    
    # output$intracluster = renderTable({
    #   tab = cluster_comp[[input$cluster1]][[1]]
    #   return(data.frame(gene = genes[match(rownames(tab), genes[,1]), 2],
    #                     fdr = tab[,2],
    #                     FC = tab[,3])[1:input$n.genes,])
    # })
  }
)