library(shiny)
library(ggplot2)
library(HDF5Array)
library(viridis)
library(reshape2)
# CLUSTER TYPES
all_names = c("Epiblast",
              "PS/mesendoderm",
              "Erythroid 1",
              "NMPs",
              "Late neural tube/spinal cord",
              "ExE endoderm",
              "ExE mesoderm",
              "ExE ectoderm 1",
              "Neuroectoderm",
              "Hemato-endothelial",
              "Early parax. mesoderm",
              "ExE ectoderm 2",
              "Late parax. mesoderm",
              "AVE/def. endo/notochord",
              "Erythroid 2",
              "Late mixed mesoderm",
              "Visceral endoderm",
              "Early mixed mesoderm",
              "Parietal endoderm",
              "Neural crest/non-neural ectoderm")
names(all_names) = 1:length(all_names)

legend_order = match(c("Epiblast",
                       "PS/mesendoderm", 
                       "AVE/def. endo/notochord",
                       "Early mixed mesoderm",
                       "Early parax. mesoderm",
                       "Late parax. mesoderm",
                       "Late mixed mesoderm",
                       "Hemato-endothelial",
                       "Erythroid 1",
                       "Erythroid 2",
                       "ExE mesoderm",
                       "NMPs",
                       "Neuroectoderm",
                       "Late neural tube/spinal cord",
                       "ExE endoderm",
                       "Visceral endoderm",
                       "Neural crest/non-neural ectoderm",
                       "ExE ectoderm 1",
                       "ExE ectoderm 2",
                       "Parietal endoderm"
), all_names)

# COLOURS
all_colours = c("PS/mesendoderm" = "#efd5a0",#grey-brown ###
                "ExE ectoderm 2" = "grey20",#darkgrey###
                "Epiblast" = "#663300",#dark brown###
                "Neural crest/non-neural ectoderm" = "palegreen3",#light green
                "Late parax. mesoderm" = "royalblue3",#blue
                "Neuroectoderm" = "greenyellow",#midgreen
                "ExE mesoderm" = "purple3",#purple
                "Hemato-endothelial" = "orange",#orange
                "Late neural tube/spinal cord" = "olivedrab",#darkgreen
                "ExE ectoderm 1" = "grey60",#light grey
                "NMPs" = "#FAFF0A",#yellow
                "Late mixed mesoderm" = "navy",#navy
                "Early parax. mesoderm" = "steelblue1",#lightblue
                "Parietal endoderm" = "grey10",#???
                "AVE/def. endo/notochord" = "coral2",#dark goldenrod
                "ExE endoderm" = "plum4",#plum ###
                "Early mixed mesoderm" = "turquoise",#skyblue
                "Erythroid 1" = "firebrick3",#darkred
                "Erythroid 2" = "red4",
                "Visceral endoderm" = "lightpink1")#pink
all_colours = all_colours[match(all_names, names(all_colours))]
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

load("data.RData")


shinyServer(
  function(input, output){
    #### HELPFUL BITS
    run = reactive({return(input$password == "gastrulationisgreat")})
    dummy_plot = ggplot(mapping = aes(x = 1:2, y = 1:2)) + annotate("text", x = 1.5, y= 1.5, label = "password locked")
    
    #### FUNCTIONS TO GET DATA
    
    get_meta = reactive({
      #subset embryonic
      out = switch(input$embryonic+1,
                   meta,
                   meta[meta$embryonic,])
      
      #select stage
      out = switch(substr(input$stage,1,1),
             "a" = out,
             "T" = out[out$theiler == input$stage,],
             "E" = out[out$stage == input$stage,],
             "m" = out[out$stage == input$stage,])
      
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
      
      return(link[meta$cell %in% get_meta()$cell,
                  match(as.character(input$gene), as.character(genes[,2]))])
      
    })
    
    #### OVERALL VIS
    
    output$data = output$data_dummy = renderPlot({
      if(!run()){
        return(dummy_plot)
      }
      
      
      unq = as.character(unique(get_meta()[, input$colourby]))
      if(grepl("E", unq[1])){
        factor_levels = unq[order(unq)]
      } else {
        factor_levels = unq[order(nchar(unq), unq)]
        
      }
      
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
      
      if(input$colourby == "stage" | input$colourby == "theiler"){
        plot = plot + scale_color_manual(values = c(brewer_pal(palette = "Spectral")(length(factor_levels)-1), "darkgrey"), name = "")
      }

      return(plot)
    })
    
    output$stage_contribution = renderPlot({
        if(!run()){
          return(dummy_plot)
        }
        
        tab = table(get_clusters(), get_meta()$stage)
        fractions = sweep(tab, 1, rowSums(tab), "/")
        frac_nomixed = fractions[,colnames(fractions)!="mixed_gastrulation"]
        means = apply(frac_nomixed, 1, function(x) sum(x * 1:length(x)))

        melt = melt(fractions)
        
        palette = c(brewer_pal(palette = "Spectral")(length(unique(meta$stage))-1), "darkgrey")
        names(palette) = unique(meta$stage)[order(unique(meta$stage))]
        
        plot = ggplot(melt, aes(x = factor(Var1, levels = names(means)[order(means)]), y = value, fill = Var2)) +
          geom_bar(stat = "identity") +
          labs(x = "Cluster", y = "Fraction of cells") +
          theme_bw() +
          scale_fill_manual(values = palette, name= "")
        
        return(plot)
        
    })
    
    
    #### LIBRARY SIZES
    
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
    
    #### GENE EXPRESSION PLOTS
    
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
    
    
    
    #MARKERS
    
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
    
    # SUBCLUSTERS
    #note that this doesn't depend at all on the get_* functions, as it operates on a different scale.
    
    output$subcluster_marker_choice = renderUI({
      options = unique(meta$cluster.sub[meta$cluster == input$subcluster_choice])
      options = options[order(options)]
      selectInput("subcluster_marker_choice", "Selected subcluster (for markers)", choices = options)
    })
    
    output$subcluster_plot = output$subcluster_plot_dummy = renderPlot({
      if(input$coord == "tsne"){
        coords = tsnes_sub[[as.character(input$subcluster_choice)]]
      } else {
        coords = layouts_sub[[as.character(input$subcluster_choice)]]
      }
                      
      clusters = meta$cluster.sub[meta$cluster == input$subcluster_choice]
      
      variable = meta[meta$cluster == input$subcluster_choice, input$subcluster_colouring]
      
      pdf = data.frame(x = coords[,1], y = coords[,2], col = variable)
      rand = sample(nrow(pdf), nrow(pdf))
      pdf = pdf[rand,]
      
      p = ggplot(pdf, aes(x = x, y= y, col = factor(col))) +
        geom_point(size = 1) +
        scale_colour_Publication(name = "Sub-cluster", drop = FALSE) +
        ggtitle(paste("Cluster", input$subcluster_choice)) +
        guides(colour = guide_legend(override.aes = list(size=10, 
                                                         alpha = 1))) +
        theme_bw()   
      
      
      
      if(input$subcluster_colouring %in% c("stage", "theiler"))
        p = p + scale_color_manual(values = c(brewer_pal(palette = "Spectral")(length(unique(variable))-1), "darkgrey"), name = "")
      #TODO: doesn't handle absence of mixed_gastrulation well
      
      return(p)
    })
    
    output$subcluster_contribution = renderPlot({
      sub_meta = meta[meta$cluster == input$subcluster_choice,]
      tab = table(sub_meta$cluster.sub, sub_meta$stage)
      fractions = sweep(tab, 1, rowSums(tab), "/")
      frac_nomixed = fractions[,colnames(fractions)!="mixed_gastrulation"]
      means = apply(frac_nomixed, 1, function(x) sum(x * 1:length(x)))
      
      melt = melt(fractions)
      
      palette = c(brewer_pal(palette = "Spectral")(length(unique(meta$stage))-1), "darkgrey")
      names(palette) = unique(meta$stage)[order(unique(meta$stage))]
      
      plot = ggplot(melt, aes(x = factor(Var1, levels = names(means)[order(means)]), y = value, fill = Var2)) +
        geom_bar(stat = "identity") +
        labs(x = "Cluster", y = "Fraction of cells") +
        theme_bw() +
        scale_fill_manual(values = palette, name = "")      
      return(plot)
    })
    
    output$subcluster_genes = renderPlot({
      if(input$coord == "tsne"){
        coords = tsnes_sub[[as.character(input$subcluster_choice)]]
      } else {
        coords = layouts_sub[[as.character(input$subcluster_choice)]]
      }
      
      expr = as.vector(link[,match(as.character(input$subcluster_gene), as.character(genes[,2]))])
      expr = expr[meta$cluster == input$subcluster_choice]

      pdf = data.frame(x = coords[,1], y = coords[,2], col = expr)
      rand = sample(nrow(pdf), nrow(pdf))
      pdf = pdf[rand,]
      
      p = ggplot(pdf, aes(x = x, y= y, col = col)) +
        geom_point(size = 1) +
        scale_color_gradient2(name = "Log2\ncounts", mid = "cornflowerblue", low = "gray75", high = "black", midpoint = max(expr)/2) +
        ggtitle(paste("Cluster", input$subcluster_choice)) +
        theme_bw()   
      
      return(p)
    })
    
    output$subcluster_violin = renderPlot({
      expr = as.vector(link[,match(as.character(input$subcluster_gene), as.character(genes[,2]))])
      expr = expr[meta$cluster == input$subcluster_choice]
      
      pdf = data.frame(expr = expr, clust = meta$cluster.sub[meta$cluster == input$subcluster_choice])
      
      p = ggplot(pdf, aes(x = clust, y= expr, fill = factor(clust))) +
        geom_violin(scale = "width") +
        ggtitle(paste("Cluster", input$subcluster_choice)) +
        theme_bw() +
        scale_fill_Publication()
      
      return(p)
    })
    
    output$subcluster_markers = renderTable({
      clust = input$subcluster_choice
      subclust = input$subcluster_marker_choice
      
      tab = markers_sub[[as.character(clust)]][[as.character(subclust)]]
      tab = tab[order(tab$IUT.p),]
      genes_mark = genes[match(rownames(tab), genes[,1]), 2]
      return(data.frame(genes = genes_mark, p.unadj = tab[,1])[1:input$n.genes,])
    })
    
  }
)