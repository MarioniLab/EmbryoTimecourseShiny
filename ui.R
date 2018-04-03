library(shiny)
library(ggplot2)

genes = readRDS("genes.rds")
meta = readRDS("meta.rds")


fluidPage(
  sidebarLayout(
    sidebarPanel(
      absolutePanel(
        #SIDEBAR INPUTS
        textInput("password", "Password", value = "replace me"),
        selectInput("stage", "Stage to plot", choices = c(sort(c(as.character(unique(meta$stage)), as.character(unique(meta$theiler)), "all"))), selected = "all"),
        selectInput("coord", "Visualisation style", choices = c("t-SNE" = "tsne", 
                                                                "Force-directed graph" = "graph")),
        checkboxInput("embryonic", "Embryonic cells only?"),
        selectInput("colourby", "Colour plot", choices = c("Sample" = "sample",
                                                           "All-data cluster" = "cluster",
                                                           "Stage (Ex.x) cluster" = "cluster.stage",
                                                           "Theiler cluster" = "cluster.theiler",
                                                           "Predicted cell type" = "celltype",
                                                           "Timepoint" = "stage",
                                                           "Theiler stage" = "theiler"),
                    selected = "cluster"),
        checkboxInput("annot", "Use annotation? (with all-data clusters only!)"),
        checkboxInput("numbers", "Number clusters in plot?"),
        selectizeInput("gene", "Gene", choices = genes[,2], selected = "Hbb-bh1"),

        numericInput("n.genes", "Number of DE genes", value = 20),
        fixed = TRUE,
        left = "2%",
        top = "2%",
        width = "20%"
      ),
      width = 3
    ),
    mainPanel(
      #put plots here
      tabsetPanel(
        tabPanel("Dataset overview",
                 plotOutput("data", height = "800px"),
                 plotOutput("stage_contribution")
        ),
        tabPanel("Gene interrogation",
                 plotOutput("data_dummy", height = "500px"), #second binding of data plot needed here
                 plotOutput("gene", height = "500px"),
                 plotOutput("gene_violin")
        ),
        tabPanel("Cluster Markers",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("marker_cluster_type", "Cluster type", choices = c("Theiler stage" = "theiler",
                                                                                    "Timepoint" = "timepoint",
                                                                                    "All-data" = "all"), selected = "all"),
                     #specifies the subset e.g. TS10
                     uiOutput("subset_marker"),
                     #specifies the cluster itself
                     uiOutput("cluster_marker")
                   ),
                   mainPanel(
                     tableOutput("markers")
                   )
                 )
                 ),
        
        tabPanel("All-data subclusters",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("subcluster_choice", "All-data cluster", choices = unique(meta$cluster), selected = "1"),
                     #specifies the clusters that are allowed
                     uiOutput("subcluster_marker_choice")
                   ),
                   mainPanel(
                     plotOutput("subcluster_plot"),
                     plotOutput("subcluster_genes"),
                     tableOutput("subcluster_markers")
                   )
                 )
                 ),
        
        tabPanel("Library sizes",
                 plotOutput("libs", height = "800px")
                 )

      )
    )
  )
)
