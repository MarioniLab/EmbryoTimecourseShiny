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
        selectInput("colourby", "Colour plot", choices = c("Cell type" = "cluster.ann",
                                                           "Top level cluster" = "cluster",
                                                           "Timepoint" = "stage",
                                                           "Theiler stage" = "theiler",
                                                           "Sample" = "sample",
                                                           "Stage (Ex.x) cluster" = "cluster.stage",
                                                           "Theiler cluster" = "cluster.theiler"
                                                           ),
                    selected = "cluster.ann"),
        checkboxInput("numbers", "Annotate clusters in plot"),

        selectizeInput("gene", "Gene", choices = genes[,2], selected = "Hbb-bh1"),

        numericInput("n.genes", "Number of DE genes", value = 20),
        checkboxInput("subset", "Subset cells (speed benefit)"),
        selectInput("subset_degree", label = "Subsetting severity", choices = c("Low" = 200, "High" = 100)),
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
        
        tabPanel("Cell-type markers",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("celltype", "Cell type", unique(meta$cluster.ann))
                   ),
                   mainPanel(
                     plotOutput("celltype_presence_plot", height = "300px", width = "300px"),
                     tableOutput("celltype_markers")
                   )
                 )
                 ),
        
        tabPanel("All-data subclusters",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("subcluster_choice", "All-data cluster", choices = unique(meta$cluster)[order(unique(meta$cluster))], selected = "1"),
                     selectInput("subcluster_colouring", "Plot colouring", choices = c("Timepoint" = "stage",
                                                                                       "Theiler" = "theiler",
                                                                                       "Sub-cluster" = "cluster.sub"),
                                 selected = "cluster.sub"),
                     selectInput("subcluster_gene", "Selected gene", choices = genes[,2], selected = "Hbb-bh1"),
                     #specifies the clusters that are allowed
                     uiOutput("subcluster_marker_choice")
                   ),
                   mainPanel(
                     tabsetPanel(
                     tabPanel("Cluster overview",
                              plotOutput("subcluster_plot"),
                              plotOutput("subcluster_contribution")),
                     tabPanel("Gene interrogation",
                              plotOutput("subcluster_plot_dummy"),
                              plotOutput("subcluster_genes"),
                              plotOutput("subcluster_violin")),
                     tabPanel("Markers",
                              tableOutput("subcluster_markers"))
                     )
                   )
                 )
                 )

      )
    )
  )
)
