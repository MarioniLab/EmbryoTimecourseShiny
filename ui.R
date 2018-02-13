library(shiny)
library(ggplot2)

# load("ui.RData")
# genes = readRDS("~/Desktop/test_shiny_chim/genes.rds")
# meta = readRDS("~/Desktop/test_shiny_chim/meta.rds")
genes = readRDS("genes.rds")
meta = readRDS("meta.rds")
#genes is a 2 column data frame, column 1 Ensembl, column 2 MGIs
#metadata you should look at yourself


fluidPage(
  sidebarLayout(
    sidebarPanel(
      absolutePanel(
        #put inputs here
        textInput("password", "Password", value = "replace me"),
        selectInput("stage", "Stage", choices = c(sort(c(as.character(unique(meta$stage)), as.character(unique(meta$theiler)), "all"))), selected = "all"),
        selectInput("coord", "Visualisation style", choices = c("t-SNE" = "tsne", 
                                                                "Force-directed graph" = "graph")),
        checkboxInput("embryonic", "Embryonic cells only? (don't tick me for now)"),
        selectInput("colourby", "Colour plot", choices = c("Sample" = "sample",
                                                           "All-data cluster" = "cluster",
                                                           "Stage (Ex.x) cluster" = "cluster.stage",
                                                           "Theiler cluster" = "cluster.theiler",
                                                           "Predicted cell type" = "celltype",
                                                           "Timepoint" = "stage",
                                                           "Theiler stage" = "theiler"),
                    selected = "cluster"),
        checkboxInput("annot", "Use fancy colours (with all-data clusters only!)"),
        checkboxInput("numbers", "Number clusters?"),
        selectizeInput("gene", "Gene", choices = genes[,2], selected = "Hbb-bh1"),
        # selectInput("de.stage", "DE stage", choices = unique(meta$stage), selected = "E7.5"),
        # selectInput("marker.type", "DE/marker clustering", choices = c("Theiler" = "theiler",
        #                                                                "All-data" = "all",
        #                                                                "Stage (EX.X)" = "stage")),
        h6("Use Colour plot to select cluster types, use Stage to narrow down stages"),
        uiOutput("cluster1select"),
        # uiOutput("cluster2select"),
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
        # tabPanel("Inter-cluster DE",
        #          h6("FC is cluster 2 w.r.t. cluster 1"),
        #          tableOutput("intercluster")),
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
                 
        tabPanel("Doublets",
                 plotOutput("doublets", height = "800px")
                 ),
        tabPanel("Library sizes",
                 plotOutput("libs", height = "800px")
        )

      )
    )
  )
)
