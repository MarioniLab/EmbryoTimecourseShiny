library(shiny)
library(ggplot2)

genes = readRDS("genes.rds")
meta = readRDS("meta.rds")


fluidPage(
  sidebarLayout(
    sidebarPanel(
      absolutePanel(
        #SIDEBAR INPUTS
        selectInput("stage", "Stage to plot", choices = c(sort(c(as.character(unique(meta$stage)), as.character(unique(meta$theiler)), "all"))), selected = "all"),
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
        tabPanel("Landing page",
                 h5("This is the accompanying server for the paper XXXXXX. There are three tabs, above:"),
                 h5("Dataset overview: this shows the t-SNE from Figure 1, with customisable colouring."),
                 h5("Gene interrogation: this may be used to assay gene expression information from the data. Use the gene dropdown on the left sidebar."),
                 h5("Cell-type markers: This lists the genes that are highly expressed in one cell-type more than any other."),
                 h5("To report any issues please contact jag216 {at} cam.ac.uk.")
        ),
        tabPanel("Dataset overview",
                 plotOutput("data", height = "800px"),
                 plotOutput("stage_contribution")
        ),
        tabPanel("Gene interrogation",
                 plotOutput("data_dummy", height = "500px"), #second binding of data plot needed here
                 plotOutput("gene", height = "500px"),
                 plotOutput("gene_violin")
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
                 )

      )
    )
  )
)
