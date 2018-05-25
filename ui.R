library(shiny)
library(ggplot2)

genes = readRDS("genes.rds")
meta = readRDS("meta.rds")

big_plot_width = "900px"
big_plot_height = "500px"

narrower_plot_width = "650px"

half_plot_width = "450px"
half_plot_height = "300px"

fluidPage(
  sidebarLayout(
    sidebarPanel(
      absolutePanel(
        #SIDEBAR INPUTS
        selectInput("stage", "Subset to plot", choices = c(sort(c(as.character(unique(meta$stage)), as.character(unique(meta$theiler)), "all"))), selected = "all"),
        selectInput("colourby", "Plot colours", choices = c("Cell type" = "cluster.ann",
                                                           "Top level cluster" = "cluster",
                                                           "Timepoint" = "stage",
                                                           "Theiler stage" = "theiler",
                                                           "Sample" = "sample",
                                                           "Timepoint cluster" = "cluster.stage",
                                                           "Theiler cluster" = "cluster.theiler"
                                                           ),
                    selected = "cluster.ann"),
        checkboxInput("numbers", "Annotate clusters in plot"),

        selectizeInput("gene", "Gene", choices = genes[,2], selected = "Ttr"),

        numericInput("n.genes", "Number of DE genes", value = 20),
        checkboxInput("subset", "Subset cells (speed benefit)"),
        selectInput("subset_degree", label = "Subsetting severity", choices = c("Low" = 200, "High" = 100)),
        fixed = TRUE,
        left = "2%",
        top = "2%",
        width = paste0(20*2/3, "%")
      ),
      width = 2
    ),
    mainPanel(
      #put plots here
      tabsetPanel(
        tabPanel("Landing page",
                 h5("This is the accompanying server for the paper XXXXXX."),
                 h3("Tabs:"),
                 h5("Dataset overview: this shows the t-SNE from Figure 1, with customisable colouring."),
                 h5("Gene interrogation: this may be used to assay gene expression information from the data."),
                 h5("Cell-type markers: This lists the genes that are more highly expressed in one cell-type more than any other."),
                 h5("Endoderm Analysis: This shows plots from the endoderm analysis (Fig 2)"),
                 h3("Options:"),
                 h5("Subset to plot: Choose the set of cells you would like to visualise."),
                 h5("Plot colours: Choose the method to colour the overview t-SNEs. Most options are straightforward; we have also included clusters calculated per timepoint or per Theiler stage if one timepoint only is of interest. Please note that these will not make sense if you visualise more than one timepoint or theiler stage."),
                 h5("Annotate clusters: This will annotate clusters on the plot for easier comparison to violin plots."),
                 h5("Gene: Select a gene for expression analyses"),
                 h5("Number of DE genes: Controls the size of the table for the Marker tab"),
                 h5("Subset + Subsetting severity: These options may enhance the speed of plotting, as cells are excluded in a density-dependent manner."),
                 h3("Other:"),
                 h5("To report any issues please contact Jonny at jag216 {at} cam.ac.uk, or John at marioni {at} ebi.ac.uk.")
        ),
        tabPanel("Dataset overview",
                 plotOutput("data", width = big_plot_width, height = big_plot_height),
                 plotOutput("stage_contribution", width = big_plot_width)
        ),
        tabPanel("Gene interrogation",
                 plotOutput("data_dummy", width = big_plot_width, height = big_plot_height), #second binding of data plot needed here
                 plotOutput("gene", width = narrower_plot_width, height = big_plot_height),
                 plotOutput("gene_violin", width = big_plot_width)
        ),
        tabPanel("Cell-type markers",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("celltype", "Cell type", unique(meta$cluster.ann))
                   ),
                   mainPanel(
                     plotOutput("celltype_presence_plot", height = half_plot_height, width = half_plot_height),
                     tableOutput("celltype_markers")
                   )
                 )
                 ),
        tabPanel("Endoderm analysis",
                 h3("These plots are interactive versions of visualisations that were present in the paper."),
                 h4("Principal components for all considered endoderm cells (i.e. embryonic + visceral) are shown."),
                 fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"), 
                               plotOutput("endo_pc1", width = half_plot_width, height = half_plot_height), 
                               plotOutput("endo_pc3", width = half_plot_width, height = half_plot_height))
                 ),
                 h4("Diffusion components for E8.0-E8.5 endoderm cells are shown."),
                 fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"), 
                               plotOutput("endo_late_ref", width = half_plot_width, height = half_plot_height), 
                               plotOutput("endo_late_gene", width = half_plot_width, height = half_plot_height))
                 ),
                 h4("The axis of the embryonic gut is shown."),
                 fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"), 
                               plotOutput("endo_gut_axis", width = half_plot_width, height = half_plot_height), 
                               plotOutput("endo_gut_gene", width = half_plot_width, height = half_plot_height))
                 ),
                 h4("The pseudotime trajectory for Visceral Endoderm to Hindgut cells is shown."),
                 plotOutput("endo_traj_gene", width = half_plot_width, height = half_plot_height)
                 ),
        tabPanel("Haematoendothelium",
                 plotOutput("haem_clusters", width = big_plot_width, height = big_plot_height),
                 plotOutput("haem_gene", width = big_plot_width, height = big_plot_height))

      )
    )
  )
)
