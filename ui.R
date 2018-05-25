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
        h3("Plot options"),
        #SIDEBAR INPUTS
        selectInput("stage", "Cell subset", choices = c(sort(c(as.character(unique(meta$stage)), as.character(unique(meta$theiler)), "all"))), selected = "all"),
        selectInput("colourby", "Plot colour", choices = c("Cell type" = "cluster.ann",
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
        checkboxInput("subset", "Subset cells"),
        selectInput("subset_degree", label = "Subsetting severity", choices = c("Low" = 200, "High" = 100)),
        fixed = TRUE,
        left = "2%",
        top = "2%",
        width = paste0(20*2/3, "%")
      ),
      width = 2
    ),
    mainPanel(
      titlePanel("A single-cell resolution molecular roadmap from mouse gastrulation to early organogenesis."),
      
      #put plots here
      tabsetPanel(
        tabPanel("Landing page",
                 br(),
                 HTML(paste(shiny::tags$b("This is the accompanying interactive server for the paper"),
                            em("A single-cell resolution molecular roadmap from mouse gastrulation to early organogenesis."))),
                 h4("Tabs:"),
                 HTML(paste(shiny::tags$b("Dataset overview:"),
                       "t-SNE plots of the data are shown with customisable colouring, with a visualisation of how cells from different timepoints contribute to different cell populations.")),
                 br(),
                 HTML(paste(shiny::tags$b("Gene interrogation:"),
                       "The patterns of expression of genes across the dataset may be viewed here.")),
                 br(),
                 HTML(paste(shiny::tags$b("Cell-type markers:"),
                       "Genes highly expressed in a single celltype are shown here.")),
                 br(),
                 HTML(paste(shiny::tags$b("Endoderm:"),
                       "Interactive versions of plots from Figure 2 of the manuscript are shown.")),
                 br(),
                 HTML(paste(shiny::tags$b("Haematoendothelium:"),
                       "Interactive versions of the force-directed graph from Figure 3 of the manuscript are shown.")),
                 h4("Options:"),
                 HTML(paste(shiny::tags$b("Cell subset:"),
                            "Select the timepoints you want to plot.")),
                 br(),
                 HTML(paste(shiny::tags$b("Plot colour:"),
                            "Select the variable you would like to colour the overview plots by.")),
                 br(),
                 HTML(paste(shiny::tags$b("Annotate clusters:"),
                            "Text will be drawn on the plot to help locate specific cell types.")),
                 br(),
                 HTML(paste(shiny::tags$b("Gene:"),
                            "Select the gene (MGI) to use for expression plots.")),
                 br(),
                 # HTML(paste(shiny::tags$b("Number of DE genes:"),
                 #            "Choose the size of the table in the",
                 #            em("Cell-type markers"),
                 #            "tab.")),
                 # br(),
                 HTML(paste(shiny::tags$b("Subset cells & severity:"),
                            "To speed up plotting, this reduces the number of cells plotted in a density dependent manner.")),
                 h4("Other notes:"),
                 HTML(paste("To report any issues please contact Jonny Griffiths at",
                            em("jag216 {at} cam.ac.uk,"),
                            "or John Marioni at", 
                            em("marioni {at} ebi.ac.uk."))),
                 br(),
                 HTML("It takes a few seconds to make the first plots when all cells are considered - this should speed up when parameters are changed thereafter.")
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
                     selectInput("celltype", "Cell type", unique(meta$cluster.ann)),
                     numericInput("n.genes", "Number of DE genes", value = 20)
                   ),
                   mainPanel(
                     plotOutput("celltype_presence_plot", height = half_plot_height, width = half_plot_height),
                     tableOutput("celltype_markers")
                   )
                 )
                 ),
        tabPanel("Endoderm",
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
