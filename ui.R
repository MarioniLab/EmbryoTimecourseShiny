library(shiny)
library(ggplot2)
library(DT)

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

genes = readRDS("genes.rds")
meta = readRDS("meta.rds")

big_plot_width = "900px"
big_plot_height = "500px"

narrower_plot_width = "650px"

half_plot_width = "450px"
narrower_half_plot_width = "350px"
half_plot_height = "300px"

fluidPage(
  sidebarLayout(
    sidebarPanel(
      absolutePanel(
        h3("Plot options"),
        #SIDEBAR INPUTS
        selectInput("coord_type", "Projection type", choices = c("UMAP" = "umap",
                                                                 "t-SNE" = "tsne")),
        selectInput("stage", "Cell subset", choices = c(sort(c(as.character(unique(meta$stage)), as.character(unique(meta$theiler)), "all"))), selected = "all"),
        selectInput("colourby", "Plot colour", choices = c("Cell type" = "celltype",
                                                           "Top level cluster" = "cluster",
                                                           "Timepoint" = "stage",
                                                           "Theiler stage" = "theiler",
                                                           "Sample" = "sample",
                                                           "Timepoint cluster" = "cluster.stage",
                                                           "Theiler cluster" = "cluster.theiler"
                                                           ),
                    selected = "cluster.ann"),
        checkboxInput("numbers", "Annotate clusters in plot"),
        selectizeInput("gene", "Gene", choices = NULL, selected = 26600),
        checkboxInput("subset", "Subset cells"),
        selectInput("subset_degree", label = "Subsetting severity", choices = c("Low" = 100, "High" = 50)),
        downloadButton("downloadOverview", label = "Overview vis"),
        downloadButton("downloadGeneTSNE", label = "Gene expr. vis"),
        downloadButton("downloadGeneViolin", label = "Gene expr. violins"),
        fixed = TRUE,
        left = "2%",
        top = "2%",
        width = paste0(20*2/3, "%")
      ),
      width = 2
    ),
    mainPanel(
      width = 10,
      titlePanel("A single-cell resolution molecular roadmap from mouse gastrulation to early organogenesis."),
      
      #put plots here
      tabsetPanel(
        tabPanel("Landing page",
                 br(),
                 HTML(paste(shiny::tags$b("This is the accompanying interactive server for the paper"),
                            em("A single-cell resolution molecular roadmap from mouse gastrulation to early organogenesis."))),
                 br(),
                 HTML("Please note: when you reach this page, the server needs to load some data for your session. We have noticed this is particularly slow on Google Chrome, so please be patient!"),
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
                       "Interactive versions of plots from Figure 2 of the manuscript are shown - these will change depending on your gene selection.")),
                 br(),
                 HTML(paste(shiny::tags$b("Haematoendothelium:"),
                       "Customisable versions of the force-directed graph from Figure 3 of the manuscript are shown - these will change depending on your gene selection.")),
                 h4("Options:"),
                 HTML(paste(shiny::tags$b("Cell subset:"),
                            "Select the timepoints you want to plot.")),
                 br(),
                 HTML(paste(shiny::tags$b("Plot colour:"),
                            "Select the variable you would like to colour the overview plots by. The options",
                            em("Timepoint cluster"),
                            "and",
                            em("Theiler cluster"),
                            "are only appropriate when visualising a single timepoint or Theiler stage respectively.")),
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
                     selectInput("celltype", "Cell type", names(celltype_colours)),
                     plotOutput("celltype_gene_plot")
                     # numericInput("n.genes", "Number of DE genes", value = 20)
                   ),
                   mainPanel(
                     plotOutput("celltype_presence_plot", height = half_plot_height, width = half_plot_height),
                     dataTableOutput("celltype_markers"),
                     h4(paste0("This is the output of the scran function findMarkers. ",
                               "These are top-ranked genes that are expressed in the selected celltype more highly than in *any* other celltype."))
                   )
                 )
                 )#,
        # tabPanel("Endoderm",
        #          h3("These plots are interactive versions of visualisations that were present in the paper."),
        #          h4("Principal components for all considered endoderm cells (i.e. embryonic + visceral) are shown."),
        #          fluidRow(
        #            splitLayout(cellWidths = c("50%", "50%"),
        #                        plotOutput("endo_pc1", width = half_plot_width, height = half_plot_height),
        #                        plotOutput("endo_pc3", width = half_plot_width, height = half_plot_height))
        #          ),
        #          h4("Diffusion components for E8.0-E8.5 endoderm cells are shown."),
        #          fluidRow(
        #            splitLayout(cellWidths = c("50%", "50%"),
        #                        plotOutput("endo_late_ref", width = half_plot_width, height = half_plot_height),
        #                        plotOutput("endo_late_gene", width = half_plot_width, height = half_plot_height))
        #          ),
        #          h4("The axis of the embryonic gut is shown."),
        #          fluidRow(
        #            splitLayout(cellWidths = c("50%", "50%"),
        #                        plotOutput("endo_gut_axis", width = half_plot_width, height = half_plot_height),
        #                        plotOutput("endo_gut_gene", width = half_plot_width, height = half_plot_height))
        #          ),
        #          h4("The pseudotime trajectory for Visceral Endoderm to Hindgut cells is shown."),
        #          plotOutput("endo_traj_gene", width = half_plot_width, height = half_plot_height)
        #          ),
        # tabPanel("Haematoendothelium",
        #          fixedRow(
        #            splitLayout(cellWidths = c("50%", "50%"),
        #                        plotOutput("haem_clusters", width = half_plot_width, height = half_plot_height),
        #                        plotOutput("haem_gene", width = narrower_half_plot_width, height = half_plot_height))
        #          ),
        #          # plotOutput("haem_clusters", width = big_plot_width, height = big_plot_height),
        #          # plotOutput("haem_gene", width = narrower_plot_width, height = big_plot_height),
        #          fixedRow(
        #            splitLayout(cellWidths = c("50%", "50%"),
        #                        plotOutput("haem_clusters_zoomed", width = half_plot_width, height = half_plot_height),
        #                        plotOutput("haem_gene_zoomed", width = narrower_half_plot_width, height = half_plot_height))
        #          )
        #          )

      )
    )
  )
)
