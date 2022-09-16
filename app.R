#library(semantic.dashboard)
#library(DT)
#By Vicente Andres, Millan Albert, Aguilar Alfaro Oscar
###############################################################################
list.of.packages <- c("shiny", "factoextra","tidyverse","pheatmap","shinythemes", "shinydashboard")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(factoextra)
library(tidyverse)
library(shinythemes) # what package is this? Add to the list above
library(shinydashboard) # what package is this? Add to the list above
library(pheatmap)
###############################################################################

# Define UI for dataset viewer app ----
ui <- fluidPage(style = "background-color:#FDFFFF",
                navbarPage("Welcome", collapsible = TRUE, inverse = TRUE, theme = shinytheme("lumen"),
                ),
                # Sidebar layout with a input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(width = 4, style = "background-color:#FBFAF8",
                               # Input: Selector for choosing dataset ----
                               # Input: Select a file ----
                               fileInput("file1", "Upload Data File (.csv)",
                                         multiple = TRUE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                               checkboxInput("t", "Check box to transpose data", TRUE),
                               
                               fileInput("file2", "Upload Metadata File (.csv)",
                                         multiple = TRUE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                               # checkboxInput("t", "Tranpose .CSV", TRUE)
                  ),
                  # Main panel for displaying outputs ----
                  mainPanel(
                    tabsetPanel(id = "tabs",
                                tabPanel("Data Input",icon=icon("address-card"),theme = shinytheme("journal"),
                                         checkboxInput("select", "Check box to select data", FALSE),
                                         div(style="display:flex",
                                             selectInput("var", "Select FEATURES to visualize", choices = NULL, multiple = TRUE),
                                             selectInput("indiv", "Select SAMPLES to visualize", choices = NULL, multiple = TRUE)),
                                         column(style = "background-color:white",width = 4,tags$h1("Features"),
                                                tags$br(), 
                                                tableOutput("view")),
                                         
                                         column(style = "background-color:white", width = 4,tags$h1("Samples"),
                                                tags$br(), 
                                                tableOutput("view2")),
                                         column(style = "background-color:white", width = 4,tags$h1("Groups"),
                                                tags$br(), 
                                                tableOutput("view3")),
                                ),
                                
                                tabPanel("PCA", icon=icon("chart-pie"),
                                         p("The purpose of the PCA is to make and deploy principal component analysis from data in CSV and TSV formats. We are using .CSV, and by uploading the data with extension .csv It will visualize the selected features and will visualize the selected sample. It will visualize our data by clicking the submit button."),
                                         actionButton("update", "Submit"),
                                         tags$br(),
                                         tags$br(),
                                         column(width = 6, plotOutput(outputId = "distPlot1")),
                                         column(width = 6,plotOutput(outputId = "distPlot4")),
                                         column(width = 6,plotOutput(outputId = "distPlot2")),
                                         column(width = 6,plotOutput(outputId = "distPlot3")),
                                ),
                                tabPanel("Heatmap", icon=icon("table"),
                                         p("The heatmap function is a powerful tool to visualize data. The function visualize graphical representation of data 
                             where the values of the data is represented in colors."),
                                         div(style="display:flex",
                                             checkboxInput("cluster_rows", "Cluster_rows = TRUE", TRUE),
                                             checkboxInput("cluster_cols", "Cluster_cols = TRUE", TRUE),
                                             checkboxInput("Z_norm", "Z_normalize = TRUE", FALSE)),
                                         sliderInput("x", "Height in megapixles", min = 0, max = 100, value = 50), #ADD
                                         sliderInput("y", "Width in megapixles", min = 0, max = 100, value = 50),  #ADD
                                         actionButton("update2", "Submit"),
                                         plotOutput(outputId = "heatplot")
                                ),
                                tabPanel( "Our Team", icon = icon("refresh"),
                                          div(
                                            h1("Andres Vicente"),
                                            p("I am Andres Vicente, I am majoring in Computer Science, I have passion for programming, programming is something that I enjoy doing. My ultimate goal is to have a degree in Computer Science and apply the knowledge I learned."),
                                            h1("Albert Millan"),
                                            p(" Alberto (Albert) Millan obtained his Ph.D. in Quantitative and Systems Biology from the University of California, Merced (UCM) in the year 2020. There he studied the role of Natural Killer (NK) cells in mice. He has experience in bioinformatics and computational modeling using R and Python. Currently, he continues to study NK cells as a Postdoc in the lab of Prof. Lewis Lanier at UC San Francisco (UCSF).  "),
                                            h1("Oscar Aguilar Alfaro"),
                                            p("Oscar is a post-doctoral fellow working with Prof. Lewis Lanier at the University of California - San Francisco. He is interested in understanding how natural killer cells recognize cancerous and virally-infected cells using the multitude of receptors they express."),
                                          )),
                                tabPanel( "Source Code", icon = icon("github")),
                    ),
                  )
                )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  df <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, row.names = 1, header = TRUE)
    if (input$t == TRUE) {
      df <- na.omit(t(df))
      df <- df[ , which(apply(df, 2, var) != 0)]
    } else {
      df <- na.omit(df)
      df <- df[ , which(apply(df, 2, var) != 0)]
    }
  })
  
  observe({
    updateSelectInput(inputId = "indiv", choices = rownames(df()))
    updateSelectInput(inputId = "var", choices = colnames(df()))
  })
  
  datasetInput <- reactive({
    if (input$select == TRUE) {
      df <- df()[input$indiv,input$var]
      df <- df[ , which(apply(df, 2, var) != 0)] ## ADD this
    } else {
      df <- df()[ , which(apply(df(), 2, var) != 0)] ## ADD this
    }
  })
  
  metadata <- reactive({
    req(input$file2)
    metadata <- read.csv(input$file2$datapath, row.names = 1, header = TRUE)
    metadata
  })
  
  metaData <- reactive({
    if (input$select == TRUE) {
      metadata <- merge(datasetInput(), metadata(), by = "row.names", sort=FALSE)[,"Group"]
      metadata
    } else {
      metadata()[,1]
    }
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable(colnames = FALSE,{
    head(colnames(datasetInput()))
  })
  output$view2 <- renderTable(colnames =FALSE,{
    head(rownames(datasetInput()))
  })
  output$view3 <- renderTable(colnames =FALSE,{
    head(metaData())
  })
  ###############################################################################
  # PCA function
  ############################################################################### 
  formData <- eventReactive(input$update, {
    res.pca <- prcomp(datasetInput(), scale = TRUE)
    res.pca
  })
  output$distPlot1 <- renderPlot({
    fviz_eig(formData(), main = "Eigenvalues against the number of dimensions")
  })
  output$distPlot2 <- renderPlot({
    fviz_pca_var(formData(),
                 col.var = "contrib", # Color by contributions to the PC
                 select.var = list(name = input$var),
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE     # Avoid text overlapping
    )
  })
  output$distPlot3 <- renderPlot({
    fviz_pca_biplot(formData(), repel = TRUE,
                    select.var = list(name = input$var),
                    col.var = "#2E9FDF", # Variables color
                    col.ind = "#696969"  # Individuals color
    )
  })
  output$distPlot4 <- renderPlot({
    fviz_pca_ind(formData(),
                 habillage = metaData(),
                 addEllipses = TRUE,
                 ellipse.type = "confidence",
                 legend.title = "Groups",
                 repel = TRUE     # Avoid text overlapping
    )
  })
  ###############################################################################
  # HEATMAP function
  ###############################################################################
  heatplot_data <- eventReactive(input$update2, {
    heatplot_data <- datasetInput()
    if (input$Z_norm == TRUE) {
      cal_z_score <- function(x){
        (x - mean(x)) / sd(x) }
      heatplot_data <- apply(heatplot_data, 2, cal_z_score)
      heatplot_data
    } else {
      heatplot_data
    }
  })
  
  # update this part
  output$heatplot <- renderPlot(width = 1000, height = 1000,{
    if (input$Z_norm == TRUE) {
      pheatmap(t(heatplot_data()), cellheight = input$x , cellwidth = input$y,
               cluster_rows = input$cluster_rows,
               cluster_cols = input$cluster_cols,
               border_color = "black",
               main = "Heatplot normalized using Z scores")
    } else {
      pheatmap(t(heatplot_data()), cellheight = input$x , cellwidth = input$y, 
               cluster_rows = input$cluster_rows,
               cluster_cols = input$cluster_cols,
               border_color = "black",
               main = "Heatplot of raw values")
    }
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)




















