library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(shinyjs)
library(shinythemes)

source("chooser.R")

ui <- shinyUI(fluidPage(theme = shinytheme("lumen"),
  titlePanel("File Input"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file",'Input shapefile',accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj",'.geojson'), multiple=TRUE),
      helpText("Default max. file size is 5MB"),
      tags$hr(),
      h5(helpText("Select the read.table parameters below")),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
      br(),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
    ),
    mainPanel(
      uiOutput("tb")
      
      # use below code if you want the tabset programming in the main panel. If so, then tabset will appear when the app loads for the first time.
      #       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
      #                   tabPanel("Data", tableOutput("table")))
    )
    
  )
))

server <- shinyServer(function(input,output, session){
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    readLines(file1$datapath, warn = FALSE) %>%
      paste(collapse = "\n")
    
  })
  
  data1 <- reactive({
    data1 <- as.data.frame(data1)
    data1 <- list()
    if(is.null(file1)){return()} 
    for (i in 1:173){
      data1[[i]] <- data$features[[i]]$properties$admin
    }
  })
print(data1)
  datageom <- reactive({
    do.call(rbind,lapply(data$features, function(x) data.frame(x$geometry)))
  })

  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data)
    
  })
  
  # This reactive output contains the dataset and displays a interactive Table
  output$table <- renderDataTable({
    if(is.null(data())){return ()}
    data()
  })
  # This reactive output contains the dataset and display the dataset in table format
  output$map <- renderLeaflet({
    
    leaflet()  %>% addTiles() %>% 
      addGeoJSON(data())
  })
  
  output$selection <- renderPrint(
    input$mychooser)
  
  observeEvent(input$button, {
    toggle("slider1")
    
  })
  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Powered by", tags$img(src='RStudio-Ball.png', heigth=200, width=200))
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),
                  tabPanel("Data", dataTableOutput("table")),
                  tabPanel("Summary", tableOutput("sum")),
                  tabPanel("Attribute", verbatimTextOutput("selection"),  
                      chooserInput("mychooser", "Available frobs", "Selected frobs",
                      row.names(data1), c(), size = 10, multiple = TRUE)),
                  tabPanel("Map", leafletOutput("map"),
                           useShinyjs(),
                           h3("Toggle Transparency"),
                           actionButton("button", "Toggle slider"),
                           sliderInput("slider1", "Slider 1", 0, 10000, 1000)))
  })
})

shinyApp(server = server, ui = ui)