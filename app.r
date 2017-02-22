source("global.R")
options(shiny.maxRequestSize=30*1024^2) 

ui <- shinyUI(navbarPage("Cap App", theme = shinytheme("lumen"),
              windowTitle = "Cap App",
              tabPanel("Load Data",
                sidebarLayout(
                  sidebarPanel(
                    fileInput("file",'Input file 1',accept=c('.csv', '.shp','.dbf','.sbn','.sbx','.shx',".prj",'.geojson'), multiple=TRUE),
                    tags$hr(),
                    uiOutput("fi")                    
                    ),
              mainPanel(
                uiOutput("tb")
                
                # use below code if you want the tabset programming in the main panel. If so, then tabset will appear when the app loads for the first time.
                #       tabsetPanel(tabPanel("Summary", verbatimTextOutput("sum")),
                #                   tabPanel("Data", tableOutput("table")))
              )
                    
                  )
)
)
)

server <- shinyServer(function(input,output, session){
  dsnames <- c()
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    myshape<- input$file
    if (is.null(myshape)) 
      return(NULL)       
    
    dir<-dirname(myshape[1,4])
    
    for ( i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    shape<-st_read(getshp)

  })
  
  data2 <- reactive({
    myshape<- input$file2
    if (is.null(myshape)) 
      return(NULL)       
    
    dir<-dirname(myshape[1,4])
    
    for ( i in 1:nrow(myshape)) {
      file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
    
    getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
    shape<-st_read(getshp)
    
  })
  

  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderPlot({
    if(is.null(data())){return ()}
    plot(data())
  })
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    plot(data())
    
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  # This reactive output contains the dataset and display the dataset in table format
  output$map <- renderLeaflet({
    leaflet(data()) %>% addTiles() %>% 
      addMarkers()
  })
  
  output$selection <- renderPrint(
    input$mychooser)
  
  observeEvent(input$button, {
    toggle("slider1")
  })

  output$fi <- renderUI({
    if(is.null(data()))
      return()
    else
      fileInput("file2",'Input file 2',accept=c('.csv', '.shp','.dbf','.sbn','.sbx','.shx',".prj",'.geojson'), multiple=TRUE)
    
  })
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Powered by", tags$img(src='RStudio-Ball.png', heigth=200, width=200))
    else
      tabsetPanel(tabPanel("About file", plotOutput("filedf")),
                  tabPanel("Data", tableOutput("contents")),
                  tabPanel("Summary", tableOutput("sum")),
                  tabPanel("Attribute", verbatimTextOutput("selection"),  
                           chooserInput("mychooser", "Available frobs", "Selected frobs",
                                        names(data()), names(data2()), size = 10, multiple = TRUE)),
                  tabPanel("Map", leafletOutput("map"),
                           useShinyjs(),
                           h3("Toggle Transparency"),
                           actionButton("button", "Toggle slider"),
                           sliderInput("slider1", "Slider 1", 0, 10000, 1000)))
  })
})

shinyApp(server = server, ui = ui)