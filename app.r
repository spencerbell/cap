source("global.R")
options(shiny.maxRequestSize=30*1024^2) 

ui <- shinyUI(navbarPage("Application to Explore Spatial Regression Techniques", theme = shinytheme("lumen"),
              windowTitle = "Cap App",
              tabPanel("Load Data",
                sidebarLayout(
                  sidebarPanel(
                    fileInput("file",'Input file 1',accept=c('.csv', '.shp','.dbf','.sbn','.sbx','.shx',".prj",'.geojson'), multiple=TRUE),
                    tags$hr(),
                    fileInput("file2",'Input file 2',accept=c('.csv', '.shp','.dbf','.sbn','.sbx','.shx',".prj",'.geojson'), multiple=TRUE)
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
  output$sum <- renderPrint({
    if(is.null(data())){return ()}
    summary(data())
    
  })
  
  output$reg <- renderPrint({
    if(is.null(data())){return ()}
    data.ols<-lm(kioskId~bikesAvail+docksAvail, data=data())
    sjt.lm(data.ols)
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderDataTable({
    if(is.null(data())){return ()}
    as.data.frame(data())
  })
  # This reactive output contains the dataset and display the dataset in table format
  output$map <- renderLeaflet({
    icons <- awesomeIcons(
      icon = 'ion-android-bicycle',
      library = 'ion'
    )
    icons2 <- awesomeIcons(
      icon = 'ion-android-car',
      markerColor = 'red',
      library = 'ion'
    )
    html_legend <- "<img src='http://leafletjs.com/docs/images/leaf-green.png'>green<br/>
<img src='http://leafletjs.com/docs/images/leaf-red.png'>red"
    
    if(!is.null(data())&!is.null(data2())){return (
      leaflet() %>% addTiles() %>% 
        addAwesomeMarkers(data = data(),
                          icon = icons,
                          popup = data()$addressStr,
                          group ="data 1") %>% 
        addAwesomeMarkers(data = data2(), 
                          icon = icons2,
                          popup = data2()$LOCATION,
                          group = "data 2") %>% 
        addControl(html = html_legend, position = "bottomleft")
    )}
    if(!is.null(data2())){return (
      leaflet() %>% addTiles() %>% 
        addAwesomeMarkers(data = data2(), 
                          icon = icons2, 
                 popup = data2()$LOCATION,
                 group = "Data 2")
    )}
    if(!is.null(data())){return (
    leaflet() %>% addTiles() %>% 
      addAwesomeMarkers(data = data(),
                 icon = icons,
                 popup = data()$addressStr,
                 group = "Data 1")
    )}

  })
  
  output$selection <- renderPrint(
    input$mychooser)
  
  observeEvent(input$button, {
    toggle("slider1")
    
  })
  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Powered by", tags$img(src='psu.png', heigth=200, width=200))
    else
      tabsetPanel(tabPanel("About file", plotOutput("filedf")),
                  tabPanel("Summary", verbatimTextOutput("sum")),
                  tabPanel("Data", dataTableOutput("table")),
                  tabPanel("Attribute", verbatimTextOutput("selection"),  
                           chooserInput("mychooser", "Available frobs", "Selected frobs",
                                        names(data()), names(data2()), size = 10, multiple = TRUE),
                  h5("Move Variable to the right for use in Spatial Regression Model"),
                  actionButton("button", "Select Variable")),
                  tabPanel("Regression", verbatimTextOutput("reg")),
                  tabPanel("Map", leafletOutput("map", width = "1200px", height = "600px")))
  })
})

shinyApp(server = server, ui = ui)