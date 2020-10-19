#install.packages("shinythemes")
require(shinythemes)
require(shiny)
require(plotly)
ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  # Put a titlePanel here
  titlePanel("k-means clustering"),
  
  sidebarLayout(
    # Sidebar. Put your inputs inside the sidebarPanel
    sidebarPanel(
      fileInput("infile", "Upload data here",
                accept = c(
                  "text/tab-separated-values",
                  "text/plain",
                  ".tsv"
                )),
      selectInput('xcol', 'X Variable', c("Upload data first")),
      selectInput('ycol', 'Y Variable', c("Upload data first")),
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 9),
      actionButton("plotIt","Plot it!")
    ),
    
    # Main panel. put your output plot here
    mainPanel(
      tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                      color: #ffffff;
                      }
                      
                      thead {
                      color: #ffffff;
                      }
                      
                      tbody {
                      color: #000000;
                      }
                      
                      "
                      
                      
      )),
      plotlyOutput('plot1'),
      DT::dataTableOutput("firstLines")
      #list(tags$head(tags$style("shipment.table span {color: #333333 ; background: #999999;}")))   
      #tags$head(tags$style("#shipment.table table {color: red;}"))
    )
  )
)

server <- function(input, output, session) {
  tableData <- reactive({
    inFile <- input$infile
    
    if (is.null(inFile))
      return(NULL)
    
    indata <- read.table(inFile$datapath, sep = "\t", header=T)
    indata
  })
  
  observeEvent(input$infile, {
    updateSelectInput(session, "xcol", 'X Variable', choices=colnames(tableData()), selected=colnames(tableData())[1])
    updateSelectInput(session, "ycol", 'Y Variable', choices=colnames(tableData()), selected=colnames(tableData())[2])
  })
  
  selectedData <- eventReactive( input$plotIt, {
    tableData()[, c(input$xcol, input$ycol)]
  })
  
  clusters <- eventReactive( input$plotIt, {
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlotly({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    ggplot(as.data.frame(selectedData()),
           aes_string(x=names(selectedData())[1], y=names(selectedData())[2])) +
      geom_point(col = clusters()$cluster,
                 pch = 20, cex = 3) +
      geom_point(data=as.data.frame(clusters()$centers), pch = 4, cex = 4, lwd = 4)
  })
  
  output$firstLines <- DT::renderDataTable({
    if (is.null(tableData()))
      return(NULL)
    
    head(tableData())
  }) #%>% formatStyle(1,color="black")
}

shinyApp(ui = ui, server = server)

