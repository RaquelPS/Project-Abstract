#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages("devtools")
# install.packages("Rcpp")
# install_github('ramnathv/rCharts', force= TRUE)
# install_github('rCharts', 'ramnathv')
# install.packages("shinyjs")


library(shiny)
library(DT)
library(mice)
library(BH)
library(devtools)
library(Rcpp)
library(rCharts)
require(markdown)
require(data.table)
library(dplyr)
# library(shinyjs)

url="http://halweb.uc3m.es/esp/Personal/personas/imolina/esp/Archivos/VinhoVerdeQuality_Data.csv"

vino=read.csv(url,header=TRUE,sep=";")
vino=vino[,4:dim(vino)[2]]
vino=na.omit(vino)
scale_vino=vino
for(i in 1:dim(vino)[2]){
  if(is.numeric(vino[,i])==TRUE) scale_vino[,i]=scale(vino[,i])
}
  

# Define UI for application that draws a histogram
ui <- navbarPage("Vinho Verde Wine EXPLORER",
                 # # Application title
                 # tabPanel("Old Faithful Geyser Data",
                 #          
                 #          # Sidebar with a slider input for number of bins 
                 #          sidebarLayout(position="right",
                 #                        sidebarPanel(
                 #                          #actionButton("download", "Download the data set"),
                 #                          sliderInput("bins",
                 #                                      "Number of bins:",
                 #                                      min = 1,
                 #                                      max = 50,
                 #                                      value = 30)),
                 #                        # Show a plot of the generated distribution
                 #                        mainPanel(
                 #                          plotOutput("distPlot")
                 #                        )
                 # 
                 #                                              )
                 # ),
                 
                 tabPanel('Vinho Verde k-means clustering',
                     selectInput('xcol', 'X Variable', names(vino)),
                     selectInput('ycol', 'Y Variable', names(vino),
                                 selected=names(vino)[[2]]),
                     numericInput('clusters', 'Cluster count', 3,
                                  min = 1, max = 9),
                   mainPanel(
                     plotOutput('plot1'))
                   ),
                 
                 tabPanel("Data Exploration",
                      sidebarLayout(position="left",
                            sidebarPanel(
                                selectInput("variant","Variant:",
                                      c("All",unique(as.character(vino$Variant)))),
                                sliderInput("alcohol", label = ("Alcohol Content"), min = min(vino$alcohol),
                                      max = max(vino$alcohol), step = 0.1, value = c(10,12),
                                      animate = T, dragRange = T),
                                sliderInput("quality", label = ("Quality:"), min = min(vino$quality),
                                      max = max(vino$quality), step = 0.1, value = c(5,8),
                                      animate = T, dragRange = T),

                                checkboxGroupInput("checkGroup2", label = ("Taste:"),
                                      choices = c(unique(as.character(vino$Taste))),
                                      selected = c(unique(as.character(vino$Taste)))),
                            # Download Button
                            downloadButton("downloadData", "Download Selection"),
                            downloadButton("downloadData2", "Download Dataset"),
                            
                            
                            actionButton(inputId = "clearAllBottom",
                                         label = "Clear selection",
                                         icon = icon("square-o")),
                            actionButton(inputId = "selectAllBottom",
                                         label = "Select all",
                                         icon = icon("check-square-o")),
                            uiOutput("tasteControl")
                          ),
                      
                      mainPanel(
                        verbatimTextOutput("taste"),
                        # Create a new row for the table.
                        DT::dataTableOutput("table")
                        #tableOutput("table")
                      )
                    )
                  ),
                 
                 tabPanel("Data Visualization",
                          
                          checkboxGroupInput("checkGroup", label = h3("Properties"), 
                                             choices = c("All",unique(as.character(names(vino))),"Clear All"),
                                             selected = c("All",unique(as.character(names(vino))),"Clear All")),
                          verbatimTextOutput("property")
                          )
          )


#############################################################################################################
#############################################################################################################
#############################################################################################################


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    data <- vino
    if (input$variant != "All") {
      data <- data[data$Variant == input$variant,]
    }
    
    #Choose the correct alcohol interval
    data=data[which(data$alcohol>min(input$alcohol) & data$alcohol<max(input$alcohol)),]
    
    #Choose the correct quality interval
    data=data[which(data$quality>min(input$quality) & data$quality<max(input$quality)),]
    data
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- vino
    if (input$variant != "All") {
      data <- data[data$Variant == input$variant,]
    }
    
    #Choose the correct alcohol interval
    data=data[which(data$alcohol>min(input$alcohol) & data$alcohol<max(input$alcohol)),]
    
    #Choose the correct quality interval
    data=data[which(data$quality>min(input$quality) & data$quality<max(input$quality)),]
    
    # if(input$checkgroup != "All"){
    #   data=data[which(data$Taste == input$checkgroup),]
    # }

    data
    
  }))
  
  # # Initialize reactive values
  # taste <- sort(unique(data$Taste))
  # values <- reactiveValues()
  # values$taste <- taste
  # 
  # 
  # observe({
  #   if(input$selectAllTop > 0) {
  #     updateCheckboxGroupInput(session=session, inputId="taste",
  #                              choices=taste, selected=taste)
  #     values$taste <- taste
  #   }
  # })
  # observe({
  #   if(input$clearAllBottom > 0) {
  #     updateCheckboxGroupInput(session=session, inputId="taste",
  #                              choices=taste, selected=taste)
  #     values$taste <- taste
  #   }
  # })
  # 
  # observe({
  #   if(input$clearAllTop > 0) {
  #     updateCheckboxGroupInput(session=session, inputId="taste",
  #                              choices=taste, selected=NULL)
  #     values$taste <- c()
  #   }
  # })
  # observe({
  #   if(input$clearAllBottom > 0) {
  #     updateCheckboxGroupInput(session=session, inputId="taste",
  #                              choices=taste, selected=NULL)
  #     values$taste <- c()
  #   }
  # })

  
  # Event type checkbox
  # output$tasteControl <- renderUI({
  #   checkboxGroupInput('taste', 'Taste:',
  #                      taste, selected = values$taste)
  # })
  
  observe({
    x <- input$selectAllButton
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateCheckboxGroupInput(session, "CheckboxGroup2",
                             # label = paste("Checkboxgroup label", length(x)),
                             choices = c(unique(as.character(vino$Taste))),
                             selected = c(unique(as.character(vino$Taste)))
    )
  })
  
  observe({
    x <- input$clearAllButton
    
    # Can use character(0) to remove all choices
    if (is.null(x)==F)
      updateCheckboxGroupInput(session, "CheckboxGroup2",
                             label = paste("Checkboxgroup label", length(x)),
                             choices = c(unique(as.character(vino$Taste))),
                             selected = NULL
    )
  })
  
    output$property <- renderPrint({input$checkGroup})
  
  output$taste <- renderPrint({input$checkGroup2})
  
  # output$property <- renderPlot({
  #  data <- vino
  #  if (input$Taste == "All") {
  #    # draw the histogram with the specified number of bins
  #    hist(data[], col = 'darkgray', border = 'white')
  #  }
  #  if (input$Taste != "All") {
  #    data <- data[data$Taste == input$Taste,]
  #    # draw the histogram with the specified number of bins
  #    hist(data, col = 'darkgray', border = 'white')
  #   }
  # 
  # 
  # })
  
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    vino[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 0.5)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
  
  # output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.Rt
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # })
  
  # Downloadable csv of selected dataset ----
  
  #Selected data download
  output$downloadData <- downloadHandler(
    filename = function() {
      # data <- vino
      # if (input$variant != "All") {
      #   data <- data[data$Variant == input$variant,]
      # }
      # 
      # #Choose the correct alcohol interval
      # data=data[which(data$alcohol>min(input$alcohol) & data$alcohol<max(input$alcohol)),]
      # 
      # #Choose the correct quality interval
      # data=data[which(data$quality>min(input$quality) & data$quality<max(input$quality)),]
      # 
      paste(input$data, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = TRUE)
    }
  )
  #Full data download
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste(vino, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = TRUE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
