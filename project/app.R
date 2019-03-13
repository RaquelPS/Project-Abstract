#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(mice)

url="http://halweb.uc3m.es/esp/Personal/personas/imolina/esp/Archivos/VinhoVerdeQuality_Data.csv"

vino=read.csv(url,header=TRUE,sep=";")
vino=vino[,4:dim(vino)[2]]
vino=na.omit(vino)

# Define UI for application that draws a histogram
ui <- navbarPage("Project",
                 # Application title
                 tabPanel("Old Faithful Geyser Data",
                          
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(position="right",
                                        sidebarPanel(
                                          #actionButton("download", "Download the data set"),
                                          sliderInput("bins",
                                                      "Number of bins:",
                                                      min = 1,
                                                      max = 50,
                                                      value = 30)),
                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          plotOutput("distPlot")
                                        )
                          )
                 ),
                 
                   tabPanel('Vinho Verde k-means clustering',
                     selectInput('xcol', 'X Variable', names(vino)),
                     selectInput('ycol', 'Y Variable', names(vino),
                                 selected=names(vino)[[2]]),
                     numericInput('clusters', 'Cluster count', 3,
                                  min = 1, max = 9),
                   mainPanel(
                     plotOutput('plot1')
                   )
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
                                               choices = c("All",unique(as.character(vino$Taste)),"Clear All"),
                                               selected = c("All",unique(as.character(vino$Taste)),"Clear All")),
                            hr(),
                            fluidRow(column(3, verbatimTextOutput("taste")))),
                            mainPanel(
                              # Create a new row for the table.
                              DT::dataTableOutput("table")
                              #tableOutput("table")
                              )
                            )),
                            
                 
                 tabPanel("Data Visualization",
                          
                          checkboxGroupInput("checkGroup", label = h3("Properties"), 
                                             choices = c("All",unique(as.character(names(vino))),"Clear All"),
                                             selected = c("All",unique(as.character(names(vino))),"Clear All")),
                          
                          
                          hr(),
                          fluidRow(column(3, verbatimTextOutput("property")))
                          )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- renderTable(table({
    data <- vino
    if (input$variant != "All") {
      data <- data[data$Variant == input$variant,]
    }
    
    #Choose the correct alcohol interval
    data=data[which(data$alcohol>min(input$alcohol) & data$alcohol<max(input$alcohol)),]
    
    #Choose the correct quality interval
    data=data[which(data$quality>min(input$quality) & data$quality<max(input$quality)),]
    
    data
    
  }))
  
  output$property <- renderPrint({ input$checkGroup })
  
  output$taste <- renderPrint({ input$checkGroup2 })
  
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
  
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.Rt
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
