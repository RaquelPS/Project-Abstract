#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


url1<-"https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
red.vino<-read.csv(url1, sep = ";")
red.vino$type=rep("red", each=nrow(red.vino))

url2<-"https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
white.vino<-read.csv(url2, sep = ";")
white.vino$type=rep("white", each=nrow(white.vino))

merged.vino=rbind(red.vino,white.vino)

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
                 
                 tabPanel("Data Exploration",
                          
                          
                              fluidRow(
                                column(4,
                                       selectInput("type","Type:",
                                                   c("All",unique(as.character(merged.vino$type))))),
                                column(4,
                                       sliderInput("alcohol", label = ("Alcohol Content"), min = min(merged.vino$alcohol),
                                                   max = max(merged.vino$alcohol), step = 0.1, value = c(10,12),
                                                   animate = T, dragRange = T)),
                                column(4,
                                       sliderInput("quality", label = ("Quality"), min = min(merged.vino$quality),
                                                   max = max(merged.vino$quality), step = 0.1, value = c(5,8),
                                                   animate = T, dragRange = T)),
                                # Create a new row for the table.
                                DT::dataTableOutput("table"))
                          )
                 )
      

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- merged.vino
    if (input$type != "All") {
      data <- data[data$type == input$type,]
    }
    
    #Choose the correct alcohol interval
    data=data[which(data$alcohol>min(input$alcohol) & data$alcohol<max(input$alcohol)),]
    
    #Choose the correct quality interval
    data=data[which(data$quality>min(input$quality) & data$quality<max(input$quality)),]
    
    data
      
  }))
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

