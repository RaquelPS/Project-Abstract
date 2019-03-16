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
library(ggplot2)
library(dplyr)
library(scales)
library(tidyverse)


url="http://halweb.uc3m.es/esp/Personal/personas/imolina/esp/Archivos/VinhoVerdeQuality_Data.csv"

vino=read.csv(url,header=TRUE,sep=";")
vino <- as_tibble(vino)
vino=vino %>%select(-starts_with("X"))
# vino=vino %>% select('fixed.acidity','volatile.acidity',"citric.acid","residual.sugar","chlorides",
#                      "free.sulfur.dioxide","total.sulfur.dioxide","density","pH",
#                      "sulphates","alcohol", "quality", "Variant", "Taste")
vino=vino %>%  drop_na()

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
                 
                 tabPanel("Correlation between two variables",
                          
                          selectInput('xcol1', label = 'X Variable', choices = names(vino)),
                          selectInput('ycol1', label = 'Y Variable', choices = names(vino)),
                          
                          mainPanel(
                            plotlyOutput('plot2')
                          )
                 ),
                 
                 tabPanel("Pie Chart-Variant",
                          
                          selectInput('variant.pie', label = 'Variant', choices = unique(as.character(vino$Variant))),
                          
                          mainPanel(
                            plotlyOutput('plot3')
                          )
                 ),
                 
                 tabPanel("Pie Chart-Quality",
                          
                          selectInput('quality.pie', label = 'Quality', choices = unique(as.character(vino$Variant))),
                          
                          mainPanel(
                            plotlyOutput('plot4')
                          )
                 ),
                 
                 tabPanel('Vinho Verde k-means clustering',
                          selectInput('xcol', 'X Variable', names(vino)),
                          selectInput('ycol', 'Y Variable', names(vino)),
                          numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
                          mainPanel(
                            plotOutput('plot1')
                          )
                 ),
                 
                 tabPanel("Data Exploration",
                          
                          fluidRow(
                            column(4,
                                   selectInput("variant","Variant:",
                                               c("All",unique(as.character(vino$Variant))))),
                            column(4,
                                   sliderInput("alcohol", label = ("Alcohol Content"), min = min(vino$alcohol),
                                               max = max(vino$alcohol), step = 0.1, value = c(10,12),
                                               animate = T, dragRange = T)),
                            column(4,
                                   sliderInput("quality", label = ("Quality"), min = min(vino$quality),
                                               max = max(vino$quality), step = 1, value = c(5,8),
                                               animate = T, dragRange = T)),
                            # Create a new row for the table.
                            DT::dataTableOutput("table")
                          )
                 ),
                 
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
  output$table <- DT::renderDataTable(DT::datatable({
    data <- vino
    if (input$variant != "All") {
      
      #?????????????????data=data %>% filter(data$Variant== data$Variant) 
      
      data <- data[data$Variant == input$variant,]
    }
    
    #Choose the correct alcohol interval
    data=data %>% filter(data$alcohol>min(input$alcohol)& data$alcohol<max(input$alcohol))
    #data=data[which(data$alcohol>min(input$alcohol) & data$alcohol<max(input$alcohol)),]
    
    #Choose the correct quality interval
    data=data %>% filter(data$quality>min(input$quality) & data$quality<max(input$quality))
    #data=data[which(data$quality>min(input$quality) & data$quality<max(input$quality)),]
    
    data
    
  }))
  
  output$property <- renderPrint({ input$checkGroup })
  
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
  
  output$plot1 <- renderPlot({
    
    selectedData <- reactive({
      vino[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
      kmeans(selectedData(), input$clusters)
    })
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 0.5)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
  #Correlation plot
  output$plot2 <- renderPlotly({
    
    p=ggplot(vino, aes_string(x=input$xcol1, y=input$ycol1, color=vino$Taste)) +
      geom_point(size=2, shape=23)+
      geom_smooth(method="lm", se=TRUE, fullrange=TRUE)
    ggplotly(p)
  })
  
  
  #Piechart variant
  output$plot3 <- renderPlotly({
    
    if (input$variant.pie=="red"){
      
      # x1=sum((vino$Taste)=="Balanced" & (vino$Variant)=="red")
      # x2=sum((vino$Taste)=="Light-Bodied" & (vino$Variant)=="red")
      # x3=sum((vino$Taste)=="Low acid" & (vino$Variant)=="red")
      # x4=sum((vino$Taste)=="Sweet" & (vino$Variant)=="red")
      # x5=sum((vino$Taste)=="Very low acid" & (vino$Variant)=="red")
      x1=nrow(vino %>% filter(Taste== 'Balanced', Variant=="red")) 
      x2=nrow(vino %>% filter(Taste== 'Light-Bodied', Variant=="red")) 
      x3=nrow(vino %>% filter(Taste== 'Low acid', Variant=="red"))
      x4=nrow(vino %>% filter(Taste== 'Sweet', Variant=="red"))
      x5=nrow(vino %>% filter(Taste== 'Very low acid', Variant=="red"))
      #total.red=sum((vino$Variant)=="red")
      total.red=nrow(vino %>% filter(Variant=="red"))
      value = c(x1,x2,x3,x4,x5)/total.red
      
      df <- data.frame(Taste = c("Balanced", "Light-Bodied", "Low acid", "Sweet", "Very low acid"),
                       value = c(x1,x2,x3,x4,x5)/total.red)
      # ggplot(df, aes(x="", y=value, fill=Taste))+
      #   geom_bar(width = 1, stat = "identity")+
      #   coord_polar("y", start=0)+
      #   geom_text(aes(y = value), 
      #                 label = percent(value), size=3) 
      
      plot_ly(df, labels = ~Taste, values = ~value, type = 'pie') %>%
        layout(title = 'Percentage of each vino taste within the red variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    }else if (input$variant.pie=="white"){
      
      # x1=sum((vino$Taste)=="Balanced" & (vino$Variant)=="white")
      # x2=sum((vino$Taste)=="Light-Bodied" & (vino$Variant)=="white")
      # x3=sum((vino$Taste)=="Low acid" & (vino$Variant)=="white")
      # x4=sum((vino$Taste)=="Sweet" & (vino$Variant)=="white")
      # x5=sum((vino$Taste)=="Very low acid" & (vino$Variant)=="white")
      # total.white=sum((vino$Variant)=="white")
      
      x1=nrow(vino %>% filter(Taste== 'Balanced', Variant=="white")) 
      x2=nrow(vino %>% filter(Taste== 'Light-Bodied', Variant=="white")) 
      x3=nrow(vino %>% filter(Taste== 'Low acid', Variant=="white"))
      x4=nrow(vino %>% filter(Taste== 'Sweet', Variant=="white"))
      x5=nrow(vino %>% filter(Taste== 'Very low acid', Variant=="white"))
      total.white=nrow(vino %>% filter(Variant=="white"))
      value = c(x1,x2,x3,x4,x5)/total.white
      
      df <- data.frame(Taste = c("Balanced", "Light-Bodied", "Low acid", "Sweet", "Very low acid"),
                       value = c(x1,x2,x3,x4,x5)/total.white)
      # ggplot(df, aes(x="", y=value, fill=Taste))+
      #   geom_bar(width = 1, stat = "identity")+
      #   coord_polar("y", start=0)+
      #   geom_text(aes(y = value), 
      #             label = percent(value), size=3) 
      
      plot_ly(df, labels = ~Taste, values = ~value, type = 'pie') %>%
        layout(title = 'Percentage of each vino taste within the white variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    }
    
  })
  
  #Piechart quality
  output$plot4 <- renderPlotly({
    
    if (input$quality.pie=="red"){
      
      # x3=sum((vino$quality)==3 & (vino$Variant)=="red")
      # x4=sum((vino$quality)==4 & (vino$Variant)=="red")
      # x5=sum((vino$quality)==5 & (vino$Variant)=="red")
      # x6=sum((vino$quality)==6 & (vino$Variant)=="red")
      # x7=sum((vino$quality)==7 & (vino$Variant)=="red")
      # x8=sum((vino$quality)==8 & (vino$Variant)=="red")
      # x9=sum((vino$quality)==9 & (vino$Variant)=="red")
      # total.red=sum((vino$Variant)=="red")
      
      x3=nrow(vino %>% filter(quality== 3, Variant=="red"))
      x4=nrow(vino %>% filter(quality== 4, Variant=="red"))
      x5=nrow(vino %>% filter(quality== 5, Variant=="red"))
      x6=nrow(vino %>% filter(quality== 6, Variant=="red"))
      x7=nrow(vino %>% filter(quality== 7, Variant=="red"))
      x8=nrow(vino %>% filter(quality== 8, Variant=="red"))
      x9=nrow(vino %>% filter(quality== 9, Variant=="red"))
      total.red=nrow(vino %>% filter(Variant=="red"))
      value = c(x3,x4,x5,x6,x7,x8,x9)/total.red
      
      df <- data.frame(Quality = c("Quality=3", "Quality=4", "Quality=5", "Quality=6", 
                                   "Quality=7","Quality=8","Quality=9"),
                       value = c(x3,x4,x5,x6,x7,x8,x9)/total.red)
      
      # ggplot(df, aes(x="", y=value, fill=Quality))+
      #   geom_bar(width = 1, stat = "identity")+
      #   coord_polar("y", start=0)+
      #   geom_text(aes(y = value), 
      #             label = percent(value), size=3) 
      
      plot_ly(df, labels = ~Quality, values = ~value, type = 'pie') %>%
        layout(title = 'Percentage of each vino quality within the red variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    }else if (input$quality.pie=="white"){
      
      # x3=sum((vino$quality)==3 & (vino$Variant)=="white")
      # x4=sum((vino$quality)==4 & (vino$Variant)=="white")
      # x5=sum((vino$quality)==5 & (vino$Variant)=="white")
      # x6=sum((vino$quality)==6 & (vino$Variant)=="white")
      # x7=sum((vino$quality)==7 & (vino$Variant)=="white")
      # x8=sum((vino$quality)==8 & (vino$Variant)=="white")
      # x9=sum((vino$quality)==9 & (vino$Variant)=="white")
      # total.white=sum((vino$Variant)=="white")
      
      x3=nrow(vino %>% filter(quality== 3, Variant=="white"))
      x4=nrow(vino %>% filter(quality== 4, Variant=="white"))
      x5=nrow(vino %>% filter(quality== 5, Variant=="white"))
      x6=nrow(vino %>% filter(quality== 6, Variant=="white"))
      x7=nrow(vino %>% filter(quality== 7, Variant=="white"))
      x8=nrow(vino %>% filter(quality== 8, Variant=="white"))
      x9=nrow(vino %>% filter(quality== 9, Variant=="white"))
      total.white=nrow(vino %>% filter(Variant=="white"))
      value = c(x3,x4,x5,x6,x7,x8,x9)/total.white
      
      df <- data.frame(Quality = c("Quality=3", "Quality=4", "Quality=5", "Quality=6", 
                                   "Quality=7","Quality=8","Quality=9"),
                       value = c(x3,x4,x5,x6,x7,x8,x9)/total.white)
      
      # ggplot(df, aes(x="", y=value, fill=Quality))+
      #   geom_bar(width = 1, stat = "identity")+
      #   coord_polar("y", start=0)+
      #   geom_text(aes(y = value), 
      #             label = percent(value), size=3) 
      
      plot_ly(df, labels = ~Quality, values = ~value, type = 'pie') %>%
        layout(title = 'Percentage of each vino quality within the white variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    }
    
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