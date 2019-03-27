
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
# install.packages("vembedr")
# install.packages("shinyLP")
# install.packages("shinythemes")
# install.packages("gridExtra")
# install.packages("e1071")

library(e1071)
library(gridExtra)
library(shinythemes)
library(shinyLP)
library(tidyverse)
library(shiny)
library(DT)
library(mice)
library(BH)
library(ggplot2)
library(scales)
library(devtools)
library(Rcpp)
library(rCharts)
require(markdown)
require(data.table)
library(dplyr)
library(shinyjs)
library(plotly)
library(vembedr)
library(caret)

url="http://halweb.uc3m.es/esp/Personal/personas/imolina/esp/Archivos/VinhoVerdeQuality_Data.csv"

vino=read.csv(url,header=TRUE,sep=";")
vino <- as_tibble(vino)
vino=vino %>%select(-starts_with("X"))
# vino=vino %>% select('fixed.acidity','volatile.acidity',"citric.acid","residual.sugar","chlorides",
#                      "free.sulfur.dioxide","total.sulfur.dioxide","density","pH",
#                      "sulphates","alcohol", "quality", "Variant", "Taste")
vino=vino %>%  drop_na()
var_vino=vino %>% select(-Taste,-pH,-Variant,-quality)
scale_vino=vino %>% select(-Taste,-Variant)
#scale_vino=as.data.frame(scale(scale_vino))
#scale_vino=merge(scale_vino,vino[,c("Taste","Variant")])
# PUT THE SCALE IN THE CORRECT WAY
# for(i in 1:dim(vino)[2]){
#   if(is.numeric(vino[,i])==TRUE) scale_vino=vino[,-i]
# }
# spl1 = createDataPartition(dataW$Variant, p = 0.7, list = FALSE) 
# spl2 = createDataPartition(dataR$Variant, p = 0.3, list = FALSE) 
# 
# a=vino[spl1,]
# b=vino[spl2,]
# 
# dim(a)[1]+dim(b)[1]

#as.factor(vino$quality)


########## PREDICTIVE???
# mymodel<-svm(as.factor(Variant)~., data=vino, kernel="radial")
# mydata=data.frame(Taste="Sweet",alcohol=10,fixed.acidity=7.4,volatile.acidity=0.58,citric.acid=0.18,
#                   residual.sugar=1.70,chlorides=0.114,free.sulfur.dioxide=9, total.sulfur.dioxide=145, density=0.9974,
#                   pH=3.11, sulphates=0.52, quality=9)
# predict(mymodel,mydata)


# Define UI for application that draws a histogram
ui <- navbarPage("Vinho Verde Wine EXPLORER",
                 theme = shinytheme("cerulean"),
                 #shinythemes::themeSelector(),
                 tabPanel(p(icon("table"),"Dataset"),
                          useShinyjs(),
                          sidebarLayout(position="left",
                                        sidebarPanel(
                                          selectInput("variant","Variant",
                                                      c("All",unique(as.character(vino$Variant)))),
                                          sliderInput("alcohol", label = ("Alcohol Content"), min = min(vino$alcohol),
                                                      max = max(vino$alcohol), step = 0.1, value = c(10,12),
                                                      animate = T, dragRange = T),
                                          sliderInput("quality", label = ("Quality"), min = min(vino$quality),
                                                      max = max(vino$quality), step = 1, value = c(5,8),
                                                      animate = T, dragRange = T),
                                          
                                          checkboxGroupInput("checkGroup2", label = ("Taste"),
                                                             choices = c(unique(as.character(vino$Taste)))),
                                          
                                          checkboxInput("all","Select All/None", value=TRUE),
                                          
                                          checkboxGroupInput("checkGroup3", label = ("Aditional Variables"),
                                                             choices = c(unique(as.character(names(vino))))),
                                          ###,"Variant","pH","quality"
                                          checkboxInput("all2","Select All/None", value=TRUE),
                                          
                                          # Download Button
                                          downloadButton("downloadData", "Download Selection"),
                                          downloadButton("downloadData2", "Download Dataset")
                                        ),#SIDEBAR PANEL
                                        
                                        mainPanel(
                                          tabsetPanel(type="tabs",
                                                      tabPanel(p(icon("search"),"Exploration"), DT::dataTableOutput("table"),verbatimTextOutput("taste")),
                                                      tabPanel("Summary", verbatimTextOutput("summary"))        
                                                      
                                          )       
                                        )#MAIN PANEL
                          )#SIDEBAR LAYOUT
                 ),#TAB PANEL 1
                 
                 tabPanel(p(icon("bar-chart-o"),"Data Visualization"),
                          # checkboxGroupInput("checkGroup", label = h3("Properties"), 
                          #                    choices = c("All",unique(as.character(names(vino))),"Clear All"),
                          #                    selected = c("All",unique(as.character(names(vino))),"Clear All")),
                          # verbatimTextOutput("property")
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Taste",
                                       
                                       selectInput('taste.pie', label = 'Variant', choices = unique(as.character(vino$Variant))),
                                       column(width = 6, class = "well",plotlyOutput('plot3')),                              
                                       column(width = 6, class = "well",plotlyOutput("plot5"))
                              ),
                              
                              tabPanel("Quality",
                                       
                                       selectInput('quality.pie', label = 'Variant', choices = unique(as.character(vino$Variant))),
                                       column(width=6,class="well",plotlyOutput('plot4')),
                                       column(width=6,class="well",plotlyOutput('plot6'))
                              ),
                              
                              tabPanel("Properties",
                                       selectInput('bp', label = 'Property', choices = c(unique(as.character(names(scale_vino))))),
                                       plotlyOutput("plot7")       
                              ),
                              
                              tabPanel("Variant Comparison",
                                       selectInput("comp", label="Property" ,choices=c(unique(as.character(names(scale_vino))))),
                                       plotlyOutput("plot8") 
                              ),
                              
                              tabPanel("Correlation between properties",
                                    fluidRow(   
                                       column(width=3,selectInput('xcol1', label = 'X Variable', choices = names(vino))),
                                       column(width=3,selectInput('ycol1', label = 'Y Variable', choices = names(vino))),
                                       column(width=3,numericInput('obsred', 'Number of Observations of Red wine', 500,
                                                    min = 1, max = nrow(vino %>% filter(Variant=="red")))),
                                       column(width=3,numericInput('obswhite', 'Number of Observations of White wine', 500,
                                                    min = 1, max = nrow(vino %>% filter(Variant=="white"))))
                                    ),
                                       plotlyOutput('plot2')
                                    
                              )
                            )#TABSET PANEL
                          )#MAIN PANEL
                 ),#TAB PANEL 2
                 
                 tabPanel(p(icon("wine-glass-alt") ,"Predictive model"),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("What's the perfect wine for the ocassion?",
                                       fluidRow(
                                       column(width=3,selectInput('tastePred', label = 'Taste desired', choices = unique(as.character(vino$Taste)))),
                                       column(width=3,numericInput('alcoholPred', 'Grades of alcohol desired', 10,
                                                                   min = min(vino$alcohol), max = max(vino$alcohol),step=0.1)),
                                       column(width=3,numericInput('qualityPred', 'Quality desired', 6,
                                                                          min = min(vino$quality), max = max(vino$quality),step=0.1)
                                       )
                              )
                              ),
                              verbatimTextOutput("pred")
                              
                              
                              # tabPanel('Vinho Verde k-means clustering',
                              #          selectInput('xcol', 'X Variable', names(vino)),
                              #          selectInput('ycol', 'Y Variable', names(vino),
                              #                      selected=names(vino)[[2]]),
                              #          numericInput('clusters', 'Cluster count', 3,
                              #                       min = 1, max = 9),
                              #          plotOutput('plot1')
                              # )
                              
                            )#TABSET PANEL
                          )#MAIN PANEL
                 ),#TABPANEL 3
                 
                 tabPanel(p(icon("link"),"More"),
                          mainPanel(
                            tabsetPanel(
                              tabPanel(p(icon("video"),"About"),
                                       uiOutput("video")
                              ),
                              
                              tabPanel(p(icon("paperclip"),"Documentation"),
                                       #uiOutput("tab"),
                                       includeMarkdown("About.md")
                              ),
                              
                              tabPanel(p(icon("file-alt"),'Report'),
                                       radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                                    inline = TRUE),
                                       downloadButton('downloadReport')
                              )
                            )#TABSET PANEL
                          )#MAIN PANEL
                 )#TABPANEL 4
                 
)#UI


#############################################################################################################
#############################################################################################################
#############################################################################################################


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  observe({
    updateCheckboxGroupInput(session=session, 
                             inputId="checkGroup2",
                             choices = unique(as.character(vino$Taste)),
                             selected = if(input$all){ unique(as.character(vino$Taste))})
  })# select/deselect all using action button
  
  observe({
    updateCheckboxGroupInput(session=session, 
                             inputId="checkGroup3",
                             choices = c(unique(as.character(names(vino)))),
                             selected = if(input$all2) {c(unique(as.character(names(vino))))})
  })# select/deselect all using action button
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- vino
    if (input$variant != "All") {
      data=data %>% filter(Variant %in% input$variant)
      #data <- data[data$Variant == input$variant,]
    }
    
    #Choose the correct alcohol interval
    data=data %>% filter(data$alcohol>min(input$alcohol)& data$alcohol<max(input$alcohol))    
    #Choose the correct quality interval
    data=data %>% filter(data$quality>min(input$quality) & data$quality<max(input$quality))    
    
    #Choose the corresponding taste observations
    data=data %>% filter(Taste %in% input$checkGroup2)
    
    #Choose the corresponding additional variables
    data=data %>% select(input$checkGroup3)
    #data=data[,input$checkGroup3]
    
    data
  }))# Filter data based on selections
  
  output$summary <- renderPrint({
    data <- vino
    if (input$variant != "All") {
      data=data %>% filter(Variant %in% input$variant)
      #data <- data[data$Variant == input$variant,]
    }
    
    #Choose the correct alcohol interval
    data=data %>% filter(alcohol>min(input$alcohol)& alcohol<max(input$alcohol))    
    #Choose the correct quality interval
    data=data %>% filter(quality>min(input$quality) & quality<max(input$quality))    
    
    #Choose the corresponding taste observations
    data=data %>% filter(Taste %in% input$checkGroup2)
    #data=data[data$Taste == input$checkGroup2,]
    
    #Choose the corresponding additional variables
    data=data %>% select(input$checkGroup3)
    
    summary(data)
  })# Summary
  
  selectedData2 <- reactive({
    dataW=vino %>% filter(Variant=="white")
    dataR=vino %>% filter(Variant=="red")
    union(dataW %>% head(input$obswhite), dataR %>% head(input$obsred))
  })
  
  output$plot2 <- renderPlotly({
    p=ggplot(selectedData2(), aes_string(x=input$xcol1, y=input$ycol1, color=selectedData2()$Taste)) +
      geom_point(size=2, shape=23)+
     # scale_fill_brewer(palette = "Set3")+
      geom_smooth(method="lm", se=TRUE, fullrange=TRUE)+
      theme_minimal()
    ggplotly(p)
  })#Correlation plot
  
  output$plot3 <- renderPlotly({
    
    if (input$taste.pie=="red"){
      
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
      
      plot_ly(df, labels = ~Taste, values = ~value, type = 'pie',colors ="Set3") %>%
        layout(title = 'Taste distribution in the red variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    }else if (input$taste.pie=="white"){
      
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
      
      plot_ly(df, labels = ~Taste, values = ~value, type = 'pie',colors="Set3") %>%
        layout(title = 'Taste distribution in the white variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    }
  })#Piechart variant
  
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
      
      plot_ly(df, labels = ~Quality, values = ~value, type = 'pie',colors="Set3") %>%
        layout(title = 'Quality distribution in the red variant',
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
      
      plot_ly(df, labels = ~Quality, values = ~value, type = 'pie', colors="Set1") %>%
        layout(title = 'Quality distribution in the white variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })#Piechart quality
  
  
  output$plot5 <- renderPlotly({
    p <- ggplot(vino, aes(x = Variant)) + 
      geom_bar(aes(y = ..count../sum(..count..), fill = Taste)) + 
      scale_fill_brewer(palette = "Set3") + 
      ylab("Percentage") + 
      theme_minimal()+
      ggtitle("Taste distribution in both variants")
    
    p <- ggplotly(p)
    p
  })
  
  output$plot6 <- renderPlotly({
    vino$quality=as.factor(vino$quality)
    p <- ggplot(vino, aes(x = Variant)) + 
      geom_bar(aes(y = ..count../sum(..count..), fill = quality)) + 
      scale_fill_brewer(palette = "Set3") + 
      ylab("Percentage") + 
      theme_minimal()+
      ggtitle("Quality distribution in both variants")
    
    p <- ggplotly(p)
    p
  })
  
  output$plot7 <- renderPlotly({
    p<-ggplot(vino,aes_string(x="Taste",y=input$bp))+
      geom_boxplot(aes(color=Taste),outlier.shape = NA)+
      scale_fill_brewer(palette = "Set3")+
      theme_minimal()
    p<-ggplotly(p)
    p
    # p <- plot_ly(vino, x = ~Taste, y = ~input$bp, color = ~Taste, type = "box") %>%
    #   layout(boxmode = "group")
    # p
  })
  
  output$plot8 <- renderPlotly({
    p <- ggplot(vino, aes_string(x = input$comp)) + 
      geom_bar(aes(y = ..count.., fill = Variant)) +
      scale_fill_brewer(palette = "Set2") + 
      ylab("Number of wines") + 
      theme_minimal()
      #ggtitle("Show precentages in bar chart")
    
    p <- ggplotly(p)
    p
  })
    
  # output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.Rt
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # })
  
  
  # output$property <- renderPrint({input$checkGroup})
  
  
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
  }
  )
  
  data1 <- reactive({
    # req(input$gender)
    data.frame(Taste=input$tastePred,
               alcohol=input$alcoholPred)
  })
  
  pred <- reactive({
    predict(mymodel,data1())
  })
  output$pred <- renderPrint(pred())
  
  #Selected data download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Selection", ".csv", sep = "")
    },
    content = function(file) {
      data <- vino
      if (input$variant != "All") {
        data=data %>% filter(Variant %in% input$variant)
      }
      
      #Choose the correct alcohol interval
      data=data %>% filter(alcohol>min(input$alcohol)& alcohol<max(input$alcohol))    
      #Choose the correct quality interval
      data=data %>% filter(quality>min(input$quality) & quality<max(input$quality))     
      
      #Choose the corresponding taste observations
      data=data %>% filter(Taste %in% input$checkGroup2)
      
      #Choose the corresponding additional variables
      data=data %>% select(input$checkGroup3)
      #data=data[,input$checkGroup3]
      
      write.csv(data, file, row.names = TRUE)
    }
  )
  
  #Full data download
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("Dataset", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(vino, file, row.names = TRUE)
    }
  )

  output$video <- renderUI({
     HTML(paste0('<iframe width="800" height="500" src="https://www.youtube.com/embed/IXeNuHpOhHM" frameborder="0" allowfullscreen></iframe>'))
    
    # iframe(width = "560", height = "315",
           # url_link = "https://www.youtube.com/watch?v=IXeNuHpOhHM")
    #tags$video(src = "https://www.youtube.com/watch?v=IXeNuHpOhHM", type = "video/mp4", autoplay = NA, controls = NA)
  })
  
  # output$tab <- renderUI({
  #   url <- a("UCI Machine Learning Repository", href="https://archive.ics.uci.edu/ml/datasets/wine+quality")
  #   url1 <- a("UCI sdfgds Learning Repository", href="https://archive.ics.uci.edu/ml/datasets/wine+quality")
  #   url2 <- a("UCI dsdsb Learning Repository", href="https://archive.ics.uci.edu/ml/datasets/wine+quality")
  #   tagList(div("URL link:", url),div("URL link:", url1),div("URL link:", url2))
  #   
  # })
  
}#SERVER

# Run the application 
shinyApp(ui = ui, server = server)
