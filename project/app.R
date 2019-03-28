################################################# 
# SHINY APP - RAQUEL PARRA SUAZO, SHENBIN ZHENG #
#################################################

############
# PACKAGES #
############

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

packages = c("e1071","gridExtra","shinythemes","shinyLP","tidyverse","shiny","DT",
             "mice","BH","ggplot2","scales","devtools","Rcpp","rCharts","markdown",
             "data.table","dplyr","shinyjs","plotly","vembedr","caret","rpart")
lapply(packages, require, character.only = TRUE)

######################
# SOME PREPROCESSING #
######################

url="http://halweb.uc3m.es/esp/Personal/personas/imolina/esp/Archivos/VinhoVerdeQuality_Data.csv"

vino=read.csv(url,header=TRUE,sep=";")
vino <- as_tibble(vino)
vino=vino %>%select(-starts_with("X"))
vino=vino %>%  drop_na()
var_vino=vino %>% select(-Taste,-pH,-Variant,-quality)
scale_vino=vino %>% select(-Taste,-Variant)


######################
######## UI ##########
######################

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
                                          
                                          checkboxInput("all2","Select All/None", value=TRUE),
                                          
                                          # Download Button
                                          downloadButton("downloadData", "Download Selection"),
                                          downloadButton("downloadData2", "Download Dataset")
                                        ),#SIDEBAR PANEL
                                        
                                        mainPanel(
                                          tabsetPanel(type="tabs",
                                                      tabPanel(p(icon("search"),"Exploration"), DT::dataTableOutput("table"),verbatimTextOutput("taste"),verbatimTextOutput("table.comments")),
                                                      tabPanel("Summary", verbatimTextOutput("summary"), verbatimTextOutput("summary.comments"))        
                                                      
                                          )       
                                        )#MAIN PANEL
                          )#SIDEBAR LAYOUT
                 ),#TAB PANEL 1
                 
                 tabPanel(p(icon("bar-chart-o"),"Data Visualization"),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Taste",
                                       selectInput('taste.pie', label = 'Variant', choices = unique(as.character(vino$Variant))),
                                       column(width = 6, class = "well",plotlyOutput('plot3')),                              
                                       column(width = 6, class = "well",plotlyOutput("plot5")),
                                       verbatimTextOutput("taste.comments")
                              ),
                              
                              tabPanel("Quality",
                                       selectInput('quality.pie', label = 'Variant', choices = unique(as.character(vino$Variant))),
                                       column(width=6,class="well",plotlyOutput('plot4')),
                                       column(width=6,class="well",plotlyOutput('plot6')),
                                       verbatimTextOutput("quality.comments")
                              ),
                              
                              tabPanel("Properties",
                                       selectInput('bp', label = 'Property', choices = c(unique(as.character(names(scale_vino))))),
                                       plotlyOutput("plot7"),
                                       verbatimTextOutput("property.comments")
                              ),
                              
                              tabPanel("Variant Comparison",
                                       selectInput("comp", label="Property" ,choices=c(unique(as.character(names(scale_vino))))),
                                       plotlyOutput("plot8"),
                                       verbatimTextOutput("comparison.comments")
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
                                       plotlyOutput('plot2'),
                                       verbatimTextOutput("correlation.comments")
                              )
                            )#TABSET PANEL
                          )#MAIN PANEL
                 ),#TAB PANEL 2
                 
                 tabPanel(p(icon("wine-glass-alt") ,"Predictive model"),
                          
                          tabsetPanel(
                            tabPanel("What's the perfect wine for the ocassion?",
                                     mainPanel(
                                       sidebarLayout(position="left",
                                                     
                                                     sidebarPanel(
                                                       selectInput('tastePred', label = 'Taste desired', choices = unique(as.character(vino$Taste))),
                                                       numericInput('alcoholPred', 'Grades of alcohol desired', 10,
                                                                    min = min(vino$alcohol), max = max(vino$alcohol),step=0.1),
                                                       numericInput('qualityPred', 'Quality desired', 6,
                                                                    min = min(vino$quality), max = max(vino$quality),step=0.1),
                                                       numericInput('acidityPred', 'Acidity desired', 6,
                                                                    min = min(vino$fixed.acidity), max = max(vino$fixed.acidity),step=0.1),
                                                       numericInput('sugarPred', 'Sugar desired', 6,
                                                                    min = min(vino$residual.sugar), max = max(vino$residual.sugar),step=0.1),
                                                       numericInput('phPred', 'pH desired', 3.5,
                                                                    min = min(vino$pH), max = max(vino$pH),step=0.01),
                                                       actionButton("Enter", "Show me!")
                                                     ),
                                                     fluidPage(
                                                       verbatimTextOutput("Pred"),
                                                       verbatimTextOutput("Pred.comments"),
                                                       uiOutput("buy")
                                                     )
                                                  )
                                     )#MAIN PANEL
                            )#TAB PANEL  
                        )#TABSET PANEL
                          
                 ),#TABPANEL 3
                 
                 tabPanel(p(icon("link"),"More"),
                          mainPanel(
                            tabsetPanel(
                              tabPanel(p(icon("video"),"About"),
                                       uiOutput("video")
                              ),
                              
                              tabPanel(p(icon("paperclip"),"Documentation"),
                                       #uiOutput("tab"),
                                       includeMarkdown("Documentation.md")
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


######################
###### SERVER ########
######################

server <- function(input, output,session) {
  
  #First chapter: data exploration and summary
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
    }
    
    #Choose the correct alcohol interval
    data=data %>% filter(data$alcohol>min(input$alcohol)& data$alcohol<max(input$alcohol))    
    #Choose the correct quality interval
    data=data %>% filter(data$quality>min(input$quality) & data$quality<max(input$quality))    
    
    #Choose the corresponding taste observations
    data=data %>% filter(Taste %in% input$checkGroup2)
    
    #Choose the corresponding additional variables
    data=data %>% select(input$checkGroup3)
    data
  }))# Filter data based on selections
  
  output$table.comments <- renderText({
    "This is the dataset we are working with. The user can select the variables of interest, and download the new dataset with the selected variables."
  })
  
  
  output$summary <- renderPrint({
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
    
    summary(data)
  })# Summary
  
  output$summary.comments <- renderText({
    "This is the output of the summary function in R. Here we can see which variables are factors and which are continuous. Those that are continuous are represented with their first quantile, their median, their third quantile, etc."
  })
  
  #Second chapter: correlation plot
  selectedData2 <- reactive({
    dataW=vino %>% filter(Variant=="white")
    dataR=vino %>% filter(Variant=="red")
    union(dataW %>% head(input$obswhite), dataR %>% head(input$obsred))
  })
  
  output$plot2 <- renderPlotly({
    p=ggplot(selectedData2(), aes_string(x=input$xcol1, y=input$ycol1, color=selectedData2()$Taste)) +
      geom_point(size=2, shape=23)+
      geom_smooth(method="lm", se=TRUE, fullrange=TRUE)+
      theme_minimal()
    ggplotly(p)
  })#Correlation plot
  
  output$correlation.comments=renderText({
    "This is the correlation plot between two variables. The user can select the variables to plot and also the number of observations of each wine variant (red or white) in order to have cleaner plots."
  })
  
  #Second chapter: taste pie
  output$plot3 <- renderPlotly({
    
    if (input$taste.pie=="red"){
      
      x1=nrow(vino %>% filter(Taste== 'Balanced', Variant=="red")) 
      x2=nrow(vino %>% filter(Taste== 'Light-Bodied', Variant=="red")) 
      x3=nrow(vino %>% filter(Taste== 'Low acid', Variant=="red"))
      x4=nrow(vino %>% filter(Taste== 'Sweet', Variant=="red"))
      x5=nrow(vino %>% filter(Taste== 'Very low acid', Variant=="red"))
      
      total.red=nrow(vino %>% filter(Variant=="red"))
      value = c(x1,x2,x3,x4,x5)/total.red
      
      df <- data.frame(Taste = c("Balanced", "Light-Bodied", "Low acid", "Sweet", "Very low acid"),
                       value = c(x1,x2,x3,x4,x5)/total.red)
      
      plot_ly(df, labels = ~Taste, values = ~value, type = 'pie',colors ="Set3") %>%
        layout(title = 'Taste distribution in the red variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    }else if (input$taste.pie=="white"){
      
      x1=nrow(vino %>% filter(Taste== 'Balanced', Variant=="white")) 
      x2=nrow(vino %>% filter(Taste== 'Light-Bodied', Variant=="white")) 
      x3=nrow(vino %>% filter(Taste== 'Low acid', Variant=="white"))
      x4=nrow(vino %>% filter(Taste== 'Sweet', Variant=="white"))
      x5=nrow(vino %>% filter(Taste== 'Very low acid', Variant=="white"))
      total.white=nrow(vino %>% filter(Variant=="white"))
      value = c(x1,x2,x3,x4,x5)/total.white
      
      df <- data.frame(Taste = c("Balanced", "Light-Bodied", "Low acid", "Sweet", "Very low acid"),
                       value = c(x1,x2,x3,x4,x5)/total.white)
      
      plot_ly(df, labels = ~Taste, values = ~value, type = 'pie',colors="Set3") %>%
        layout(title = 'Taste distribution in the white variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    }
  })#Piechart variant
  
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
  
  output$taste.comments=renderText({
    "These are the plots related with the percentage of each taste within the whole dataset. We can select the wine variant that we want in order to check which taste is the most common in the selected variant.
    Notice that the barplot in the right hand side is representing the proportion of each taste within red and white wines directly, so that the choice of the wine variant does not affect it. 
    With this barplot we also wanted to represent the large different of amount of data between red and white wines: we have much more white wines than red ones, reason of why we have higher bar in the white variant."
  })
  
  #Second chapter: quality pie
  output$plot4 <- renderPlotly({
    
    if (input$quality.pie=="red"){
      
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
      
      plot_ly(df, labels = ~Quality, values = ~value, type = 'pie',colors="Set3") %>%
        layout(title = 'Quality distribution in the red variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    }else if (input$quality.pie=="white"){
      
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
      
      plot_ly(df, labels = ~Quality, values = ~value, type = 'pie', colors="Set1") %>%
        layout(title = 'Quality distribution in the white variant',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })#Piechart quality
  
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
  
  output$quality.comments=renderText({
    "Here we have ploted the distribution of the dataset in terms of the quality and variant, so that we can see the percentage of each quality within the variant.
    In the right hand side we have also ploted a barplot to point out the same conclusion as before: the amount of white wine's observation is much more than the red ones"
  })
  
  #Second chapter: boxplots of each variable
  output$plot7 <- renderPlotly({
    p<-ggplot(vino,aes_string(x="Taste",y=input$bp))+
      geom_boxplot(aes(color=Taste),outlier.shape = NA)+
      scale_fill_brewer(palette = "Set3")+
      theme_minimal()
    p<-ggplotly(p)
    p
  })
  
  output$property.comments=renderText({
    "Boxplot of each variables (wine characteristics) within each wine taste. We can see the median, the mean and other useful information."
  })
  
  #Second chapter: variables comparison
  output$plot8 <- renderPlotly({
    p <- ggplot(vino, aes_string(x = input$comp)) + 
      geom_bar(aes(y = ..count.., fill = Variant)) +
      scale_fill_brewer(palette = "Set2") + 
      ylab("Number of wines") + 
      theme_minimal()
    
    p <- ggplotly(p)
    p
  })
  
  output$comparison.comments=renderText({
    "We want to represent here the density form of each variables and to notice the differences, if there are any, between both wine variant (red and white)."
  })
  
  data.pred=vino %>% select(c("fixed.acidity", "residual.sugar", "pH",
                              "alcohol", "quality", "Variant", "Taste"))
  mymodel<-rpart(as.factor(Variant)~., method="class", data = data.pred)
  
  observeEvent( input$Enter, {
    Taste=as.factor(input$tastePred)
    alcohol=input$alcoholPred
    quality=input$qualityPred
    fixed.acidity=input$acidityPred
    residual.sugar=input$sugarPred
    pH=input$phPred
    
    data.pred1 = data.frame(fixed.acidity, residual.sugar, alcohol, 
                            pH, quality, Taste)
    
    output$Pred <- renderText({
      x=predict(mymodel, data.pred1)
      if(which.max(x)==1) print("Red")
      else print("White")
    })
    
    output$Pred.comments=renderText({
      x=predict(mymodel, data.pred1)
      
      if(which.max(x)==1) {
        print("Red Vinho Verde wines are an intense red color, sometimes with a pink or bright red foam, and with a vinous aroma, especially of berries. In the mouth it is fresh and intense, and a very good food wine.")
      }else {
        print("White Vinho Verde wines are citrus or straw-colored with rich, fruity and floral aromas, depending on the grapes that are used. They have a balanced palate, and are intense and very refreshing.")
      }
    })
    
    output$buy <- renderUI({
      x=predict(mymodel, data.pred1)
      url1 <- a("Red wine shopping!", href="https://www.portugalvineyards.com/es/s/274/vinho-verde#s[7][]:690&s[6][]:522&s[8][]:&rg:&sid:1&h:leftColumn&id_seo:274")
      url2 <- a("White wine shopping!", href="https://www.portugalvineyards.com/es/s/274/vinho-verde#s[7][]:700&s[6][]:522&s[8][]:&rg:&sid:1&h:leftColumn&id_seo:274")
      if(which.max(x)==1) tagList(div("Where to buy:"),div(url1))
      else tagList(div("Where to buy:"),div(url2))
      
    })
  })
  
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
    
  })
  
  
}#SERVER

shinyApp(ui = ui, server = server)