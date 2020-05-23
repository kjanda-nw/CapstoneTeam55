header = '
MSDS 498
Spring 2020
Team 55 - Forest Tracker

Purpose: Evaluate R-Shiny for creating mobile app. Walking through the tutorial provided in class modules
         plus others.
         
TUTORIAL: https://deanattali.com/blog/building-shiny-apps-tutorial/

'
#import libraries
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(reshape2)

#load the data
ind <- read.csv("Combined_1991_2021_Adj.csv", stringsAsFactors = FALSE)

#start with a simple dataset
ftd <- ind[ ,names(ind) %in% c("countryname","yr","forest_change")]

#build a datatable with the images
preds <- read.csv("images.csv")
images <- c('<img src="img/test_0.jpg" height="52"></img>','<img src="img/test_1.jpg" height="52"></img>',
            '<img src="img/test_2.jpg" height="52"></img>','<img src="img/test_3.jpg" height="52"></img>',
            '<img src="img/test_4.jpg" height="52"></img>','<img src="img/test_5.jpg" height="52"></img>',
            '<img src="img/test_6.jpg" height="52"></img>','<img src="img/test_7.jpg" height="52"></img>',
            '<img src="img/test_8.jpg" height="52"></img>','<img src="img/test_9.jpg" height="52"></img>',
            '<img src="img/test_10.jpg" height="52"></img>')
tt <- cbind(preds,images)

#ui setup
ui <- fluidPage(
  titlePanel("Forest Tracker"),
  "Team 55",
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
    sliderInput("yrInput","Year",1991,2021,value=c(2011,2021),sep=""), #update with predictions
    uiOutput("countryOutput")
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Plot",plotOutput("areaplot")),
                  tabPanel("Table",tableOutput("results")),
                  tabPanel("Images",DT::dataTableOutput("images"))
      )
    )
  )
)
#server setup
server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput","Country",
                sort(unique(ftd$countryname)),
                selected = "Brazil", multiple = TRUE)
  })
  
  filtered <- reactive ({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    ftd %>% 
      filter(yr >= input$yrInput[1],
             yr <= input$yrInput[2],
             countryname %in% input$countryInput
      ) 
    
  })
  ifiltered <- reactive ({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    tt %>%
      filter(Country %in% input$countryInput) %>%
      arrange(desc(Deforestation_prob))
  })
  
  output$areaplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered()) +
      geom_line(mapping = aes(x=yr, y=forest_change, colour=countryname)) +
      labs(x="Year", y="Change Forest Area (sq. km)", title="Forest area") +
      scale_colour_discrete(name="Country")
  })
  
  output$results <- renderTable({
    filtered()
  })
  
  output$images <- DT::renderDataTable({
    DT::datatable(ifiltered(),escape = FALSE)
  })
}

#initial app
shinyApp(ui = ui, server = server)