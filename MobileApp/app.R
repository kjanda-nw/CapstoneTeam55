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
library(ggplot2)
library(dplyr)
library(tidyverse)
library(reshape2)

#load the data
ind <- read.csv("country_pred_delta_forest.csv", stringsAsFactors = FALSE)

#start with a simple dataset
ftd <- ind[ ,names(ind) %in% c("countryname","yr","forest_area")]

#ui setup
ui <- fluidPage(
  titlePanel("Forest Tracker"),
  "Team 55",
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
    sliderInput("yrInput","Year",1991,2016,value=c(2011,2016),sep=""), #update with predictions
    uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("areaplot"),
      br(), br(),
      tableOutput("results")
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
  
  output$areaplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered()) +
      geom_line(mapping = aes(x=yr, y=forest_area, colour=countryname)) +
      labs(x="Year", y="Forest area (sq. km)", title="Forest area") +
      scale_colour_discrete(name="Country")
  })
  
  output$results <- renderTable({
    filtered()
  })
}

#initial app
shinyApp(ui = ui, server = server)