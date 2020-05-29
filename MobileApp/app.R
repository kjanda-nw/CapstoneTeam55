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
ftd <- melt(ind, id.vars = c("yr","countryname","Sub.Regions","IncomeGroup","Country.Code"))

#build a datatable with the images
is <- read.csv("images.csv")

tt <- is[, names(is) %in% c("Image_Number","Image","pr_slash_burn","pr_selective_logging","CountryName","Sub.Regions")]

vars <- c("GDP"='gdp',"Prior Year Population"='lag_pop',"Prior Year Burned Area"='lag_x_6798_7246',
          "Prior Year Soybean Oil Export"='lag_x_237_5910',"Prior Year Palm Oil Export"='lag_x_257_5910',
          "Prior Year Production of Pulpwood"='lag_x_1603_5516',
          "Prior Year Production of Saw and Veneer Logs"='lag_x_1604_5516',
          "Prior Year Production of Other Industrial Roundwood"='lag_x_1626_5516',
          "Prior Year Production of Sawnwood"='lag_x_1633_5516',
          "Prior Year Production of Veneer Sheets"='lag_x_1634_5516')
fvars <- c("Forest Area (sq. km)"='forest_area',"Prior Year Forest Area (sq. km)"='lag_forest_area',
           "Percent Change in Forest Area"='delta_forest_area',"Change in Forest Area (sq. km)"='forest_change',
           "Absolute Change in Forest Area (sq. km)"='abs_forest_change')

countries <- ind[,names(ind) %in% c("countryname","Sub.Regions")]
countries2 <- unique(countries)

#ui setup
ui <- fluidPage(
  titlePanel("Forest Tracker"),
  "Team 55",
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
    sliderInput("yrInput","Year",1991,2021,value=c(2011,2021),sep=""), #update with predictions
    selectizeInput("fplot_var", "Forest Variable", choices = fvars, selected="forest_area"),
    selectizeInput("plot_var", "Predictor Variable",choices = vars),
    selectInput("countryInput","Country",
                choice=split(countries2$countryname,countries2$Sub.Regions),
                selected = "Brazil", multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Images",DT::dataTableOutput("images")),
                  tabPanel("Plot",
                           plotOutput("areaplot"),
                           plotOutput("predplot")
                  ),
                  tabPanel("Table",tableOutput("results"))
      )
    )
  )
)
#server setup
server <- function(input, output) {
  #output$countryOutput <- renderUI({
  #  selectInput("countryInput","Country",
  #              choice=split(countries2$Sub.Regions,countries2$countryname),
  #              selected = "Brazil", multiple = TRUE)
  #})
  
  filtered <- reactive ({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    ind %>% 
      filter(yr >= input$yrInput[1],
             yr <= input$yrInput[2],
             countryname %in% input$countryInput
      ) %>% subset(select=c('yr','countryname',input$fplot_var,input$plot_var))
    
  })
  plotdata<- reactive ({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    var = input$plot_var
    ftd %>% 
      filter(yr >= input$yrInput[1],
             yr <= input$yrInput[2],
             countryname %in% input$countryInput,
             variable == var
      )
  })
  fplotdata <- reactive ({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    var = input$fplot_var
    ftd %>% 
      filter(yr >= input$yrInput[1],
             yr <= input$yrInput[2],
             countryname %in% input$countryInput,
             variable == var
      )
    
  })
  
  ifiltered <- reactive ({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    tt %>%
      filter(CountryName %in% input$countryInput) %>%
      arrange(desc(pr_selective_logging))
  })
  
  output$areaplot <- renderPlot({
    if (is.null(fplotdata())) {
      return()
    }
    ggplot(fplotdata()) +
      geom_line(mapping = aes(x=yr, y=value, colour=countryname)) +
      labs(x="Year", title=input$fplot_var) +
      scale_colour_discrete(name="Country")
  })
  output$predplot <- renderPlot({
    if (is.null(plotdata())) {
      return()
    }
    ggplot(plotdata(), aes(x=yr, y=value, colour=countryname)) +
      geom_line() +
      labs(x="Year", title=input$plot_var) +
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