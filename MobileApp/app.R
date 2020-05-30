header = '
MSDS 498
Spring 2020
Team 55 - Forest Tracker

Purpose: Evaluate R-Shiny for creating mobile app. Walking through the tutorial provided in class modules
         plus others.
         
TUTORIAL: https://deanattali.com/blog/building-shiny-apps-tutorial/

'
options(rsconnect.max.bundle.files = 21000)

#import libraries
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(reshape2)

#load the data
ind <- read.csv("Combined_1991_2021_Adj.csv", stringsAsFactors = FALSE)

#Create a melted version to feed the plots
ftd <- melt(ind, id.vars = c("yr","countryname","Sub.Regions","IncomeGroup","Country.Code"))

#read in csv with image id/probabilities/country/location(URL)
is <- read.csv("images.csv")

#create a flag
is$Deforestation_flag <- ifelse(is$pr_selective_logging>0.5 | is$pr_slash_burn>0.5,1,0)
#keep only the probabilities of interest
tt <- is[, names(is) %in% c("Image_Number","Image","CountryName","Deforestation_flag","pr_selective_logging","pr_slash_burn")]
tt <- tt[, c("Image_Number","Image","CountryName","Deforestation_flag","pr_selective_logging","pr_slash_burn")]

#set up lists for drop down selection - we want both a person readable label and the actual variable name
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

#Get a unique list of countries and subregion for dropdown
countries <- ind[,names(ind) %in% c("countryname","Sub.Regions")]
countries2 <- unique(countries)

#ui setup
ui <- shinyUI(fluidPage(
  #formatting for sidebar (selection pane)
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #E0E0E0;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )),
  #app title
  titlePanel("Forest Tracker"),
  #subtxt
  "Capstone Team 55",
  br(), #spaces
  br(),
  sidebarLayout( #set up user select panel
    sidebarPanel(id="sidebar",
    sliderInput("yrInput","Year",1991,2021,value=c(2011,2021),sep=""), #slider defaults to most recent 5 years + predictions
    selectizeInput("fplot_var", "Forest Variable", choices = fvars, selected="forest_area"), #users can select a forest var
    selectizeInput("plot_var", "Predictor Variable",choices = vars), #users can select a model feature
    selectInput("countryInput","Country",
                choice=split(countries2$countryname,countries2$Sub.Regions),
                selected = "Brazil", multiple = TRUE) #users can select one or more countries (defaults to Brazil)
    ),
    #main tabs
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Images",DT::dataTableOutput("images")),
                  tabPanel("Plots",
                           plotOutput("areaplot"),
                           plotOutput("predplot")
                  ),
                  tabPanel("Table",tableOutput("results"))
      )
    )
  )
))
#server setup
server <- shinyServer(function(input, output) {

  #filter dataset for table (reactive to user inputs)
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
  #filter dataset for predictor plot (reactive to user inputs)
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
  #filter dataset for forest plot (reactive to user inputs)
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
  #filter dataset of images (reactive to user inputs)
  ifiltered <- reactive ({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    tt %>%
      filter(CountryName %in% input$countryInput) %>%
      arrange(desc(Deforestation_flag))
  })
  #plot forest variable
  output$areaplot <- renderPlot({
    if (is.null(fplotdata())) {
      return()
    }
    ggplot(fplotdata()) +
      geom_line(mapping = aes(x=yr, y=value, colour=countryname)) +
      labs(x="Year", y="", title=input$fplot_var) +
      scale_colour_discrete(name="Country")
  },height=300
  )
  #plot predictor variable
  output$predplot <- renderPlot({
    if (is.null(plotdata())) {
      return()
    }
    ggplot(plotdata(), aes(x=yr, y=value, colour=countryname)) +
      geom_line() +
      labs(x="Year", y="", title=input$plot_var) +
      scale_colour_discrete(name="Country")
  },height=300
  )
  #render table 
  output$results <- renderTable({
    filtered()
  })
  #render image table
  output$images <- DT::renderDataTable({
    DT::datatable(ifiltered(),escape = FALSE)
  })
})

#initial app
shinyApp(ui = ui, server = server)