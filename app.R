#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plyr)
library(tidyverse)
library(broom)

# Read in the pre-formatted data ----
mdstest <- read.csv("data/mdstest.csv")

# Load tidy table producing function ----
source("tabDodgeApp.R")

# Load plotting function ----
source("ggBarplotDodgeApp.R")

# Save indicator choices input
indicator_choices <- list(
  "Places where you socialize or engage in community activities" = "B3001",
  "Shops, banks and post office" = "B3002",
  "Transportation" = "B3003",
  "Dwelling including the toilet" = "B3004"
)


# Define UI ----
ui <-
  fluidPage(theme = shinytheme("yeti"),
            
            titlePanel("MDSViz"),
            
            sidebarLayout(
              sidebarPanel(
                h3("Options"),
                
                selectInput(
                  "country",
                  h5("Country"),
                  choices = list("Chile" = "Chile",
                                 "Sri Lanka" = "Sri Lanka"),
                  selected = "Sri Lanka"
                ),
                
                selectInput(
                  "indicators",
                  h5("Indicators - Hard or very hard to use..."),
                  choices = indicator_choices,
                  selected = NULL,
                  selectize = TRUE,
                  multiple = TRUE
                ),
                
                
                h5("Disaggregators"),
                
                radioButtons(
                  "fill_col",
                  h6("Fill column"),
                  choices = list(
                    "none" = "",
                    "Sex" = "sex",
                    "Age group" = "age_cat",
                    "Disability level" = "performance_cat"
                  ),
                  selected = NULL
                ),
                
                radioButtons(
                  "facet_col",
                  h6("Facet column; must be different than fill column"),
                  choices = list(
                    "none" = "",
                    "Sex" = "sex",
                    "Age group" = "age_cat",
                    "Disability level" = "performance_cat"
                  ),
                  selected = NULL
                )
              ),
              
              mainPanel(
                plotOutput("graph")
                # , verbatimTextOutput("verb")
              )
            )
            )

# Define server logic ----
server <- function(input, output) {

  
  output$graph <- renderPlot({
    
    if (!is.null(input$indicators)) {
      #calculate table
      tab <- tabDodgeApp(mdstest, input$indicators, disaggs = c(input$fill_col, input$facet_col))
      
      #print graph
      ggBarplotDodgeApp(tab, fill_col = input$fill_col, facet_col = input$facet_col)
      
    }
  })
  
  # output$verb <- renderPrint({
  #   unlist(c(input$fill_col, input$facet_col))
  #   # input$facet_col
  #   # head(mdstest[,input$indicators])
  # })
}

# Run the application
shinyApp(ui = ui, server = server)


##FIX: labels on xaxis
##FIX: choose which dataset to use
