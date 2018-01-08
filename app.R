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
library(tidyverse)
library(broom)

# Read in the pre-formatted data ----

mdstest <- read.csv("data/mdstest.csv")

# Load tidy table producing function ----
# source()

# Load plotting function ----
# source()



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
                  choices = list(
                    "Places where you socialize or engage in community activities" = "B3001",
                    "Shopes, banks and post office" = "B3002",
                    "Transportation" = "B3003",
                    "Dwelling including the toilet" = "B3004"
                  ),
                  selected = NULL,
                  selectize = TRUE,
                  multiple = TRUE
                ),
                
                selectInput(
                  "disaggregators",
                  h5("Disaggregators"),
                  choices = list(
                    "Sex" = "sex",
                    "Age group" = "age_cat",
                    "Disability level" = "performance_cat"
                  ),
                  selected = NULL,
                  selectize = TRUE,
                  multiple = TRUE
                )
              ),
              
              mainPanel(
                textOutput("selected_country"),
                textOutput("selected_indicators"),
                textOutput("selected_disaggregators"),
                plotOutput("graph"),
                verbatimTextOutput("verb")
              )
            ))


# Define server logic ----
server <- function(input, output) {
  output$selected_country <- renderText({
    paste("You have selected the country", input$country)
  })
  
  output$selected_indicators <- renderText({
    paste("You have selected the indicators", paste(input$indicators,collapse = ", "))
  })
  
  output$selected_disaggregators <- renderText({
    paste("You have selected the disaggregators", paste(input$disaggregators,collapse = ", "))
  })
  
  output$graph <- renderPlot({

    # tab <- tabFunc(...)
    # plotFunc(tab,...)
    
    if (!is.null(input$indicators)) {
      tab <- table(mdstest[, input$indicators])
      plot(tab)
    }
  })
  
  # output$verb <- renderPrint({
  #   input$indicators
  #   head(mdstest[,input$indicators])
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
