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

# Read in the pre-formatted data ----

mdstest <- read.csv("data/mdstest.csv")

# Load tidy table producing function ----


# Load plotting function ----




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
                  selected = "Chile"
                ),
                
                checkboxGroupInput(
                  "indicators",
                  h5("Indicators"),
                  choices = list(
                    "EF 1" = "B3001",
                    "EF 2" = "B3002",
                    "EF 3" = "B3003",
                    "EF 4" = "B3004"
                  ),
                  selected = NULL
                ),
                
                checkboxGroupInput(
                  "disaggregators",
                  h5("Disaggregators"),
                  choices = list(
                    "Sex" = "sex",
                    "Age group" = "age_cat",
                    "Disability level" = "performance_cat"
                  ),
                  selected = NULL
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
