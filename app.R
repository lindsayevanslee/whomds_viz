library(shiny)
library(shinythemes)
library(plyr)
library(tidyverse)
library(broom)
library(whomds)

# Read in the pre-formatted data ----
df_all <- list(srilanka_brief = read_csv("data/srilanka_brief.csv"),
               chile_brief = read_csv("data/chile_brief.csv"))
# srilanka_brief <- read_csv("data/srilanka_brief.csv")
# chile_brief <- read_csv("data/chile_brief.csv")

# Load tidy table producing function ----
source("tab_for_plot.R")

# Load plotting function ----
source("mds_barplot.R")

# Save indicator choices input (all countries must have same column labels)
indicator_choices <- list(
  "Places where you socialize or engage in community activities" = "BE1",
  "Shops, banks and post office" = "BE2",
  "Transportation" = "BE3",
  "Dwelling including the toilet" = "BE4"
)


# Define UI ----
ui <-
  fluidPage(theme = shinytheme("yeti"),
            
            titlePanel("DRAFT MODEL DISABILITY SURVEY VISUALIZATION"),
            
            sidebarLayout(
              sidebarPanel(
                h3("Options"),
                
                selectInput(
                  "country",
                  h5("Country"),
                  choices = list("Chile" = "chile_brief",
                                 "Sri Lanka" = "srilanka_brief"),
                  selected = "srilanka_brief"
                ),
                
                selectInput(
                  "indicators",
                  h5("Indicators"),
                  choices = indicator_choices,
                  selected = NULL,
                  selectize = TRUE,
                  multiple = TRUE
                ),
                
                checkboxGroupInput(
                  inputId = "resp_values",
                  label = "Response options to include in indicator", 
                  choices = c("1" = "1",
                              "2" = "2", 
                              "3" = "3", 
                              "4" = "4",
                              "5" = "5"),
                  selected = c("4", "5")),
                
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
                  h6("Facet column (must be different than fill column)"),
                  choices = list(
                    "none" = "",
                    "Sex" = "sex",
                    "Age group" = "age_cat",
                    "Disability level" = "performance_cat"
                  ),
                  selected = NULL
                ),
                
                checkboxInput(inputId = "weighted",
                              label = "Use survey weights?",
                              value = TRUE)
                
              ),
              
              mainPanel(
                "THE VALUES LISTED HERE ARE A DEMONSTRATION ONLY AND DO NOT REPRESENT ACCURATE DATA GATHERED BY THE WORLD HEALTH ORGANIZATION.",
                plotOutput("graph"), 
                uiOutput("button")
              )
            )
  )

# Define server logic ----
server <- function(input, output) {
  
  #reactive object that selects data set and creates survey object if desired
  df_final <- reactive({
    
    if (input$weighted) {
      des <- df_all[[input$country]] %>% 
        gather(key = "q",
               value = "resp", !!!rlang::syms(input$indicators)) %>% 
        mutate(q = ordered(q),
               resp = ordered(resp, levels = 1:5)) %>% 
        as_survey_design(
          ids = contains("CLUSTER"),
          strata = contains("STRATA"),
          weights = contains("WEIGHT"),
          nest = TRUE
        ) 
      
      return(des)
      
      
    } else {
      df <- df_all[[input$country]]
      return(df)
    }
    
  })
  
  #plot------
  
  #create function with plot
  myplot <- function() {
    if (!is.null(input$indicators)) {
      #calculate table
      tab <- tab_for_plot(df = df_final(), 
                          cols = input$indicators, 
                          disaggs = c(input$fill_col, input$facet_col),
                          weighted = input$weighted, 
                          resp_values = as.numeric(input$resp_values))
      
      #print graph
      mds_barplot(tab, 
                  fill_col = input$fill_col, 
                  facet_col = input$facet_col, 
                  indicator_choices = indicator_choices)
      
    }
  }
  
  #render the plot
  output$graph <- renderPlot({
    myplot()
  })
  
  
  # make download button appear after data uploaded ----------
  output$button <- renderUI({
    if (is.null(myplot())) return()
    downloadButton("download", "Download plot")
  })
  
  #allow user to download plot ---------
  output$download <- downloadHandler(
    filename = function() { paste0("MDS_PLOT", ".png") },
    content = function(file) {
      ggsave(file, 
             plot = myplot(), 
             device = png(), 
             width = 7, height = 7, 
             units = "cm")
    }
  )
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

##FIX: fix how the downloaded plots look
##FIX: make x-axis labels prettier
##FIX: expand list of indicators
