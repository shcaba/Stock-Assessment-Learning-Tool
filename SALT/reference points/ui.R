library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)


#make a user interactive fisheries threshhold control rule where the reference points can be changed the x-axis is relative stock size to unfished and the y-axis is catch.

# Define UI
ui <- page_sidebar(
  title = "Fisheries Threshold Control Rule",
  sidebar = sidebar(
    h4("Reference Points"),
    
    # Biomass reference points
    h5("Biomass Reference Points"),
    numericInput("b_target", 
                 "Target Reference Point (B_target/B0):", 
                 value = 0.4, 
                 min = 0.1, 
                 max = 0.8, 
                 step = 0.01),

        numericInput("b_limit", 
                 "Limit Reference Point (B_limit/B0):", 
                 value = 0.2, 
                 min = 0.01, 
                 max = 0.5, 
                 step = 0.01),
    
    
    # Fishing mortality/catch parameters
    h5("Fishing Parameters"),
    numericInput("f_target", 
                 "Target Fishing Mortality (F_target):", 
                 value = 0.3, 
                 min = 0.01, 
                 max = 1.0, 
                 step = 0.01),
    
    numericInput("max_catch", 
                 "Maximum Catch (relative units):", 
                 value = 1.0, 
                 min = 0.1, 
                 max = 2.0, 
                 step = 0.1),
    
    # Control rule shape
    h5("Control Rule Shape"),
    selectInput("rule_type", 
                "Control Rule Type:",
                choices = list(
                  "Linear" = "linear",
                  "Hockey Stick" = "hockey",
                  "Smooth Transition" = "smooth"
                ),
                selected = "linear"),
    
    # Stock status indicator
    h5("Current Stock Status"),
    numericInput("current_stock", 
                 "Current Stock Size (B/B0):", 
                 value = 0.3, 
                 min = 0.01, 
                 max = 1.5, 
                 step = 0.01)
  ),
  
  # Main panel with plot and information
  card(
    card_header("Control Rule Visualization"),
    plotOutput("control_rule_plot", height = "500px")
  ),
  
  card(
    card_header("Stock Assessment Summary"),
    verbatimTextOutput("stock_status")
  )
)

