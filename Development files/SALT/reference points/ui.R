library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)


#make a user interactive fisheries threshhold control rule where the reference points can be changed the x-axis is relative stock size to unfished and the y-axis is catch.

# Define UI
ui <- page_sidebar(
  title = "Fisheries Threshold Harvest Control Rule",
  sidebar = sidebar(
    h5(tags$b("Set your harvest control rule"),style="text-align: center;"),

    # Stock status indicator
    
    # Biomass reference points
    #h6("",style="text-align: center;"),

    h6("Biomass Reference Points relative to unfished",style="text-align: center;"),
    fluidRow(
      column(width = 6,
             numericInput("b_target", 
                 "Target:", 
                 value = 0.4, 
                 min = 0.02, 
                 max = 0.1, 
                 step = 0.01)),
      column(width = 6,
             numericInput("b_limit", 
                 "Limit:", 
                 value = 0.25, 
                 min = 0.01, 
                 max = 1, 
                 step = 0.01))),
    
    fluidRow(
      column(width = 6,
             numericInput("b_nocatch", 
                 "No Catch:", 
                 value = 0.1, 
                 min = 0.01, 
                 max = 1, 
                 step = 0.01)),
        column(width = 6,
               numericInput("buffer", 
                 "Buffer", 
                 value = 1, 
                 min = 0, 
                 max = 1, 
                 step = 0.001))),
    
    
    # Fishing mortality/catch parameters
    #h6("Harvest rate at MSY (or proxy). This is line slope and based on stock productivity.",style="text-align: center;"),
    numericInput("E_msy", 
                 "Harvest rate at MSY (or proxy). This is the blue line slope and based on stock productivity", 
                 value = 0.3, 
                 min = 0.01, 
                 max = 1.0, 
                 step = 0.01),
    
    
#    numericInput("max_catch", 
#                 "Maximum Catch (relative units):", 
#                 value = 1.0, 
#                 min = 0.1, 
#                 max = 2.0, 
#                 step = 0.1),
    
        h5(tags$b("Change this value to see what your catch is at a specific stock size"),style="text-align: center;"),
                  sliderInput("current_stock", 
                     "Spawning Stock Size (SB/SB0):", 
                     value = 0.4, 
                     min = 0.01, 
                     max = 1, 
                     step = 0.01),

# Control rule shape
  #   h5("Control Rule Shape"),
  #   selectInput("rule_type", 
  #               "Control Rule Type:",
  #               choices = list(
  #                 "Linear" = "linear",
  #                 "Hockey Stick" = "hockey",
  #                 "Smooth Transition" = "smooth"
  #               ),
  #               selected = "linear")

   ),
  
  # Main panel with plot and information
layout_columns(
  card(
    card_header("Harvest Control Rule Visualization"),
    plotOutput("control_rule_plot", height = "500px")
  ),

  
  card(
    card_header("Stock Status Summary"),
    verbatimTextOutput("stock_status_RPs")
  ),

card(
  card_header("Harvest Control Rule Summary"),
  verbatimTextOutput("stock_status")
),
col_widths = c(12,6,6),
row_heights = c(2,1)
),
)

