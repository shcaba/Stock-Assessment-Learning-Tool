#Drawing selectivity curves with bins by length or age

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

ui <- page_sidebar(
  title = "Selectivity Curve Designer",
  sidebar = sidebar(
    width = 300,
    h4("Selectivity Parameters"),
    
    # Choose between length or age
    radioButtons(
      "bin_type", 
      "Bin Type:",
      choices = list("Length" = "length", "Age" = "age"),
      selected = "length"
    ),
    
    # Number of bins
    numericInput(
      "n_bins", 
      "Bin step:",
      value = 2,
      min = 1,
      step = 1
    ),
    
    # Bin range inputs (will be updated based on bin_type)
    conditionalPanel(
      condition = "input.bin_type == 'length'",
      numericInput("min_length", "Minimum Length (cm):", value = 10, min = 1),
      numericInput("max_length", "Maximum Length (cm):", value = 80, min = 1)
    ),
    
    conditionalPanel(
      condition = "input.bin_type == 'age'",
      numericInput("min_age", "Minimum Age (years):", value = 1, min = 0),
      numericInput("max_age", "Maximum Age (years):", value = 15, min = 1)
    ),
    
    hr(),
    
    h5("Quick Presets:"),
    actionButton("preset_logistic", "Logistic", class = "btn-sm"),
    actionButton("preset_dome", "Dome-shaped", class = "btn-sm"),
    actionButton("preset_flat", "Knife-edged", class = "btn-sm"),
    actionButton("reset_all", "Reset All", class = "btn-sm btn-outline-secondary"),
    br(),
    h5("Smooth out custom curve?"),
    actionButton("smooth", "Apply Smoothing", class = "btn-info"),
    
    hr(),
    
    p("Adjust individual bin selectivity values using the sliders on the right, or click and drag points on the plot."),
    
    downloadButton("download_data", "Download Data", class = "btn-primary")
  ),
  
  # Main panel with plot and sliders
  layout_columns(
    col_widths = c(8, 4),
    
    # Plot panel
    card(
      card_header("Selectivity Curve"),
      plotOutput("selectivity_plot", 
                 height = "500px",
                 click = "plot_click",
                 hover = "plot_hover")
    ),
    
    # Sliders panel
    card(
      card_header("Bin Selectivity Values"),
      div(
        style = "max-height: 500px; overflow-y: auto;",
        uiOutput("selectivity_sliders")
      )
    )
  )
)
