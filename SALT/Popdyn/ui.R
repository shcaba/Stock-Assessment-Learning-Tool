library(shiny)
library(bslib)
library(plotly)
library(dplyr)

# Define UI
ui <- page_sidebar(
  title = "Age-Structured Fishery Model",
  
  sidebar = sidebar(
    width = 300,
    
    # Model parameters
    card(
      card_header("Population Parameters"),
      numericInput("max_age", "Maximum Age:", value = 10, min = 5, max = 20, step = 1),
      numericInput("r0", "Virgin Recruitment (R0):", value = 1000, min = 100, max = 10000, step = 100),
      numericInput("steepness", "Stock-Recruit Steepness (h):", value = 0.7, min = 0.2, max = 1.0, step = 0.1),
      numericInput("natural_m", "Natural Mortality (M):", value = 0.2, min = 0.05, max = 0.5, step = 0.05)
    ),
    
    card(
      card_header("Growth Parameters"),
      numericInput("linf", "L∞ (Asymptotic Length):", value = 80, min = 50, max = 150, step = 5),
      numericInput("k", "Growth Rate (K):", value = 0.3, min = 0.1, max = 1.0, step = 0.1),
      numericInput("t0", "Theoretical Age at Length 0:", value = -1, min = -3, max = 1, step = 0.1)
    ),
    
    card(
      card_header("Fishing Parameters"),
      numericInput("fishing_mort", "Fishing Mortality (F):", value = 0.2, min = 0, max = 1.0, step = 0.05),
      numericInput("selectivity_a50", "Selectivity A50:", value = 3, min = 1, max = 8, step = 0.5),
      numericInput("selectivity_slope", "Selectivity Slope:", value = 2, min = 0.5, max = 5, step = 0.5)
    ),
    
    card(
      card_header("Simulation"),
      numericInput("years", "Number of Years:", value = 50, min = 20, max = 100, step = 10),
      actionButton("run_model", "Run Model", class = "btn-primary")
    )
  ),
  
  # Main panel with results
  layout_columns(
    col_widths = c(12, 6, 6),
    
    card(
      card_header("Population Dynamics Over Time"),
      plotlyOutput("population_plot", height = "400px")
    ),
    
    card(
      card_header("Age Structure (Final Year)"),
      plotlyOutput("age_structure_plot", height = "350px")
    ),
    
    card(
      card_header("Selectivity and Maturity"),
      plotlyOutput("selectivity_plot", height = "350px")
    )
  )
)
