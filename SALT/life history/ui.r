library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)

# Define UI
ui <- page_sidebar(
  title = "Life History Relationships",
  
  sidebar = sidebar(
    width = 350,
    
    #  Parameters
    
    #Natural Mortality & Growth (von Bertalanffy) Parameters 
    card(
      card_header("Natural Mortality (M)"),
      numericInput("M", "Natural Mortality Rate (M)", value = 0.2, min = 0.01, max = 2, step = 0.01)
    ),
    card(
      card_header("Growth Parameters. Lengths in cm."),
      numericInput("Linf", "L∞ (Asymptotic Length)", value = 100, min = 10, max = 500),
      numericInput("K", "Growth Rate (K)", value = 0.15, min = 0.01, max = 2, step = 0.01),
      numericInput("t0", "t₀ (Theoretical age at length 0)", value = -1, min = -10, max = 1, step = 0.1)
    ),
    
    # Maturity Parameters
    card(
      card_header("Maturity Parameters. Lengths in cm"),
      numericInput("L50", "L₅₀ (Length at 50% maturity)", value = 66, min = 5, max = 200),
      numericInput("L95", "L₉₅ (Length at 95% maturity)", value = 80, min = 10, max = 300)
    ),
    
    # Weight-Length Parameters
    card(
      card_header("Weight (kg)-Length (cm) Parameters"),
      numericInput("a", "Parameter 'a'", value = 0.00001, min = 0.001, max = 1, step = 0.001),
      numericInput("b", "Parameter 'b'", value = 3, min = 1, max = 5, step = 0.1)
    ),
   ),
  
  # Main panel with plots
  card(
    card_header("Biological Relationships"),
    layout_columns(
      card(
        card_header("Natural Mortality"),
        plotlyOutput("mortality_plot")
      ),
      card(
        card_header("Growth Curve"),
        plotlyOutput("growth_plot")
      ),
      col_widths = c(6, 6)
    ),
    layout_columns(
      card(
        card_header("Maturity"),
        plotlyOutput("maturity_plot")
      ),
      card(
        card_header("Weight-Length Relationship"),
        plotlyOutput("weight_length_plot")
      ),
      col_widths = c(6, 6)
    )
  )
)

