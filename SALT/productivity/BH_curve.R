library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Define UI
ui <- page_sidebar(
  title = "Beverton-Holt Stock Recruitment Curve",
  sidebar = sidebar(
    h4("Stock Recruitment Parameters"),
    numericInput("R0", 
                 "R₀ (Unfished Recruitment)", 
                 value = 1000, 
                 min = 100, 
                 max = 10000, 
                 step = 100),
    numericInput("S0", 
                 "S₀ (Unfished Spawning Biomass)", 
                 value = 1000, 
                 min = 100, 
                 max = 10000, 
                 step = 100),
    sliderInput("steepness", 
                "Steepness (h)", 
                min = 0.2, 
                max = 1.0, 
                value = 0.7, 
                step = 0.05),
    hr(),
    h5("About Steepness:"),
    p("Steepness (h) is the fraction of R₀ expected when spawning biomass is 20% of S₀."),
    p("• h = 0.2: Very low productivity"),
    p("• h = 0.7: Moderate productivity"), 
    p("• h = 1.0: Maximum productivity")
  ),
  
  # Main panel with outputs
  card(
    card_header("Stock Recruitment Curve"),
    plotOutput("sr_plot", height = "400px")
  ),
  
  card(
    card_header("Key Statistics"),
    tableOutput("stats_table")
  ),
  
  card(
    card_header("Beverton-Holt Equation"),
    withMathJax(),
    div(
      style = "text-align: center; font-size: 16px; margin: 20px 0;",
      "$$\\frac{R}{R_0} = \\frac{4h \\cdot \\frac{S}{S_0}}{(1-h) + \\frac{S}{S_0}(5h-1)}$$"
    ),
    p("Where:"),
    tags$ul(
      tags$li("R/R₀ = Relative recruitment"),
      tags$li("S/S₀ = Relative spawning stock biomass"),
      tags$li("h = Steepness parameter")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive function to calculate Beverton-Holt recruitment (relative form)
  beverton_holt_relative <- function(S_rel, h) {
    # Beverton-Holt stock recruitment relationship in relative form
    # S_rel = S/S0, returns R/R0
    numerator <- 4 * h * S_rel
    denominator <- (1 - h) + S_rel * (5 * h - 1)
    R_rel <- numerator / denominator
    return(R_rel)
  }
  
  # Create reactive data
  sr_data <- reactive({
    # Create range of relative spawning biomass values (S/S0)
    S_rel_values <- seq(0, 1, length.out = 200)
    
    # Calculate relative recruitment for each relative spawning biomass
    R_rel_values <- beverton_holt_relative(S_rel_values, input$steepness)
    
    data.frame(
      Relative_Spawning_Biomass = S_rel_values,
      Relative_Recruitment = R_rel_values
    )
  })
  
  # Generate the plot
  output$sr_plot <- renderPlot({
    data <- sr_data()
    
    # Calculate key reference points (in relative terms)
    S_rel_20 <- 0.2
    R_rel_20 <- beverton_holt_relative(S_rel_20, input$steepness)
    
    ggplot(data, aes(x = Relative_Spawning_Biomass, y = Relative_Recruitment)) +
      geom_line(color = "blue", size = 1.2) +
      
      # Add reference lines
      geom_vline(xintercept = 1.0, linetype = "dashed", color = "red", alpha = 0.7) +
      geom_vline(xintercept = 0.2, linetype = "dotted", color = "orange", alpha = 0.7) +
      geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", alpha = 0.7) +
      geom_hline(yintercept = R_rel_20, linetype = "dotted", color = "orange", alpha = 0.7) +
      
      # Add reference points
      geom_point(aes(x = 1.0, y = 1.0), color = "red", size = 3) +
      geom_point(aes(x = 0.2, y = R_rel_20), color = "orange", size = 3) +
      
      # Add annotations
      annotate("text", x = 1.0, y = 1.05, 
               label = "(S₀/S₀, R₀/R₀)", color = "red", hjust = 0.5) +
      annotate("text", x = 0.2, y = R_rel_20 + 0.05, 
               label = paste("(0.2, ", round(R_rel_20, 3), ")", sep = ""), 
               color = "orange", hjust = 0.5) +
      
      # Styling
      labs(
        x = "Relative Spawning Stock Biomass (S/S₀)",
        y = "Relative Recruitment (R/R₀)",
        title = paste("Beverton-Holt Stock Recruitment Curve (h =", input$steepness, ")")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()
      ) +
      xlim(0, 2) +
      ylim(0, max(data$Relative_Recruitment) * 1.1)
  })
  
  # Generate statistics table
  output$stats_table <- renderTable({
    R_rel_20 <- beverton_holt_relative(0.2, input$steepness)
    
    # Calculate absolute values for reference
    R_20 <- R_rel_20 * input$R0
    
    stats_df <- data.frame(
      Parameter = c("R₀ (Unfished Recruitment)", 
                    "S₀ (Unfished Spawning Biomass)",
                    "Steepness (h)",
                    "R/R₀ at S/S₀ = 0.2",
                    "R at 20% S₀ (absolute)",
                    "Replacement line slope",
                    "Maximum R/R₀"),
      Value = c(input$R0,
                input$S0,
                input$steepness,
                round(R_rel_20, 3),
                round(R_20, 1),
                round(1/input$steepness, 2),
                round(beverton_holt_relative(2.0, input$steepness), 3)),
      stringsAsFactors = FALSE
    )
    
    stats_df
  }, digits = 3, align = 'lr')
}

# Run the application
shinyApp(ui = ui, server = server)
