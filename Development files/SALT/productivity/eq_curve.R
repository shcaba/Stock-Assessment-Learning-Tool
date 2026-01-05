library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Define UI
ui <- page_sidebar(
  title = "Productivity and Fishery Yield Analysis: How fishery yield changes with stock productivity",
  
  sidebar = sidebar(
    width = 300,
    
    p("Explore stock productivity (steepness) and the relationship between relative spawning stock biomass and yield per recruit, 
       incorporating selectivity and natural mortality."),
    
    p("Steepness (h) is the fraction of R₀ expected when spawning biomass is 20% of unfished spawning biomass (SB₀)."),
    #p("• h = 0.2: Very low productivity"),
    #p("• h = 0.7: Moderate productivity"), 
    #p("• h = 1.0: Maximum productivity"),
    
   
    
        # Steepness parameter
    sliderInput("steepness", 
                "Steepness (h):", 
                min = 0.2, 
                max = 1.0, 
                value = 0.7, 
                step = 0.01,
                width = "100%"),
    
    # Natural mortality
    sliderInput("natural_mortality", 
                "Natural Mortality (M):", 
                min = 0.01, 
                max = 1, 
                value = 0.2, 
                step = 0.01,
                width = "100%"),

    h5("Age at Maturity & Selectivity"),
    uiOutput("maturity.in"),
    
    
        # Selectivity parameters
#    h5("Selectivity at Age"),
    uiOutput("selectivity.in"),
    
    
    
    # Pretty Good Yield
  #h5("Pretty Good Yield: What % of MSY is good enough?"),
          numericInput("PGY", 
                 "Pretty Good Yield: What % of MSY is good enough?", 
                 value = 0.8, 
                 min = 0, 
                 max = 1, 
                 step = 0.01),
  
  
hr(),

  ),
  

  # Main panel with yield curve plot
layout_columns(
  card(
    card_header("Stock Recruitment Curve"),
    plotOutput("sr_plot", height = "400px")
  ),

  card(
    card_header("Beverton-Holt Stock-Recruitment Equation"),
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
  ),
  col_widths = c(6,6),
  row_heights = c(2,2)
),

layout_columns(
  card(
    card_header("Yield Per Recruit Curve"),
    plotOutput("yield_curve", height = "500px")
  ),
  
  # Additional information card
  card(
    card_header("Model Parameters"),
    tableOutput("parameters_table")
  ),
  col_widths = c(6,6),
  row_heights = c(2,2)
)
)

# Define server logic
server <- function(input, output, session) {

# Calculate Beverton-Holt recruitment (relative form)
  beverton_holt_relative <- function(S_rel, h) {
    # Beverton-Holt stock recruitment relationship in relative form
    # S_rel = S/S0, returns R/R0
    numerator <- 4 * h * S_rel
    denominator <- (1 - h) + S_rel * (5 * h - 1)
    R_rel <- numerator / denominator
    return(R_rel)
  }
    
# Calculate selectivity-at-age
  calc_selectivity <- function(age, a50, a95) {
    slope <- log(19) / (a95 - a50)  # Slope for logistic selectivity
    selectivity <- 1 / (1 + exp(-slope * (age - a50)))
    return(selectivity)
  }
  
# Calculate spawning biomass per recruit
  calc_spawning_biomass <- function(F_rate, M, ages, selectivity, maturity, weight_at_age) {
    # Survival to each age
    Z <- F_rate * selectivity + M  # Total mortality
    survival <- exp(-cumsum(Z))
    survival <- c(1, survival[-length(survival)])  # Add age-0 survival
    
    # Spawning biomass per recruit
    spawning_biomass <- sum(survival * maturity * weight_at_age)
    return(spawning_biomass)
  }
  
# Calculate yield per recruit
  calc_yield <- function(F_rate, M, ages, selectivity, weight_at_age) {
    Z <- F_rate * selectivity + M
    
    # Calculate numbers at age using Baranov catch equation
    survival <- exp(-cumsum(Z))
    survival <- c(1, survival[-length(survival)])
    
    # Yield calculation
    catch_at_age <- (F_rate * selectivity / Z) * (1 - exp(-Z)) * survival
    catch_at_age[Z == 0] <- 0  # Handle division by zero
    
    yield <- sum(catch_at_age * weight_at_age)
    return(yield)
  }

  output$maturity.in<-renderUI({  
    fluidRow(
      column(width = 6, numericInput("A50", 
                                     "50% Maturity:", 
                                     min = 1, 
                                     max = 1000, 
                                     value = round((5.4/input$natural_mortality)*0.1,0), 
                                     step = 0.01)),
      column(width = 6,
             numericInput("A95", 
                          "95% Maturity:", 
                          min = 2, 
                          max = 15, 
                          value = round((5.4/input$natural_mortality)*0.2,0), 
                          step = 0.01)))
  })
  
  output$selectivity.in<-renderUI({  
    fluidRow(
      column(width = 6, 
             numericInput("sel_50", 
                          "50% Sel:", 
                          min = 1, 
                          max = 10, 
                          value = round((5.4/input$natural_mortality)*0.1,0), 
                          step = 0.01)),
      column(width = 6,
             numericInput("sel_95", 
                          "95% Sel:", 
                          min = 2, 
                          max = 15, 
                          value = round((5.4/input$natural_mortality)*0.2,0), 
                          step = 0.01)))  
    })
  
  # Create reactive data
  sr_data <- reactive({
    # Create range of relative spawning biomass values (S/S0)
    S_rel_values <- seq(0, 1, length.out = 100)
    
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
      xlim(0, 1.1) +
      ylim(0, max(data$Relative_Recruitment) * 1.1)
  })
  
    
# Reactive population calculation
  yield_data <- reactive({
    ages <- 1:(5.4/input$natural_mortality)
    
    # Selectivity-at-age (logistic)
    selectivity <- calc_selectivity(ages, input$sel_50, input$sel_95)
    
    # Maturity-at-age (assume knife-edge at age 2 for simplicity)
    #maturity <- ifelse(ages >= 2, 1, 0)
    maturity <- 1 / (1 + exp(-log(19) * (ages - input$A50) / (input$A95 - input$A50)))
    
    # Weight-at-age (assume linear growth for simplicity)
    weight_at_age <- ages * 0.1  # kg
    
    #browser()
    
    # Range of fishing mortality rates
    F_rates <- c(seq(0, 0.5, by = 0.0001),seq(0.501, 1, by = 0.001),seq(1.01, 5, by = 0.01),seq(5.1, 10, by = 0.1))
    
    # Calculate metrics for each F rate
    results <- data.frame(
      F_rate = F_rates,
      spawning_biomass = numeric(length(F_rates)),
      yield = numeric(length(F_rates)),
      rel_spawning_biomass = numeric(length(F_rates))
    )
    
    # Virgin spawning biomass (F = 0)
    virgin_sb <- calc_spawning_biomass(0, input$natural_mortality, ages, 
                                       selectivity, maturity, weight_at_age)
    
    for(i in seq_along(F_rates)) {
      sb <- calc_spawning_biomass(F_rates[i], input$natural_mortality, ages, 
                                  selectivity, maturity, weight_at_age)
      yld <- calc_yield(F_rates[i], input$natural_mortality, ages, 
                        selectivity, weight_at_age)
      
      results$spawning_biomass[i] <- sb
      results$yield[i] <- yld
      results$rel_spawning_biomass[i] <- sb / virgin_sb
    }
    
    # Apply steepness effect (Beverton-Holt recruitment)
    # R = (4 * h * R0 * SSB) / (SSB0 * (1 - h) + SSB * (5 * h - 1))
    h <- input$steepness
    R0 <- 1000  # Unfished recruitment (arbitrary units)
    
    recruitment_multiplier <- (4 * h * results$rel_spawning_biomass) / 
      ((1 - h) + results$rel_spawning_biomass * (5 * h - 1))
    
    # Adjust yield by recruitment
    results$adjusted_yield <- results$yield * recruitment_multiplier
    
    return(results)
  })
  
  # Yield curve plot
  output$yield_curve <- renderPlot({
    data <- yield_data()
    
    # Find MSY point
    msy_point <- data[which.max(data$adjusted_yield), ]
    pgy_points<- data[round(data$adjusted_yield,4)==round(input$PGY*msy_point$adjusted_yield,4),]
      
    ggplot(data, aes(x = rel_spawning_biomass, y = adjusted_yield)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(data = msy_point, 
                 aes(x = rel_spawning_biomass, y = adjusted_yield),
                 color = "red", size = 3) +
      geom_vline(xintercept = msy_point$rel_spawning_biomass, 
                 color = "red", linetype = "dashed", alpha = 0.7) +
      geom_vline(xintercept = max(pgy_points$rel_spawning_biomass), 
                 color = "purple", linetype = "dashed", alpha = 0.7) +
      geom_vline(xintercept = min(pgy_points$rel_spawning_biomass), 
                 color = "purple", linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = input$PGY*msy_point$adjusted_yield, 
                 color = "purple", linetype = "dashed", alpha = 0.7)+
      geom_point(data = pgy_points, 
                 aes(x = rel_spawning_biomass, y = adjusted_yield),
                 color = "purple", size = 3) +
      labs(
        title = "Fishery Yield Curve",
        subtitle = paste("Steepness =", input$steepness, 
                         "| MSY at SSB/SSB₀ =", 
                         round(msy_point$rel_spawning_biomass, 3)),
        x = "Relative Spawning Stock Biomass (SSB/SSB₀)",
        y = "Yield (arbitrary units)",
        caption = "Red point shows Maximum Sustainable Yield (MSY)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      ) +
      scale_x_continuous(limits = c(0, 1), labels = scales::percent_format()) +
      scale_y_continuous(labels = scales::comma_format())
  })
  
  # Parameters table
  output$parameters_table <- renderTable({
    data <- yield_data()
    msy_point <- data[which.max(data$adjusted_yield), ]
    pgy_points<- data[round(data$adjusted_yield,4)==round(input$PGY*msy_point$adjusted_yield,4),]
    
    params <- data.frame(
      Parameter = c("Steepness (h)", 
                    "Natural Mortality (M)", 
                    "Maximum Age",
                    "Age at 50% Maturity", 
                    "Age at  95% Maturity",
                    "Age at 50% Selectivity", 
                    "Age at  95% Selectivity",
                    "MSY SSB/SSB₀",
                    "F at MSY",
                    "Pretty Good Yield %",
                    "High relative SB at Pretty Good Yield",
                    "Low relative SB at Pretty Good Yield"),
      Value = c(input$steepness,
                input$natural_mortality,
                5.4/input$natural_mortality,
                input$A50,
                input$A95,
                input$sel_50,
                input$sel_95,
                round(msy_point$rel_spawning_biomass, 3),
                round(msy_point$F_rate, 3),
                input$PGY,
                max(pgy_points$rel_spawning_biomass),
                min(pgy_points$rel_spawning_biomass))
    )
    
    return(params)
  }, striped = TRUE, hover = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)

