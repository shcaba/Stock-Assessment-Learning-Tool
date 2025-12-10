library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Define UI
ui <- page_sidebar(
  title = "Fishery Yield Curve Analysis",
  
  sidebar = sidebar(
    width = 300,
    
    # Steepness parameter
    sliderInput("steepness", 
                "Steepness (h):", 
                min = 0.3, 
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

        # Selectivity parameters
    h4("Selectivity Parameters"),
    
    sliderInput("sel_50", 
                "Selectivity at 50% (A50):", 
                min = 1, 
                max = 10, 
                value = 3, 
                step = 0.5,
                width = "100%"),
    
    sliderInput("sel_95", 
                "Selectivity at 95% (A95):", 
                min = 2, 
                max = 15, 
                value = 5, 
                step = 0.5,
                width = "100%"),
    
    
    # Maximum age
#    numericInput("max_age", 
#                 "Maximum Age:", 
#                 value = 20, 
#                 min = 10, 
#                 max = 50, 
#                 step = 1),
    
    hr(),
    
    p("This app shows the relationship between relative spawning stock biomass and yield per recruit, 
      incorporating steepness and selectivity parameters commonly used in fisheries stock assessments.")
  ),
  
  # Main panel with yield curve plot
  card(
    card_header("Yield Per Recruit Curve"),
    plotOutput("yield_curve", height = "500px")
  ),
  
  # Additional information card
  card(
    card_header("Model Parameters"),
    tableOutput("parameters_table")
  )
)

# Define server logic
server <- function(input, output, session) {
  
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
  
  # Reactive data calculation
  yield_data <- reactive({
    ages <- 1:(5.4/input$natural_mortality)
    
    # Selectivity-at-age (logistic)
    selectivity <- calc_selectivity(ages, input$sel_50, input$sel_95)
    
    # Maturity-at-age (assume knife-edge at age 2 for simplicity)
    maturity <- ifelse(ages >= 2, 1, 0)
    
    # Weight-at-age (assume linear growth for simplicity)
    weight_at_age <- ages * 0.1  # kg
    
    # Range of fishing mortality rates
    F_rates <- seq(0, 100, by = 0.05)
    
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
    
    ggplot(data, aes(x = rel_spawning_biomass, y = adjusted_yield)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(data = msy_point, 
                 aes(x = rel_spawning_biomass, y = adjusted_yield),
                 color = "red", size = 3) +
      geom_vline(xintercept = msy_point$rel_spawning_biomass, 
                 color = "red", linetype = "dashed", alpha = 0.7) +
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
    
    params <- data.frame(
      Parameter = c("Steepness (h)", 
                    "Natural Mortality (M)", 
                    "Selectivity A50", 
                    "Selectivity A95",
                    "MSY SSB/SSB₀",
                    "F at MSY"),
      Value = c(input$steepness,
                input$natural_mortality,
                input$sel_50,
                input$sel_95,
                round(msy_point$rel_spawning_biomass, 3),
                round(msy_point$F_rate, 3))
    )
    
    return(params)
  }, striped = TRUE, hover = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)

