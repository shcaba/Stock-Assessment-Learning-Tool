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

# Define server logic
server <- function(input, output, session) {
  
  # Reactive function to run the fishery model
  model_results <- eventReactive(input$run_model, {
    
    # Initialize parameters
    max_age <- input$max_age
    years <- input$years
    ages <- 1:max_age
    
    # Initialize population matrix (years x ages)
    N <- matrix(0, nrow = years, ncol = max_age)
    
    # Von Bertalanffy growth
    length_at_age <- input$linf * (1 - exp(-input$k * (ages - input$t0)))
    
    # Weight-length relationship (assuming W = aL^b with a=0.01, b=3)
    weight_at_age <- 0.01 * length_at_age^3
    
    # Logistic selectivity function
    selectivity <- 1 / (1 + exp(-input$selectivity_slope * (ages - input$selectivity_a50)))
    
    # Maturity (assuming similar to selectivity but earlier)
    maturity_a50 <- max(1, input$selectivity_a50 - 1)
    maturity <- 1 / (1 + exp(-2 * (ages - maturity_a50)))
    
    # Fishing mortality by age
    F_age <- input$fishing_mort * selectivity
    
    # Total mortality by age
    Z_age <- input$natural_m + F_age
    
    # Survival by age
    S_age <- exp(-Z_age)
    
    # Initial population (equilibrium age structure)
    N[1, 1] <- input$r0
    for(a in 2:max_age) {
      N[1, a] <- N[1, a-1] * exp(-input$natural_m)
    }
    N[1, max_age] <- N[1, max_age] / (1 - exp(-input$natural_m))  # Plus group
    
    # Population dynamics over time
    for(t in 2:years) {
      # Spawning biomass
      SSB <- sum(N[t-1, ] * weight_at_age * maturity)
      
      # Beverton-Holt recruitment
      alpha <- 4 * input$steepness * input$r0 / ((1 - input$steepness) * sum(weight_at_age * maturity))
      beta <- (5 * input$steepness - 1) / ((1 - input$steepness) * sum(weight_at_age * maturity))
      
      N[t, 1] <- alpha * SSB / (1 + beta * SSB)
      
      # Aging and mortality
      for(a in 2:max_age) {
        if(a < max_age) {
          N[t, a] <- N[t-1, a-1] * S_age[a-1]
        } else {
          # Plus group
          N[t, a] <- N[t-1, a-1] * S_age[a-1] + N[t-1, a] * S_age[a]
        }
      }
    }
    
    # Calculate derived quantities
    biomass <- rowSums(N * matrix(rep(weight_at_age, years), nrow = years, byrow = TRUE))
    ssb <- rowSums(N * matrix(rep(weight_at_age * maturity, years), nrow = years, byrow = TRUE))
    catch_numbers <- N * matrix(rep(F_age / Z_age * (1 - exp(-Z_age)), years), nrow = years, byrow = TRUE)
    catch_biomass <- rowSums(catch_numbers * matrix(rep(weight_at_age, years), nrow = years, byrow = TRUE))
    
    list(
      N = N,
      biomass = biomass,
      ssb = ssb,
      catch_biomass = catch_biomass,
      ages = ages,
      length_at_age = length_at_age,
      weight_at_age = weight_at_age,
      selectivity = selectivity,
      maturity = maturity,
      years = 1:years
    )
  })
  
  # Population dynamics plot
  output$population_plot <- renderPlotly({
    results <- model_results()
    
    plot_data <- data.frame(
      Year = results$years,
      `Total Biomass` = results$biomass,
      `Spawning Biomass` = results$ssb,
      `Catch` = results$catch_biomass
    )
    
    p <- plot_ly(plot_data, x = ~Year) %>%
      add_lines(y = ~Total.Biomass, name = "Total Biomass", line = list(color = "blue")) %>%
      add_lines(y = ~Spawning.Biomass, name = "Spawning Biomass", line = list(color = "red")) %>%
      add_lines(y = ~Catch, name = "Catch", line = list(color = "green")) %>%
      layout(
        title = "Population Dynamics",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Biomass/Catch"),
        hovermode = "x unified"
      )
    
    p
  })
  
  # Age structure plot
  output$age_structure_plot <- renderPlotly({
    results <- model_results()
    
    final_year_N <- results$N[nrow(results$N), ]
    
    plot_data <- data.frame(
      Age = results$ages,
      Numbers = final_year_N,
      Biomass = final_year_N * results$weight_at_age
    )
    
    p <- plot_ly(plot_data, x = ~Age) %>%
      add_bars(y = ~Numbers, name = "Numbers", yaxis = "y", opacity = 0.7) %>%
      add_lines(y = ~Biomass, name = "Biomass", yaxis = "y2", line = list(color = "red")) %>%
      layout(
        title = "Age Structure (Final Year)",
        xaxis = list(title = "Age"),
        yaxis = list(title = "Numbers", side = "left"),
        yaxis2 = list(title = "Biomass", side = "right", overlaying = "y"),
        hovermode = "x unified"
      )
    
    p
  })
  
  # Selectivity and maturity plot
  output$selectivity_plot <- renderPlotly({
    results <- model_results()
    
    plot_data <- data.frame(
      Age = results$ages,
      Length = results$length_at_age,
      Selectivity = results$selectivity,
      Maturity = results$maturity
    )
    
    p <- plot_ly(plot_data, x = ~Age) %>%
      add_lines(y = ~Selectivity, name = "Selectivity", line = list(color = "blue")) %>%
      add_lines(y = ~Maturity, name = "Maturity", line = list(color = "red")) %>%
      add_lines(y = ~Length/max(Length), name = "Relative Length", 
                line = list(color = "green", dash = "dash")) %>%
      layout(
        title = "Selectivity, Maturity, and Growth",
        xaxis = list(title = "Age"),
        yaxis = list(title = "Proportion / Relative Length"),
        hovermode = "x unified"
      )
    
    p
  })
  
  # Run model on startup
  observe({
    shinyjs::click("run_model")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
