library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)

# Helper functions for fish population simulation
generate_unfished_population <- function(n_fish = 1000, max_age = 20) {
  # Generate ages following exponential decay (natural mortality)
  ages <- rexp(n_fish, rate = 0.2)
  ages <- pmin(ages, max_age)
  
  # Von Bertalanffy growth model: L(t) = L_inf * (1 - exp(-K * (t - t0)))
  L_inf <- 80  # Asymptotic length
  K <- 0.15    # Growth coefficient
  t0 <- -1     # Theoretical age at length zero
  
  lengths <- L_inf * (1 - exp(-K * (ages - t0)))
  
  data.frame(
    age = ages,
    length = lengths,
    population = "Unfished"
  )
}

calculate_selectivity <- function(lengths, ages, selectivity_type, params) {
  switch(selectivity_type,
         "logistic_length" = {
           L50 <- params$L50
           slope <- params$slope
           1 / (1 + exp(-slope * (lengths - L50)))
         },
         "logistic_age" = {
           A50 <- params$A50
           slope <- params$slope
           1 / (1 + exp(-slope * (ages - A50)))
         },
         "dome_shaped" = {
           L50 <- params$L50
           L95 <- params$L95
           dome_peak <- params$dome_peak
           
           ascending <- 1 / (1 + exp(-2 * log(19) * (lengths - L50) / (L95 - L50)))
           descending <- 1 / (1 + exp(2 * log(19) * (lengths - dome_peak) / (dome_peak - L95)))
           ascending * descending
         },
         "knife_edge" = {
           min_length <- params$min_length
           ifelse(lengths >= min_length, 1, 0)
         }
  )
}

apply_fishing_mortality <- function(population, selectivity, fishing_mortality = 0.3) {
  # Total mortality = natural mortality + fishing mortality * selectivity
  natural_mortality <- 0.2
  total_mortality <- natural_mortality + fishing_mortality * selectivity
  
  # Probability of survival
  survival_prob <- exp(-total_mortality)
  
  # Simulate which fish survive
  survivors <- rbinom(nrow(population), 1, survival_prob) == 1
  
  population[survivors, ]
}

# UI
ui <- page_sidebar(
  title = "Fish Population Selectivity Comparison",
  
  sidebar = sidebar(
    h4("Population Parameters"),
    numericInput("n_fish", "Number of Fish", value = 1000, min = 100, max = 5000, step = 100),
    numericInput("max_age", "Maximum Age", value = 20, min = 10, max = 50),
    
    hr(),
    
    h4("Selectivity Settings"),
    selectInput("selectivity_type", "Selectivity Type",
                choices = list(
                  "Logistic (Length)" = "logistic_length",
                  "Logistic (Age)" = "logistic_age",
                  "Dome-shaped" = "dome_shaped",
                  "Knife-edge" = "knife_edge"
                ),
                selected = "logistic_length"),
    
    # Conditional UI for selectivity parameters
    conditionalPanel(
      condition = "input.selectivity_type == 'logistic_length'",
      numericInput("L50", "L50 (50% selectivity length)", value = 40, min = 10, max = 80),
      numericInput("slope_length", "Slope", value = 0.2, min = 0.05, max = 1, step = 0.05)
    ),
    
    conditionalPanel(
      condition = "input.selectivity_type == 'logistic_age'",
      numericInput("A50", "A50 (50% selectivity age)", value = 5, min = 1, max = 15),
      numericInput("slope_age", "Slope", value = 0.5, min = 0.1, max = 2, step = 0.1)
    ),
    
    conditionalPanel(
      condition = "input.selectivity_type == 'dome_shaped'",
      numericInput("dome_L50", "L50", value = 30, min = 10, max = 60),
      numericInput("dome_L95", "L95", value = 50, min = 20, max = 80),
      numericInput("dome_peak", "Peak Length", value = 70, min = 40, max = 90)
    ),
    
    conditionalPanel(
      condition = "input.selectivity_type == 'knife_edge'",
      numericInput("min_length", "Minimum Length", value = 35, min = 10, max = 70)
    ),
    
    hr(),
    
    numericInput("fishing_mortality", "Fishing Mortality Rate", value = 0.3, min = 0, max = 1, step = 0.05),
    
    hr(),
    
    actionButton("simulate", "Run Simulation", class = "btn-primary")
  ),
  
  # Main panel with outputs
  layout_columns(
    col_widths = c(12, 6, 6),
    
    card(
      card_header("Selectivity Curve"),
      plotlyOutput("selectivity_plot")
    ),
    
    card(
      card_header("Length Composition Comparison"),
      plotlyOutput("length_comparison")
    ),
    
    card(
      card_header("Age Composition Comparison"),
      plotlyOutput("age_comparison")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store simulation results
  simulation_results <- reactiveValues(
    unfished = NULL,
    fished = NULL,
    selectivity_data = NULL
  )
  
  # Run simulation when button is clicked or inputs change
  observeEvent(c(input$simulate, input$selectivity_type), {
    
    # Generate unfished population
    unfished_pop <- generate_unfished_population(input$n_fish, input$max_age)
    
    # Set selectivity parameters based on type
    selectivity_params <- switch(input$selectivity_type,
                                 "logistic_length" = list(L50 = input$L50, slope = input$slope_length),
                                 "logistic_age" = list(A50 = input$A50, slope = input$slope_age),
                                 "dome_shaped" = list(L50 = input$dome_L50, L95 = input$dome_L95, dome_peak = input$dome_peak),
                                 "knife_edge" = list(min_length = input$min_length)
    )
    
    # Calculate selectivity
    selectivity <- calculate_selectivity(
      unfished_pop$length, 
      unfished_pop$age, 
      input$selectivity_type, 
      selectivity_params
    )
    
    # Apply fishing mortality
    fished_pop <- apply_fishing_mortality(unfished_pop, selectivity, input$fishing_mortality)
    fished_pop$population <- "Fished"
    
    # Store results
    simulation_results$unfished <- unfished_pop
    simulation_results$fished <- fished_pop
    
    # Create selectivity data for plotting
    length_range <- seq(0, 100, by = 1)
    age_range <- seq(0, input$max_age, by = 0.1)
    
    if (input$selectivity_type %in% c("logistic_length", "dome_shaped", "knife_edge")) {
      sel_data <- data.frame(
        value = length_range,
        selectivity = calculate_selectivity(length_range, rep(5, length(length_range)), 
                                            input$selectivity_type, selectivity_params),
        type = "Length"
      )
    } else {
      sel_data <- data.frame(
        value = age_range,
        selectivity = calculate_selectivity(rep(50, length(age_range)), age_range, 
                                            input$selectivity_type, selectivity_params),
        type = "Age"
      )
    }
    
    simulation_results$selectivity_data <- sel_data
  })
  
  # Initialize simulation on app start
  observe({
    if (is.null(simulation_results$unfished)) {
      # Trigger initial simulation
      updateActionButton(session, "simulate", label = "Run Simulation")
      # Simulate a click
      session$sendCustomMessage("simulate_click", list())
    }
  })
  
  # Selectivity curve plot
  output$selectivity_plot <- renderPlotly({
    req(simulation_results$selectivity_data)
    
    p <- ggplot(simulation_results$selectivity_data, aes(x = value, y = selectivity)) +
      geom_line(size = 1.2, color = "blue") +
      labs(
        x = paste(simulation_results$selectivity_data$type[1], 
                  ifelse(simulation_results$selectivity_data$type[1] == "Length", "(cm)", "(years)")),
        y = "Selectivity",
        title = paste("Selectivity Curve -", gsub("_", " ", tools::toTitleCase(input$selectivity_type)))
      ) +
      theme_minimal() +
      ylim(0, 1)
    
    ggplotly(p)
  })
  
  # Length composition comparison
  output$length_comparison <- renderPlotly({
    req(simulation_results$unfished, simulation_results$fished)
    
    combined_data <- rbind(simulation_results$unfished, simulation_results$fished)
    
    p <- ggplot(combined_data, aes(x = length, fill = population)) +
      geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
      scale_fill_manual(values = c("Unfished" = "blue", "Fished" = "red")) +
      labs(
        x = "Length (cm)",
        y = "Frequency",
        fill = "Population",
        title = "Length Composition Comparison"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Age composition comparison
  output$age_comparison <- renderPlotly({
    req(simulation_results$unfished, simulation_results$fished)
    
    combined_data <- rbind(simulation_results$unfished, simulation_results$fished)
    
    p <- ggplot(combined_data, aes(x = age, fill = population)) +
      geom_histogram(alpha = 0.7, position = "identity", bins = 20) +
      scale_fill_manual(values = c("Unfished" = "blue", "Fished" = "red")) +
      labs(
        x = "Age (years)",
        y = "Frequency",
        fill = "Population",
        title = "Age Composition Comparison"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
