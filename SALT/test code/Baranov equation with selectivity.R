#Baranov equation with selectivity

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Define UI
ui <- page_sidebar(
  title = "Baranov Catch Equation with Selectivity",
  
  sidebar = sidebar(
    h4("Parameters"),
    
    # Population parameters
    numericInput("N0", "Initial Population (N0):", 
                 value = 1000, min = 1, max = 10000, step = 100),
    
    numericInput("M", "Natural Mortality (M):", 
                 value = 0.2, min = 0.01, max = 2, step = 0.01),
    
    numericInput("F", "Fishing Mortality (F):", 
                 value = 0.3, min = 0, max = 2, step = 0.01),
    
    # Selectivity parameters
    h4("Selectivity Parameters"),
    
    selectInput("selectivity_type", "Selectivity Type:",
                choices = list("Logistic" = "logistic",
                               "Knife-edge" = "knife_edge",
                               "Dome-shaped" = "dome"),
                selected = "logistic"),
    
    conditionalPanel(
      condition = "input.selectivity_type == 'logistic'",
      numericInput("L50", "L50 (length at 50% selectivity):", 
                   value = 25, min = 1, max = 100, step = 1),
      numericInput("L95", "L95 (length at 95% selectivity):", 
                   value = 35, min = 1, max = 100, step = 1)
    ),
    
    conditionalPanel(
      condition = "input.selectivity_type == 'knife_edge'",
      numericInput("Lc", "Minimum capture length (Lc):", 
                   value = 30, min = 1, max = 100, step = 1)
    ),
    
    conditionalPanel(
      condition = "input.selectivity_type == 'dome'",
      numericInput("L50_dome", "L50:", 
                   value = 25, min = 1, max = 100, step = 1),
      numericInput("L95_dome", "L95:", 
                   value = 35, min = 1, max = 100, step = 1),
      numericInput("Lmax_sel", "Length at maximum selectivity:", 
                   value = 40, min = 1, max = 100, step = 1),
      numericInput("decline_rate", "Decline rate after Lmax:", 
                   value = 0.05, min = 0.01, max = 0.2, step = 0.01)
    ),
    
    # Time parameters
    numericInput("time_steps", "Time Steps:", 
                 value = 12, min = 1, max = 50, step = 1)
  ),
  
  # Main panel with outputs
  layout_columns(
    card(
      card_header("Population Dynamics"),
      plotOutput("population_plot")
    ),
    
    card(
      card_header("Selectivity Curve"),
      plotOutput("selectivity_plot")
    ),
    
    col_widths = c(6, 6)
  ),
  
  layout_columns(
    card(
      card_header("Catch and Mortality"),
      plotOutput("catch_plot")
    ),
    
    card(
      card_header("Summary Results"),
      tableOutput("summary_table")
    ),
    
    col_widths = c(8, 4)
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Calculate selectivity based on length
  calculate_selectivity <- reactive({
    lengths <- seq(1, 80, by = 1)
    
    if (input$selectivity_type == "logistic") {
      # Logistic selectivity
      selectivity <- 1 / (1 + exp(-log(19) * (lengths - input$L50) / (input$L95 - input$L50)))
    } else if (input$selectivity_type == "knife_edge") {
      # Knife-edge selectivity
      selectivity <- ifelse(lengths >= input$Lc, 1, 0)
    } else if (input$selectivity_type == "dome") {
      # Dome-shaped selectivity
      sel1 <- 1 / (1 + exp(-log(19) * (lengths - input$L50_dome) / (input$L95_dome - input$L50_dome)))
      sel2 <- ifelse(lengths > input$Lmax_sel, 
                     exp(-input$decline_rate * (lengths - input$Lmax_sel)), 1)
      selectivity <- sel1 * sel2
    }
    
    data.frame(length = lengths, selectivity = selectivity)
  })
  
  # Apply Baranov catch equation
  baranov_results <- reactive({
    # Assume length-based population structure (simplified)
    lengths <- seq(10, 70, by = 5)
    n_lengths <- length(lengths)
    
    # Initial population by length (simplified normal distribution)
    N_init <- dnorm(lengths, mean = 40, sd = 10) * input$N0
    
    # Get selectivity for these lengths
    sel_data <- calculate_selectivity()
    selectivity <- approx(sel_data$length, sel_data$selectivity, lengths)$y
    selectivity[is.na(selectivity)] <- 0
    
    # Time series results
    time_steps <- input$time_steps
    results <- data.frame()
    
    for (t in 1:time_steps) {
      if (t == 1) {
        N_t <- N_init
      } else {
        # Apply Baranov equation: N(t+1) = N(t) * exp(-(F*sel + M))
        Z <- input$F * selectivity + input$M  # Total mortality
        N_t <- N_t * exp(-Z)
      }
      
      # Calculate catch using Baranov equation
      # C = (F * sel / Z) * N * (1 - exp(-Z))
      Z <- input$F * selectivity + input$M
      Z[Z == 0] <- 1e-10  # Avoid division by zero
      catch <- (input$F * selectivity / Z) * N_t * (1 - exp(-Z))
      
      # Store results
      temp_df <- data.frame(
        time = t,
        length = lengths,
        population = N_t,
        catch = catch,
        selectivity = selectivity,
        Z = Z
      )
      results <- rbind(results, temp_df)
    }
    
    results
  })
  
  # Population dynamics plot
  output$population_plot <- renderPlot({
    results <- baranov_results()
    
    pop_summary <- results %>%
      group_by(time) %>%
      summarise(total_pop = sum(population), .groups = 'drop')
    
    ggplot(pop_summary, aes(x = time, y = total_pop)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "blue", size = 2) +
      labs(x = "Time Step", y = "Total Population", 
           title = "Population Decline Over Time") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Selectivity curve plot
  output$selectivity_plot <- renderPlot({
    sel_data <- calculate_selectivity()
    
    ggplot(sel_data, aes(x = length, y = selectivity)) +
      geom_line(color = "red", size = 1) +
      labs(x = "Length", y = "Selectivity", 
           title = "Selectivity Curve") +
      ylim(0, 1) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Catch plot
  output$catch_plot <- renderPlot({
    results <- baranov_results()
    
    catch_summary <- results %>%
      group_by(time) %>%
      summarise(total_catch = sum(catch), .groups = 'drop')
    
    ggplot(catch_summary, aes(x = time, y = total_catch)) +
      geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
      labs(x = "Time Step", y = "Total Catch", 
           title = "Catch Over Time") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Summary table
  output$summary_table <- renderTable({
    results <- baranov_results()
    
    summary_stats <- results %>%
      group_by(time) %>%
      summarise(
        Population = round(sum(population), 0),
        Catch = round(sum(catch), 0),
        .groups = 'drop'
      ) %>%
      slice_tail(n = 5)  # Show last 5 time steps
    
    colnames(summary_stats) <- c("Time", "Population", "Catch")
    summary_stats
  }, digits = 0)
}

# Run the application
shinyApp(ui = ui, server = server)
