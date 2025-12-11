library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Define UI
ui <- page_sidebar(
  title = "Demonstrating Uncertainty: Bias and Precision",
  
  sidebar = sidebar(
    h4("Simulation Parameters"),
    
    sliderInput("true_value", 
                "True Value:", 
                min = 0, max = 100, value = 50, step = 1),
    
    sliderInput("bias", 
                "Bias (systematic error):", 
                min = -20, max = 20, value = 0, step = 1),
    
    sliderInput("precision", 
                "Precision (1/variance):", 
                min = 0.1, max = 5, value = 1, step = 0.1),
    
    sliderInput("n_samples", 
                "Number of Samples:", 
                min = 10, max = 500, value = 100, step = 10),
    
    actionButton("resample", "Generate New Sample", 
                 class = "btn-primary"),
    
    hr(),
    
    h5("Predefined Scenarios:"),
    actionButton("scenario1", "High Precision, No Bias", 
                 class = "btn-outline-success btn-sm"),
    actionButton("scenario2", "Low Precision, No Bias", 
                 class = "btn-outline-warning btn-sm"),
    actionButton("scenario3", "High Precision, High Bias", 
                 class = "btn-outline-danger btn-sm"),
    actionButton("scenario4", "Low Precision, High Bias", 
                 class = "btn-outline-dark btn-sm")
  ),
  
  # Main content
  layout_columns(
    col_widths = c(12, 6, 6),
    
    # Top row - main visualization
    card(
      card_header("Distribution of Measurements"),
      plotOutput("main_plot", height = "400px")
    ),
    
    # Bottom left - statistics
    card(
      card_header("Statistical Summary"),
      tableOutput("statistics_table")
    ),
    
    # Bottom right - explanation
    card(
      card_header("Interpretation"),
      uiOutput("interpretation")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store data
  values <- reactiveValues(
    data = NULL,
    seed = 123
  )
  
  # Generate data based on current parameters
  generate_data <- reactive({
    # Use seed for reproducibility until user clicks resample
    set.seed(values$seed)
    
    # Calculate standard deviation from precision
    sd_value <- 1 / sqrt(input$precision)
    
    # Generate measurements with bias and precision
    measurements <- rnorm(input$n_samples, 
                          mean = input$true_value + input$bias, 
                          sd = sd_value)
    
    data.frame(
      measurement = measurements,
      true_value = input$true_value,
      bias = input$bias,
      precision = input$precision
    )
  })
  
  # Update data when parameters change or resample is clicked
  observe({
    values$data <- generate_data()
  })
  
  # Resample button
  observeEvent(input$resample, {
    values$seed <- sample(1:10000, 1)
    values$data <- generate_data()
  })
  
  # Predefined scenarios
  observeEvent(input$scenario1, {
    updateSliderInput(session, "bias", value = 0)
    updateSliderInput(session, "precision", value = 3)
    values$seed <- sample(1:10000, 1)
  })
  
  observeEvent(input$scenario2, {
    updateSliderInput(session, "bias", value = 0)
    updateSliderInput(session, "precision", value = 0.3)
    values$seed <- sample(1:10000, 1)
  })
  
  observeEvent(input$scenario3, {
    updateSliderInput(session, "bias", value = 15)
    updateSliderInput(session, "precision", value = 3)
    values$seed <- sample(1:10000, 1)
  })
  
  observeEvent(input$scenario4, {
    updateSliderInput(session, "bias", value = 15)
    updateSliderInput(session, "precision", value = 0.3)
    values$seed <- sample(1:10000, 1)
  })
  
  # Main plot
  output$main_plot <- renderPlot({
    req(values$data)
    
    data <- values$data
    
    # Calculate mean and confidence interval
    mean_measurement <- mean(data$measurement)
    se_measurement <- sd(data$measurement) / sqrt(nrow(data))
    ci_lower <- mean_measurement - 1.96 * se_measurement
    ci_upper <- mean_measurement + 1.96 * se_measurement
    
    # Create the plot
    p <- ggplot(data, aes(x = measurement)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 30, alpha = 0.7, fill = "steelblue", 
                     color = "white") +
      geom_density(alpha = 0.3, fill = "steelblue") +
      
      # Add vertical lines for true value, sample mean, and CI
      geom_vline(aes(xintercept = true_value), 
                 color = "green", size = 2, linetype = "solid",
                 alpha = 0.8) +
      geom_vline(aes(xintercept = mean_measurement), 
                 color = "red", size = 2, linetype = "dashed",
                 alpha = 0.8) +
      geom_vline(aes(xintercept = ci_lower), 
                 color = "red", size = 1, linetype = "dotted",
                 alpha = 0.6) +
      geom_vline(aes(xintercept = ci_upper), 
                 color = "red", size = 1, linetype = "dotted",
                 alpha = 0.6) +
      
      # Add labels and annotations
      annotate("text", x = input$true_value, y = Inf, 
               label = "True Value", vjust = 2, hjust = -0.1,
               color = "green", fontface = "bold") +
      annotate("text", x = mean_measurement, y = Inf, 
               label = "Sample Mean", vjust = 2, hjust = 1.1,
               color = "red", fontface = "bold") +
      
      labs(
        title = paste("Distribution of", input$n_samples, "measurements"),
        subtitle = paste("Bias =", input$bias, "| Precision =", round(input$precision, 2)),
        x = "Measurement Value",
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom"
      )
    
    print(p)
  })
  
  # Statistics table
  output$statistics_table <- renderTable({
    req(values$data)
    
    data <- values$data
    
    # Calculate statistics
    sample_mean <- mean(data$measurement)
    sample_sd <- sd(data$measurement)
    bias_estimate <- sample_mean - input$true_value
    rmse <- sqrt(mean((data$measurement - input$true_value)^2))
    
    # Create summary table
    stats <- data.frame(
      Metric = c("True Value", "Sample Mean", "Sample SD", 
                 "Estimated Bias", "RMSE", "Theoretical SD"),
      Value = c(
        round(input$true_value, 2),
        round(sample_mean, 2),
        round(sample_sd, 2),
        round(bias_estimate, 2),
        round(rmse, 2),
        round(1/sqrt(input$precision), 2)
      ),
      stringsAsFactors = FALSE
    )
    
    stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Interpretation text
  output$interpretation <- renderUI({
    req(values$data)
    
    # Determine bias level
    bias_level <- if(abs(input$bias) < 2) {
      "low"
    } else if(abs(input$bias) < 10) {
      "moderate" 
    } else {
      "high"
    }
    
    # Determine precision level
    precision_level <- if(input$precision > 2) {
      "high"
    } else if(input$precision > 0.5) {
      "moderate"
    } else {
      "low"
    }
    
    # Generate interpretation
    interpretation_text <- tagList(
      h6("Current Scenario:", class = "text-primary"),
      p(paste("This scenario shows", precision_level, "precision and", 
              bias_level, "bias.")),
      
      h6("Key Concepts:", class = "text-primary"),
      tags$ul(
        tags$li(strong("Bias:"), "Systematic error that shifts all measurements away from the true value"),
        tags$li(strong("Precision:"), "How tightly clustered measurements are (opposite of variance)"),
        tags$li(strong("Accuracy:"), "How close measurements are to the true value (affected by both bias and precision)")
      ),
      
      if(abs(input$bias) > 5 && input$precision > 1.5) {
        div(class = "alert alert-warning", 
            "High precision but high bias: Measurements are consistent but systematically wrong!")
      } else if(abs(input$bias) < 2 && input$precision < 0.5) {
        div(class = "alert alert-info", 
            "Low bias but low precision: Measurements are unbiased but highly variable.")
      } else if(abs(input$bias) < 2 && input$precision > 1.5) {
        div(class = "alert alert-success", 
            "Low bias and high precision: Ideal scenario with accurate and precise measurements!")
      } else {
        div(class = "alert alert-danger", 
            "High bias and low precision: Worst case with both systematic error and high variability.")
      }
    )
    
    interpretation_text
  })
}

# Run the application
shinyApp(ui = ui, server = server)
