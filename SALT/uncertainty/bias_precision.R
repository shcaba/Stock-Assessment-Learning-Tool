library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Define UI
ui <- page_sidebar(
  title = "Understanding Uncertainty: Bias and Precision",
  
  sidebar = sidebar(
    h4("Uncertainty is "),
    tags$ul(
      tags$li(strong("Unknown:"), "What we do not know or have trouble measuring."),
      tags$li(strong("Unpredictable:"), "We know it, but it is hard to predict."),
      tags$li(strong("Risky:"), "If it is hard to measure or predict, it increases decision-making risk.")
    ),
    
    # sliderInput("true_value", 
    #             "True Value:", 
    #             min = 25, max = 175, value = 100, step = 1),

    numericInput("n_samples", 
                 "Number of Samples:", 
                 min = 1, max = 1000000, value = 200, step = 1),
    
    numericInput("bias", 
                "Bias (% error of true value):", 
                min = -100, max = 100, value = 0, step = 1),
    
    numericInput("CV", 
                "Precision (coeff. of variation):", 
                min = 0, max = 1, value = 0.1, step = 0.01),
    
   
    actionButton("resample", "Generate New Sample", 
                 class = "btn-primary"),
    
    hr(),
    
    # h5("Predefined Scenarios:"),
    # actionButton("scenario1", "High Precision, No Bias", 
    #              class = "btn-outline-success btn-sm"),
    # actionButton("scenario2", "Low Precision, No Bias", 
    #              class = "btn-outline-warning btn-sm"),
    # actionButton("scenario3", "High Precision, High Bias", 
    #              class = "btn-outline-danger btn-sm"),
    # actionButton("scenario4", "Low Precision, High Bias", 
    #              class = "btn-outline-dark btn-sm")
  ),
  
  # Main content
  layout_columns(
    col_widths = c(8, 4, 12),
    
    # Top row - main visualization
    card(
      card_header("Distribution of Measurements"),
      plotOutput("main_plot", height = "400px")
    ),
    
    # Bottom left - statistics
    card(
      layout_columns(
        col_widths = c(12,12),
        card_header("Statistical Summary"),
        tableOutput("statistics_table"),
        card_header("Interpretation"),
        uiOutput("interp")
      )
    ),
    
    # Bottom right - explanation
      layout_columns(
        col_widths = c(4,4,4),
        card(
          card_header("Key concepts"),
        uiOutput("uncertainty")),
        card(
          card_header("Sources of Uncertainty"),
          uiOutput("sources")),
        card(
          card_header("Estimating Uncertainty"),
        uiOutput("estimation"))
      )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  true_value<-100
  
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
    sd_value <- input$CV*true_value
    
    # Generate measurements with bias and precision
    measurements <- rnorm(input$n_samples, 
                          mean = true_value + (true_value*(input$bias/100)), 
                          sd = sd_value)
    
    data.frame(
      measurement = measurements,
      true_value = true_value,
      bias = input$bias,
      precision = input$CV
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
  
  # # Predefined scenarios
  # observeEvent(input$scenario1, {
  #   updateSliderInput(session, "bias", value = 0)
  #   updateSliderInput(session, "precision", value = 3)
  #   values$seed <- sample(1:10000, 1)
  # })
  # 
  # observeEvent(input$scenario2, {
  #   updateSliderInput(session, "bias", value = 0)
  #   updateSliderInput(session, "precision", value = 0.3)
  #   values$seed <- sample(1:10000, 1)
  # })
  # 
  # observeEvent(input$scenario3, {
  #   updateSliderInput(session, "bias", value = 15)
  #   updateSliderInput(session, "precision", value = 3)
  #   values$seed <- sample(1:10000, 1)
  # })
  # 
  # observeEvent(input$scenario4, {
  #   updateSliderInput(session, "bias", value = 15)
  #   updateSliderInput(session, "precision", value = 0.3)
  #   values$seed <- sample(1:10000, 1)
  # })
  
  # Main plot
  output$main_plot <- renderPlot({
    req(values$data)
    
    data <- values$data
    
    # Calculate mean and confidence interval
    mean_measurement <- mean(data$measurement)
    #se_measurement <- sd(data$measurement) / sqrt(nrow(data))
    ci_lower <- mean_measurement - 1.96 * sd(data$measurement)
    ci_upper <- mean_measurement + 1.96 * sd(data$measurement)
    
    # Create the plot
    p <- ggplot(data, aes(x = measurement)) +
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 100, alpha = 0.7, fill = "steelblue", 
                     color = "white") +
#      geom_density(alpha = 0.3, fill = "steelblue") +
      xlim(0,200)+
      # Add vertical lines for true value, sample mean, and CI
      geom_vline(aes(xintercept = true_value), 
                 color = "black", size = 2, linetype = "solid",
                 alpha = 0.8) +
      geom_vline(aes(xintercept = mean_measurement), 
                 color = "orange", size = 2, linetype = "dashed",
                 alpha = 0.8) +
      geom_vline(aes(xintercept = ci_lower), 
                 color = "orange", size = 1, linetype = "dotted",
                 alpha = 0.6) +
      geom_vline(aes(xintercept = ci_upper), 
                 color = "orange", size = 1, linetype = "dotted",
                 alpha = 0.6) +
      
      # Add labels and annotations
      annotate("text", x = true_value, y = Inf, 
               label = "True Value", vjust = 2, hjust = -0.1,
               color = "black", fontface = "bold") +
      annotate("text", x = mean_measurement, y = Inf, 
               label = "Sample Mean", vjust = 2, hjust = 1.1,
               color = "orange", fontface = "bold") +
      
      labs(
        title = paste("Distribution of", input$n_samples, "measurements"),
        subtitle = paste("Bias =", input$bias, "%| Precision =", round(input$CV, 2)),
        x = "Sampled Metric (e.g., lengths, abundance, etc.)",
        y = "Density"
      ) +
      #theme_minimal() +
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
    bias_estimate <- sample_mean - true_value
#    rmse <- sqrt(mean((data$measurement - true_value)^2))
    sd_value <- input$CV*true_value
    # Create summary table
    stats <- data.frame(
      Metric = c("True Value", "Sample Mean", "% Estimated Bias", 
                 "Sample SD", "Theoretical SD"),
      Value = c(
        round(true_value, 2),
        round(sample_mean, 2),
        round(bias_estimate, 2),
        round(sd_value, 2),
        round(sample_sd, 2)
        #round(rmse, 2),
      ),
      stringsAsFactors = FALSE
    )
    
    if(abs(input$bias) > 50 && input$CV < 0.2) {
      div(class = "alert alert-warning", 
          "High precision but high bias: Measurements are consistent but systematically wrong!")
    } else if(abs(input$bias) < 20 && input$CV > 0.5) {
      div(class = "alert alert-info", 
          "Low bias but low precision: Measurements are unbiased but highly variable.")
    } else if(abs(input$bias) < 20 && input$CV < 0.2) {
      div(class = "alert alert-success", 
          "Low bias and high precision: Ideal scenario with accurate and precise measurements!")
    } else {
      div(class = "alert alert-danger", 
          "High bias and low precision: Worst case with both systematic error and high variability.")
    }
    
    stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  
  
  output$interp <- renderUI({

      if(abs(input$bias) > 50 && input$CV < 0.2) {
        div(class = "alert alert-warning", 
            "High precision but high bias: Measurements are consistent but systematically wrong!")
      } else if(abs(input$bias) < 20 && input$CV > 0.5) {
        div(class = "alert alert-info", 
            "Low bias but low precision: Measurements are unbiased but highly variable.")
      } else if(abs(input$bias) < 20 && input$CV < 0.2) {
        div(class = "alert alert-success", 
            "Low bias and high precision: Ideal scenario with accurate and precise measurements!")
      } else {
        div(class = "alert alert-danger", 
            "High bias and low precision: Worst case with both systematic error and high variability.")
      }
    
  })
  
  
  
  
  # Uncertainty text
  output$uncertainty <- renderUI({

    # Generate interpretation
    uncertainty_text <- tagList(

      h5("Describing Uncertainty:", class = "text-primary"),
      tags$ul(
        tags$li(strong("Bias:"), "Systematic error that shifts all measurements away from the true value"),
        tags$li(strong("Precision:"), "How tightly clustered measurements are (opposite of variance)"),
      ),

      h5("Types of uncertainy:", class = "text-primary"),
      tags$ul(
        tags$li(strong("Measurement error:"), "Imperfect measures that lead to bias. This is possibly controllable or reducible with better measuring approaches."),
        tags$li(strong("Process uncertainty:"), "Naturally occuring variability (e.g., length at age, recruitment). This is hard to control and a source of both imprecision. May also induce bias if ignored or mis-modelled."),
      ),
      
    )
    uncertainty_text
  })
  
  # Sources text
  output$sources <- renderUI({

    # Sources of uncertainty
    sources_text <- tagList(

      h5("Where Does Model Uncertainty Come From?", class = "text-primary"),
      tags$ul(
        tags$li(strong("Data Representativeness:"), "When the data do not measure or represent what is intended. Major source of bias."),
        tags$li(strong("Parameter estimation:"), "Unknown parameter values and estimating them via data and/or priors.  May produces both bias and imprecision"),
        tags$li(strong("Model assumptions:"), "Models are approximations of reality and have assumptions based on how they are specified (i.e., which parameters are used and estimated or not). Those assumptions may cause bias if they are poor or poorly explored."),
        tags$li(strong("Model type:"), "Different models will have different assumptions. Knowing these assumptions for each model type will identify areas of uncertainty (or overcertainty when pre-specifying parameters)."),
        tags$li(strong("Natural Variability:"), "No matter how well the system is measured, it may not be stationary or static. Natural variability, even measured perfectly, causes uncertainty."),
      ),

    )

    sources_text
    
  })

  # Sources of uncertainty
  output$estimation <- renderUI({
    
    estimation_text <- tagList(

    h5("How Is Stock Assessment Uncertainty Estimated?", class = "text-primary"),
    tags$ul(
      tags$li(strong("Within Model"), "When the data do not measure or represent what is intended. Major source of bias."),
      tags$ol(
        tags$li(strong("Maximum Likelihood Estimation (MLE):"), "Produces asymptotic variances which are normally distributed. Much faster than Bayesian analyses, but may underestimate within model uncertainty compared to Bayesian analyses."),
        tags$li(strong("Bayesian Estimation:"), "Uses the data, priors and MLE to explore and estimate uncertainty. Long estimation run times."),
      ),
      tags$li(strong("Among Model Uncertainty:"), "Different models will have different assumptions. Knowing these assumptions is key.  "),
      tags$ol(
        tags$li(strong("Sensitivity Analysis:"), "Changing model inputs or assumptions to explore how it changes model outputs. One of the most common and powerful ways to explore model uncertainty."),
        tags$li(strong("Likelihood Profiles:"), "Changing one parameter or model specification across a series of values to see how the model fit and outputs change. A way to demonstrate both within model and among model uncertainty."),
      ),
    ),
    
  )
  
  estimation_text
  
})

      
}

# Run the application
shinyApp(ui = ui, server = server)
