library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Define UI
ui <- page_sidebar(
  title = "von Bertalanffy Growth Model with Variability",
  sidebar = sidebar(
    width = 300,
    h4("Growth Parameters"),
    numericInput("L_inf", 
                 "Asymptotic Length (L∞)", 
                 value = 100, 
                 min = 50, 
                 max = 200, 
                 step = 5),
    numericInput("K", 
                 "Growth Coefficient (K)", 
                 value = 0.3, 
                 min = 0.05, 
                 max = 1.0, 
                 step = 0.05),
    numericInput("t0", 
                 "Theoretical Age at Length 0 (t₀)", 
                 value = -0.5, 
                 min = -2, 
                 max = 0, 
                 step = 0.1),
    hr(),
    h4("Variability Parameters"),
    numericInput("sigma", 
                 "Standard Deviation (σ)", 
                 value = 5, 
                 min = 1, 
                 max = 20, 
                 step = 1),
    numericInput("n_fish", 
                 "Number of Fish to Simulate", 
                 value = 100, 
                 min = 10, 
                 max = 500, 
                 step = 10),
    hr(),
    h4("Age Range"),
    numericInput("max_age", 
                 "Maximum Age (years)", 
                 value = 15, 
                 min = 5, 
                 max = 30, 
                 step = 1),
    br(),
    actionButton("simulate", "Generate New Sample", class = "btn-primary")
  ),
  
  # Main panel with plots and information
  navset_card_tab(
    nav_panel("Growth Curve",
              card(
                card_header("von Bertalanffy Growth with Individual Variation"),
                plotOutput("growth_plot", height = "500px")
              )
    ),
    nav_panel("Length Distribution",
              card(
                card_header("Length Distribution by Age Class"),
                plotOutput("distribution_plot", height = "500px")
              )
    ),
    nav_panel("Model Information",
              card(
                card_header("von Bertalanffy Growth Model"),
                card_body(
                  h4("Model Equation:"),
                  p("L(t) = L∞ × (1 - e^(-K(t - t₀)))"),
                  br(),
                  h4("Parameters:"),
                  tags$ul(
                    tags$li(strong("L∞:"), "Asymptotic length - the theoretical maximum length the fish can reach"),
                    tags$li(strong("K:"), "Growth coefficient - determines how quickly the fish approaches L∞"),
                    tags$li(strong("t₀:"), "Theoretical age at which length would be zero"),
                    tags$li(strong("σ:"), "Standard deviation of length at age (adds biological variability)")
                  ),
                  br(),
                  h4("Biological Interpretation:"),
                  p("The von Bertalanffy growth model describes how fish length changes with age. 
                    In reality, individual fish of the same age show variation in length due to 
                    genetic differences, environmental factors, and measurement error. This app 
                    simulates that variability by adding normally distributed random error to 
                    the predicted lengths.")
                )
              )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # von Bertalanffy growth function
  vb_growth <- function(age, L_inf, K, t0) {
    L_inf * (1 - exp(-K * (age - t0)))
  }
  
  # Reactive data generation
  growth_data <- reactive({
    # Trigger regeneration when simulate button is pressed or parameters change
    input$simulate
    
    ages <- seq(0, input$max_age, by = 0.5)
    
    # Generate predicted lengths
    predicted_lengths <- vb_growth(ages, input$L_inf, input$K, input$t0)
    
    # Create smooth curve data
    curve_data <- data.frame(
      age = ages,
      length = predicted_lengths,
      type = "Expected"
    )
    
    # Generate individual fish data with variability
    set.seed(42 + input$simulate)  # Ensures reproducible but changeable results
    
    # Sample ages for individual fish
    fish_ages <- sample(seq(1, input$max_age, by = 0.5), input$n_fish, replace = TRUE)
    
    # Calculate expected lengths for these ages
    expected_lengths <- vb_growth(fish_ages, input$L_inf, input$K, input$t0)
    
    # Add normally distributed error
    observed_lengths <- expected_lengths + rnorm(input$n_fish, 0, input$sigma)
    
    # Ensure no negative lengths
    observed_lengths <- pmax(observed_lengths, 0)
    
    fish_data <- data.frame(
      age = fish_ages,
      length = observed_lengths,
      type = "Observed"
    )
    
    list(curve = curve_data, fish = fish_data)
  })
  
  # Main growth plot
  output$growth_plot <- renderPlot({
    data <- growth_data()
    
    ggplot() +
      geom_point(data = data$fish, aes(x = age, y = length), 
                 alpha = 0.6, color = "steelblue", size = 2) +
      geom_line(data = data$curve, aes(x = age, y = length), 
                color = "red", size = 1.2) +
      geom_ribbon(data = data$curve, 
                  aes(x = age, ymin = length - input$sigma, ymax = length + input$sigma),
                  alpha = 0.2, fill = "red") +
      labs(
        x = "Age (years)",
        y = "Length (cm)",
        title = "von Bertalanffy Growth Model with Individual Variation",
        subtitle = paste("L∞ =", input$L_inf, "cm, K =", input$K, ", t₀ =", input$t0, ", σ =", input$sigma, "cm")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      ) +
      scale_x_continuous(breaks = seq(0, input$max_age, by = 2)) +
      annotate("text", x = input$max_age * 0.7, y = input$L_inf * 0.2, 
               label = "Red line: Expected growth\nBlue points: Individual fish\nShaded area: ±1 SD", 
               hjust = 0, vjust = 0, size = 3.5, color = "gray30")
  })
  
  # Length distribution plot
  output$distribution_plot <- renderPlot({
    data <- growth_data()
    
    # Create age classes for boxplot
    data$fish$age_class <- round(data$fish$age)
    
    # Filter to show only age classes with sufficient data
    age_counts <- table(data$fish$age_class)
    ages_to_show <- names(age_counts[age_counts >= 5])
    fish_filtered <- data$fish[data$fish$age_class %in% ages_to_show, ]
    
    if(nrow(fish_filtered) > 0) {
      ggplot(fish_filtered, aes(x = factor(age_class), y = length)) +
        geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.color = "red") +
        geom_jitter(width = 0.2, alpha = 0.5, color = "steelblue") +
        labs(
          x = "Age Class (years)",
          y = "Length (cm)",
          title = "Length Distribution by Age Class",
          subtitle = "Boxplots show median, quartiles, and outliers"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
    } else {
      ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Insufficient data for age classes"), size = 6) +
        theme_void()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
