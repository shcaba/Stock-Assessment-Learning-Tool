library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)

# Simulate fish population data
generate_fish_data <- function(n_fish = 1000, fishing_mortality = 0, natural_mortality = 0.2, 
                               max_age = 20, growth_k = 0.15, linf = 80, t0 = -1) {
  
  # Generate ages with exponential mortality
  total_mortality <- natural_mortality + fishing_mortality
  ages <- rexp(n_fish, rate = total_mortality)
  ages <- pmin(ages, max_age)  # Cap at maximum age
  ages <- ages[ages > 0.5]  # Remove very young fish
  
  # Von Bertalanffy growth equation for length
  lengths <- linf * (1 - exp(-growth_k * (ages - t0)))
  
  # Add some variability to lengths
  lengths <- lengths + rnorm(length(lengths), 0, 3)
  lengths <- pmax(lengths, 5)  # Minimum length
  
  data.frame(
    age = ages,
    length = lengths,
    population = ifelse(fishing_mortality > 0, "Fished", "Unfished")
  )
}

ui <- page_sidebar(
  title = "Fish Population Structure: Fished vs Unfished",
  
  sidebar = sidebar(
    h4("Population Parameters"),
    
    numericInput("n_fish", 
                 "Population Size:", 
                 value = 1000, 
                 min = 100, 
                 max = 5000, 
                 step = 100),
    
    sliderInput("fishing_mortality", 
                "Fishing Mortality Rate:", 
                min = 0, 
                max = 1, 
                value = 0.3, 
                step = 0.05),
    
    sliderInput("natural_mortality", 
                "Natural Mortality Rate:", 
                min = 0.1, 
                max = 0.5, 
                value = 0.2, 
                step = 0.05),
    
    hr(),
    
    h4("Growth Parameters"),
    
    sliderInput("max_age", 
                "Maximum Age (years):", 
                min = 10, 
                max = 50, 
                value = 20, 
                step = 1),
    
    sliderInput("linf", 
                "Asymptotic Length (cm):", 
                min = 40, 
                max = 120, 
                value = 80, 
                step = 5),
    
    sliderInput("growth_k", 
                "Growth Rate (K):", 
                min = 0.05, 
                max = 0.5, 
                value = 0.15, 
                step = 0.01)
  ),
  
  layout_columns(
    col_widths = c(12, 6, 6),
    
    card(
      card_header("Population Summary Statistics"),
      tableOutput("summary_table")
    ),
    
    card(
      card_header("Age Composition"),
      plotlyOutput("age_plot", height = "400px")
    ),
    
    card(
      card_header("Length Composition"), 
      plotlyOutput("length_plot", height = "400px")
    ),
    
    card(
      card_header("Length vs Age Relationship"),
      plotlyOutput("length_age_plot", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  
  # Generate reactive data for both populations
  fish_data <- reactive({
    # Unfished population
    unfished <- generate_fish_data(
      n_fish = input$n_fish,
      fishing_mortality = 0,
      natural_mortality = input$natural_mortality,
      max_age = input$max_age,
      growth_k = input$growth_k,
      linf = input$linf
    )
    
    # Fished population
    fished <- generate_fish_data(
      n_fish = input$n_fish,
      fishing_mortality = input$fishing_mortality,
      natural_mortality = input$natural_mortality,
      max_age = input$max_age,
      growth_k = input$growth_k,
      linf = input$linf
    )
    
    rbind(unfished, fished)
  })
  
  # Summary statistics table
  output$summary_table <- renderTable({
    data <- fish_data()
    
    summary_stats <- data %>%
      group_by(population) %>%
      summarise(
        `Sample Size` = n(),
        `Mean Age (years)` = round(mean(age), 2),
        `Max Age (years)` = round(max(age), 2),
        `Mean Length (cm)` = round(mean(length), 2),
        `Max Length (cm)` = round(max(length), 2),
        .groups = 'drop'
      )
    
    summary_stats
  }, striped = TRUE, hover = TRUE)
  
  # Age composition histogram
  output$age_plot <- renderPlotly({
    data <- fish_data()
    
    p <- ggplot(data, aes(x = age, fill = population)) +
      geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
      scale_fill_manual(values = c("Fished" = "#e74c3c", "Unfished" = "#3498db")) +
      labs(x = "Age (years)", y = "Frequency", 
           title = "Age Distribution: Fished vs Unfished Population") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Length composition histogram  
  output$length_plot <- renderPlotly({
    data <- fish_data()
    
    p <- ggplot(data, aes(x = length, fill = population)) +
      geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
      scale_fill_manual(values = c("Fished" = "#e74c3c", "Unfished" = "#3498db")) +
      labs(x = "Length (cm)", y = "Frequency",
           title = "Length Distribution: Fished vs Unfished Population") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Length vs Age scatter plot
  output$length_age_plot <- renderPlotly({
    data <- fish_data()
    
    p <- ggplot(data, aes(x = age, y = length, color = population)) +
      geom_point(alpha = 0.6, size = 1) +
      geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
      scale_color_manual(values = c("Fished" = "#e74c3c", "Unfished" = "#3498db")) +
      labs(x = "Age (years)", y = "Length (cm)",
           title = "Length vs Age: Growth Trajectories") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
}

shinyApp(ui = ui, server = server)
