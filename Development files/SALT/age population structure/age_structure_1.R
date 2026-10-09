library(shiny)
library(bslib)
library(ggplot2)

ui <- page_sidebar(
  title = "Age-Structured Population Dynamics",
  sidebar = sidebar(
    h4("Population Controls"),
    sliderInput(
      "recruits",
      "New individuals (age 0):",
      min = 0,
      max = 100,
      value = 20,
      step = 1
    ),
    sliderInput(
      "mortality_rate",
      "Mortality rate (proportion leaving):",
      min = 0,
      max = 0.5,
      value = 0.1,
      step = 0.01
    ),
    hr(),
    actionButton("step", "Advance One Time Step", class = "btn-primary"),
    actionButton("reset", "Reset Population", class = "btn-secondary"),
    hr(),
    p("Click 'Advance One Time Step' to:"),
    tags$ul(
      tags$li("Add new individuals to age group 0"),
      tags$li("Apply mortality to all age groups"),
      tags$li("Age all individuals by 1 year")
    )
  ),
  card(
    card_header("Population Distribution by Age Group"),
    plotOutput("age_plot", height = "400px")
  ),
  card(
    card_header("Population Summary"),
    tableOutput("summary_table")
  )
)

server <- function(input, output, session) {
  # Initialize population - 10 age groups (0-9)
  population <- reactiveVal({
    data.frame(
      age_group = 0:9,
      count = c(50, 45, 40, 35, 30, 25, 20, 15, 10, 5)
    )
  })
  
  # Reset population to initial state
  observeEvent(input$reset, {
    population(data.frame(
      age_group = 0:9,
      count = c(50, 45, 40, 35, 30, 25, 20, 15, 10, 5)
    ))
  })
  
  # Advance one time step
  observeEvent(input$step, {
    current_pop <- population()
    
    # Apply mortality to all age groups
    current_pop$count <- current_pop$count * (1 - input$mortality_rate)
    
    # Age all individuals (shift counts up by one age group)
    new_counts <- numeric(10)
    new_counts[1] <- input$recruits  # New recruits in age 0
    
    # Move existing individuals to next age group
    for (i in 2:10) {
      new_counts[i] <- current_pop$count[i - 1]
    }
    
    # Update population
    population(data.frame(
      age_group = 0:9,
      count = new_counts
    ))
  })
  
  # Plot age distribution
  output$age_plot <- renderPlot({
    pop_data <- population()
    
    ggplot(pop_data, aes(x = factor(age_group), y = count)) +
      geom_col(fill = "#2C3E50", alpha = 0.8) +
      geom_text(aes(label = round(count, 1)), vjust = -0.5, size = 4) +
      labs(
        x = "Age Group",
        y = "Number of Individuals",
        title = "Population by Age Group"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.minor = element_blank()
      ) +
      ylim(0, max(pop_data$count) * 1.15)
  })
  
  # Summary table
  output$summary_table <- renderTable({
    pop_data <- population()
    
    data.frame(
      Metric = c("Total Population", "Mean Age", "Youngest Group (Age 0)", "Oldest Group (Age 9)"),
      Value = c(
        round(sum(pop_data$count), 1),
        round(sum(pop_data$age_group * pop_data$count) / sum(pop_data$count), 2),
        round(pop_data$count[1], 1),
        round(pop_data$count[10], 1)
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
}

shinyApp(ui, server)

