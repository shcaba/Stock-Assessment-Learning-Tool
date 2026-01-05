#Baranov catch equation

library(shiny)
library(bslib)
library(plotly)
library(DT)

# Define UI
ui <- page_sidebar(
  title = "Baranov Catch Equation",
  sidebar = sidebar(
    h4("Parameters"),
    numericInput("N0", "Initial Population (N₀):", 
                 value = 1000, min = 1, max = 10000, step = 100),
    numericInput("F", "Fishing Mortality Rate (F):", 
                 value = 0.3, min = 0, max = 2, step = 0.1),
    numericInput("M", "Natural Mortality Rate (M):", 
                 value = 0.2, min = 0, max = 2, step = 0.1),
    numericInput("t", "Time Period (years):", 
                 value = 1, min = 0.1, max = 5, step = 0.1),
    hr(),
    h4("Scenario Analysis"),
    checkboxInput("compare_scenarios", "Compare F scenarios", FALSE),
    conditionalPanel(
      condition = "input.compare_scenarios",
      numericInput("F2", "Second F value:", value = 0.5, min = 0, max = 2, step = 0.1),
      numericInput("F3", "Third F value:", value = 0.8, min = 0, max = 2, step = 0.1)
    )
  ),
  
  layout_columns(
    card(
      card_header("Results Summary"),
      tableOutput("results_table")
    ),
    card(
      card_header("Population Dynamics Over Time"),
      plotlyOutput("population_plot")
    )
  ),
  
  layout_columns(
    card(
      card_header("Baranov Catch Equation"),
      div(
        h5("Formula:"),
        withMathJax("$$C = \\frac{F}{F + M} \\cdot N_0 \\cdot (1 - e^{-(F+M) \\cdot t})$$"),
        br(),
        h5("Where:"),
        tags$ul(
          tags$li("C = Total catch"),
          tags$li("F = Fishing mortality rate"),
          tags$li("M = Natural mortality rate"),
          tags$li("N₀ = Initial population size"),
          tags$li("t = Time period")
        ),
        br(),
        h5("Survival:"),
        withMathJax("$$S = e^{-(F+M) \\cdot t}$$"),
        h5("Final Population:"),
        withMathJax("$$N_t = N_0 \\cdot S$$")
      )
    ),
    card(
      card_header("Sensitivity Analysis"),
      plotlyOutput("sensitivity_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Baranov catch equation function
  baranov_catch <- function(N0, F, M, t) {
    Z <- F + M  # Total mortality
    C <- (F / Z) * N0 * (1 - exp(-Z * t))
    S <- exp(-Z * t)
    Nt <- N0 * S
    
    list(
      catch = C,
      survival = S,
      final_pop = Nt,
      mortality_ratio = F / Z
    )
  }
  
  baranov_catch_selective <- function(N0_l, F, M, S_l, t) {
    F_effective <- F * S_l  # Effective fishing mortality
    Z_l <- F_effective + M  # Total mortality at length l
    C_l <- (F_effective / Z_l) * N0_l * (1 - exp(-Z_l * t))
    S_survival <- exp(-Z_l * t)
    Nt_l <- N0_l * S_survival
    
    list(
      catch = C_l,
      survival = S_survival,
      final_pop = Nt_l,
      effective_F = F_effective,
      total_Z = Z_l
    )
  }
  
  # Results table
  output$results_table <- renderTable({
    result <- baranov_catch(input$N0, input$F, input$M, input$t)
    
    data.frame(
      Metric = c("Total Catch", "Final Population", "Survival Rate", 
                 "Fishing vs Total Mortality", "Total Mortality Rate"),
      Value = c(
        round(result$catch, 1),
        round(result$final_pop, 1),
        paste0(round(result$survival * 100, 1), "%"),
        paste0(round(result$mortality_ratio * 100, 1), "%"),
        round(input$F + input$M, 3)
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # Population dynamics plot
  output$population_plot <- renderPlotly({
    time_seq <- seq(0, input$t, length.out = 100)
    
    if (input$compare_scenarios) {
      # Multiple scenarios
      scenarios <- data.frame(
        time = rep(time_seq, 3),
        population = c(
          input$N0 * exp(-(input$F + input$M) * time_seq),
          input$N0 * exp(-(input$F2 + input$M) * time_seq),
          input$N0 * exp(-(input$F3 + input$M) * time_seq)
        ),
        scenario = rep(c(paste("F =", input$F), 
                         paste("F =", input$F2), 
                         paste("F =", input$F3)), each = length(time_seq))
      )
      
      p <- ggplot2::ggplot(scenarios, ggplot2::aes(x = time, y = population, color = scenario)) +
        ggplot2::geom_line(size = 1.2) +
        ggplot2::labs(x = "Time (years)", y = "Population", 
                      title = "Population Decline Under Different Fishing Pressures") +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_viridis_d()
      
    } else {
      # Single scenario
      population <- input$N0 * exp(-(input$F + input$M) * time_seq)
      catch_cumulative <- sapply(time_seq, function(t) {
        if (t == 0) 0 else baranov_catch(input$N0, input$F, input$M, t)$catch
      })
      
      df <- data.frame(
        time = time_seq,
        population = population,
        cumulative_catch = catch_cumulative
      )
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
        ggplot2::geom_line(ggplot2::aes(y = population, color = "Population"), size = 1.2) +
        ggplot2::geom_line(ggplot2::aes(y = cumulative_catch, color = "Cumulative Catch"), size = 1.2) +
        ggplot2::labs(x = "Time (years)", y = "Number of Fish", 
                      title = "Population and Cumulative Catch Over Time") +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_manual(values = c("Population" = "#1f77b4", "Cumulative Catch" = "#ff7f0e")) +
        ggplot2::labs(color = "")
    }
    
    ggplotly(p)
  })
  
  # Sensitivity analysis
  output$sensitivity_plot <- renderPlotly({
    F_values <- seq(0, 1.5, by = 0.05)
    catches <- sapply(F_values, function(f) {
      baranov_catch(input$N0, f, input$M, input$t)$catch
    })
    
    df <- data.frame(F = F_values, Catch = catches)
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = F, y = Catch)) +
      ggplot2::geom_line(size = 1.2, color = "#2ca02c") +
      ggplot2::geom_vline(xintercept = input$F, linetype = "dashed", color = "red", alpha = 0.7) +
      ggplot2::labs(x = "Fishing Mortality Rate (F)", y = "Total Catch", 
                    title = "Catch vs Fishing Mortality Rate") +
      ggplot2::theme_minimal() +
      ggplot2::annotate("text", x = input$F + 0.1, y = max(catches) * 0.9, 
                        label = paste("Current F =", input$F), color = "red")
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
