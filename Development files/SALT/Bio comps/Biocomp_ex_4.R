library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI
ui <- page_sidebar(
  title = "Fished vs Unfished Population Composition",
  
  sidebar = sidebar(
    h4("Growth Parameters"),
    numericInput("Linf", "L∞ (Asymptotic Length)", value = 50, min = 10, max = 200, step = 1),
    numericInput("K", "Growth Rate (K)", value = 0.2, min = 0.01, max = 1, step = 0.01),
    numericInput("t0", "Theoretical Age at Length 0 (t₀)", value = -1, min = -5, max = 0, step = 0.1),
    
    hr(),
    h4("Mortality Parameters"),
    numericInput("M", "Natural Mortality (M)", value = 0.2, min = 0.01, max = 1, step = 0.01),
    numericInput("F", "Fishing Mortality (F)", value = 0.3, min = 0, max = 2, step = 0.05),
    
    hr(),
    h4("Selectivity Parameters"),
    selectInput("selectivity_type", "Selectivity Type",
                choices = list("Logistic" = "logistic", "Knife-edge" = "knife"),
                selected = "logistic"),
    conditionalPanel(
      condition = "input.selectivity_type == 'logistic'",
      numericInput("L50", "L₅₀ (Length at 50% selectivity)", value = 25, min = 1, max = 100, step = 1),
      numericInput("L95", "L₉₅ (Length at 95% selectivity)", value = 35, min = 1, max = 100, step = 1)
    ),
    conditionalPanel(
      condition = "input.selectivity_type == 'knife'",
      numericInput("Lc", "Lc (Length at full selectivity)", value = 30, min = 1, max = 100, step = 1)
    ),
    
    hr(),
    h4("Population Parameters"),
    numericInput("max_age", "Maximum Age", value = 20, min = 5, max = 50, step = 1),
    numericInput("R0", "Unfished Recruitment", value = 1000, min = 100, max = 10000, step = 100)
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Age Composition Comparison"),
      plotlyOutput("age_plot")
    ),
    card(
      card_header("Length Composition Comparison"),
      plotlyOutput("length_plot")
    )
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Growth Curve"),
      plotlyOutput("growth_plot")
    ),
    card(
      card_header("Selectivity Curve"),
      plotlyOutput("selectivity_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data calculation
  pop_data <- reactive({
    # Create age vector
    ages <- 0:input$max_age
    
    # Von Bertalanffy growth equation
    lengths <- input$Linf * (1 - exp(-input$K * (ages - input$t0)))
    
    # Calculate selectivity
    if (input$selectivity_type == "logistic") {
      # Logistic selectivity
      slope <- log(19) / (input$L95 - input$L50)  # 19 = ln(0.95/0.05)
      selectivity <- 1 / (1 + exp(-slope * (lengths - input$L50)))
    } else {
      # Knife-edge selectivity
      selectivity <- ifelse(lengths >= input$Lc, 1, 0)
    }
    
    # Calculate unfished numbers at age (exponential decay due to natural mortality)
    N_unfished <- input$R0 * exp(-input$M * ages)
    
    # Calculate fished numbers at age (exponential decay due to total mortality)
    Z <- input$M + input$F * selectivity  # Total mortality
    N_fished <- input$R0 * exp(-cumsum(c(0, Z[-length(Z)])))  # More accurate cumulative mortality
    
    # Create data frame
    data.frame(
      age = ages,
      length = lengths,
      selectivity = selectivity,
      N_unfished = N_unfished,
      N_fished = N_fished,
      F_at_age = input$F * selectivity,
      Z = Z
    )
  })
  
  # Age composition plot
  output$age_plot <- renderPlotly({
    data <- pop_data()
    
    # Reshape data for plotting
    plot_data <- data %>%
      select(age, N_unfished, N_fished) %>%
      tidyr::pivot_longer(cols = c(N_unfished, N_fished), 
                          names_to = "population", values_to = "numbers") %>%
      mutate(population = case_when(
        population == "N_unfished" ~ "Unfished",
        population == "N_fished" ~ "Fished"
      ))
    
    p <- ggplot(plot_data, aes(x = age, y = numbers, fill = population)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      scale_fill_manual(values = c("Unfished" = "#2E86AB", "Fished" = "#A23B72")) +
      labs(x = "Age", y = "Numbers", fill = "Population") +
      theme_minimal() +
      theme(legend.position = "top")
    
    ggplotly(p)
  })
  
  # Length composition plot
  output$length_plot <- renderPlotly({
    data <- pop_data()
    
    # Create length bins for better visualization
    length_bins <- seq(0, max(data$length) * 1.1, length.out = 20)
    
    # Assign each fish to length bins
    data$length_bin <- cut(data$length, breaks = length_bins, include.lowest = TRUE)
    bin_centers <- (length_bins[-1] + length_bins[-length(length_bins)]) / 2
    
    # Sum numbers by length bin
    length_data <- data %>%
      group_by(length_bin) %>%
      summarise(
        length_center = mean(length),
        N_unfished = sum(N_unfished),
        N_fished = sum(N_fished),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(cols = c(N_unfished, N_fished), 
                          names_to = "population", values_to = "numbers") %>%
      mutate(population = case_when(
        population == "N_unfished" ~ "Unfished",
        population == "N_fished" ~ "Fished"
      ))
    
    p <- ggplot(length_data, aes(x = length_center, y = numbers, fill = population)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      scale_fill_manual(values = c("Unfished" = "#2E86AB", "Fished" = "#A23B72")) +
      labs(x = "Length", y = "Numbers", fill = "Population") +
      theme_minimal() +
      theme(legend.position = "top")
    
    ggplotly(p)
  })
  
  # Growth curve plot
  output$growth_plot <- renderPlotly({
    data <- pop_data()
    
    p <- ggplot(data, aes(x = age, y = length)) +
      geom_line(color = "#F18F01", size = 1.2) +
      geom_point(color = "#F18F01", size = 2) +
      labs(x = "Age", y = "Length", 
           title = paste("Von Bertalanffy Growth Curve")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Selectivity curve plot
  output$selectivity_plot <- renderPlotly({
    data <- pop_data()
    
    p <- ggplot(data, aes(x = length, y = selectivity)) +
      geom_line(color = "#C73E1D", size = 1.2) +
      geom_point(color = "#C73E1D", size = 2) +
      labs(x = "Length", y = "Selectivity", 
           title = paste(tools::toTitleCase(input$selectivity_type), "Selectivity")) +
      ylim(0, 1) +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
