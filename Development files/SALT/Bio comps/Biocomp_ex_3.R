library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI
ui <- page_sidebar(
  title = "Fish Population Age and Length Composition Comparison",
  
  sidebar = sidebar(
    h4("Population Parameters"),
    
    # Growth parameters
    card(
      card_header("Growth Curve (von Bertalanffy)"),
      numericInput("L_inf", "L∞ (asymptotic length, cm):", value = 100, min = 50, max = 200),
      numericInput("K", "K (growth coefficient):", value = 0.3, min = 0.1, max = 1.0, step = 0.05),
      numericInput("t0", "t₀ (theoretical age at length 0):", value = -0.5, min = -2, max = 0, step = 0.1)
    ),
    
    # Mortality parameters
    card(
      card_header("Mortality Parameters"),
      numericInput("M", "Natural Mortality (M, year⁻¹):", value = 0.2, min = 0.1, max = 0.8, step = 0.05),
      numericInput("F", "Fishing Mortality (F, year⁻¹):", value = 0.3, min = 0, max = 1.0, step = 0.05)
    ),
    
    # Selectivity parameters
    card(
      card_header("Age-based Selectivity"),
      numericInput("age_sel_50", "Age at 50% selection:", value = 3, min = 1, max = 10, step = 0.5),
      numericInput("age_sel_95", "Age at 95% selection:", value = 5, min = 2, max = 15, step = 0.5)
    ),
    
    card(
      card_header("Length-based Selectivity"),
      numericInput("len_sel_50", "Length at 50% selection (cm):", value = 40, min = 10, max = 100),
      numericInput("len_sel_95", "Length at 95% selection (cm):", value = 60, min = 20, max = 120)
    ),
    
    # Simulation parameters
    card(
      card_header("Simulation Settings"),
      numericInput("max_age", "Maximum Age:", value = 15, min = 10, max = 30),
      numericInput("R0", "Recruitment (R₀):", value = 1000, min = 100, max = 10000)
    )
  ),
  
  # Main panel with outputs
  layout_columns(
    col_widths = c(6, 6),
    
    card(
      card_header("Age Composition Comparison"),
      plotlyOutput("age_comp_plot", height = "400px")
    ),
    
    card(
      card_header("Length Composition Comparison"),
      plotlyOutput("length_comp_plot", height = "400px")
    ),
    
    card(
      card_header("Selectivity Curves"),
      plotlyOutput("selectivity_plot", height = "400px")
    ),
    
    card(
      card_header("Growth Curve"),
      plotlyOutput("growth_plot", height = "400px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to calculate population dynamics
  population_data <- reactive({
    ages <- 1:input$max_age
    
    # Von Bertalanffy growth equation
    lengths <- input$L_inf * (1 - exp(-input$K * (ages - input$t0)))
    
    # Logistic selectivity functions
    # Age-based selectivity
    age_slope <- log(19) / (input$age_sel_95 - input$age_sel_50)  # slope for 5% to 95%
    age_selectivity <- 1 / (1 + exp(-age_slope * (ages - input$age_sel_50)))
    
    # Length-based selectivity
    len_slope <- log(19) / (input$len_sel_95 - input$len_sel_50)
    length_selectivity <- 1 / (1 + exp(-len_slope * (lengths - input$len_sel_50)))
    
    # Unfished population (exponential decay with natural mortality)
    unfished_numbers <- input$R0 * exp(-input$M * (ages - 1))
    
    # Fished population (with both natural and fishing mortality)
    # Fishing mortality is applied based on selectivity
    total_mortality <- input$M + input$F * age_selectivity
    fished_numbers <- input$R0 * exp(-cumsum(c(0, total_mortality[-length(total_mortality)])))
    
    # Create data frame
    data.frame(
      age = ages,
      length = lengths,
      age_selectivity = age_selectivity,
      length_selectivity = length_selectivity,
      unfished_numbers = unfished_numbers,
      fished_numbers = fished_numbers,
      unfished_prop = unfished_numbers / sum(unfished_numbers),
      fished_prop = fished_numbers / sum(fished_numbers)
    )
  })
  
  # Age composition plot
  output$age_comp_plot <- renderPlotly({
    data <- population_data()
    
    # Reshape data for plotting
    plot_data <- data %>%
      select(age, unfished_prop, fished_prop) %>%
      tidyr::pivot_longer(cols = c(unfished_prop, fished_prop), 
                          names_to = "condition", 
                          values_to = "proportion") %>%
      mutate(condition = case_when(
        condition == "unfished_prop" ~ "Unfished",
        condition == "fished_prop" ~ "Fished"
      ))
    
    p <- ggplot(plot_data, aes(x = age, y = proportion, fill = condition)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      scale_fill_manual(values = c("Unfished" = "#2E86C1", "Fished" = "#E74C3C")) +
      labs(x = "Age (years)", y = "Proportion", fill = "Condition") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Length composition plot
  output$length_comp_plot <- renderPlotly({
    data <- population_data()
    
    # Create length bins for better visualization
    length_bins <- seq(0, max(data$length) + 5, by = 5)
    data$length_bin <- cut(data$length, breaks = length_bins, labels = FALSE)
    
    # Aggregate by length bins
    length_data <- data %>%
      group_by(length_bin) %>%
      summarise(
        length_mid = mean(range(c(0, length_bins))[length_bin:(length_bin+1)]),
        unfished_prop = sum(unfished_prop),
        fished_prop = sum(fished_prop),
        .groups = 'drop'
      ) %>%
      tidyr::pivot_longer(cols = c(unfished_prop, fished_prop), 
                          names_to = "condition", 
                          values_to = "proportion") %>%
      mutate(condition = case_when(
        condition == "unfished_prop" ~ "Unfished",
        condition == "fished_prop" ~ "Fished"
      ))
    
    p <- ggplot(length_data, aes(x = length_mid, y = proportion, fill = condition)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      scale_fill_manual(values = c("Unfished" = "#2E86C1", "Fished" = "#E74C3C")) +
      labs(x = "Length (cm)", y = "Proportion", fill = "Condition") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Selectivity plot
  output$selectivity_plot <- renderPlotly({
    data <- population_data()
    
    plot_data <- data %>%
      select(age, length, age_selectivity, length_selectivity) %>%
      tidyr::pivot_longer(cols = c(age_selectivity, length_selectivity), 
                          names_to = "selectivity_type", 
                          values_to = "selectivity") %>%
      mutate(
        selectivity_type = case_when(
          selectivity_type == "age_selectivity" ~ "Age-based",
          selectivity_type == "length_selectivity" ~ "Length-based"
        ),
        x_var = ifelse(selectivity_type == "Age-based", age, length),
        x_label = ifelse(selectivity_type == "Age-based", "Age (years)", "Length (cm)")
      )
    
    p <- ggplot(plot_data, aes(x = x_var, y = selectivity, color = selectivity_type)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c("Age-based" = "#8E44AD", "Length-based" = "#D35400")) +
      labs(x = "Age (years) / Length (cm)", y = "Selectivity", color = "Selectivity Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Growth curve plot
  output$growth_plot <- renderPlotly({
    data <- population_data()
    
    p <- ggplot(data, aes(x = age, y = length)) +
      geom_line(color = "#27AE60", size = 1.5) +
      geom_point(color = "#27AE60", size = 2) +
      labs(x = "Age (years)", y = "Length (cm)", 
           title = paste0("Von Bertalanffy Growth Curve (L∞=", input$L_inf, ", K=", input$K, ")")) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
