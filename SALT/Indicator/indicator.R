library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(shinyWidgets)

ui <- page_sidebar(
  title = "Time Series Average vs Target Comparison",
  
  sidebar = sidebar(
    h4("Data Options"),
    
    pickerInput(inputId = 'stock.choice',
                label = 'Choose A Stock',
                choices = c("A","B","C"),
                options = list(`style` = "btn-info")),

    # Input for target value
    h5("Select the years to use for each indicator by data set"),
    textInput(
      "Ct_I_yrs",
      "Indicator years:",
      value = ""
      ),
    
    textInput(
      "Ct_RP_yrs",
      "Reference point:",
      value = ""
    ),

        textInput(
      "Index_yrs",
      "Index years:",
      value = ""
    ),
    
    textInput(
      "MeanLt_yrs",
      "Mean length years:",
      value = ""
    ),
    
    # pickerInput(inputId = 'data.choice',
    #             label = 'Choose A Data set',
    #             choices = c("Catches","Index","Mean Length"),
    #             options = list(`style` = "btn-info")),
    
    
    # Input for time series values
    textAreaInput(
      "time_series",
      "Time Series Values (comma-separated):",
      value = "10, 12, 15, 8, 11, 13, 9, 14, 16, 10",
      placeholder = "Enter values separated by commas",
      rows = 4
    ),
    
    # Input for target value
    numericInput(
      "target_value",
      "Target Value:",
      value = 12,
      min = 0,
      step = 0.1
    ),
    
    # Action button to update analysis
    actionButton(
      "update_analysis",
      "Update Analysis",
      class = "btn-primary"
    )
  ),
  
  # Main panel with results
  layout_columns(
    col_widths = c(6, 6),
    
    # Summary statistics card
    card(
      card_header("Summary Statistics"),
      tableOutput("summary_stats")
    ),
    card(
      card_header("General Management Procedure"),
       withMathJax(),
       div(
         style = "text-align: center; font-size:24px; margin: 20px 0;",
         "$$\\ MM_{y} = \\ MM_{y-z} \\cdot \\ Modifier$$"
       ),
      #p("MM_(y+1) = MM_Y * Modifier"),
      #br(),
      p("Where:"),
      tags$ul(
        tags$li("MM = Management Metric. Examples are catch limits, effort, etc."),
        tags$li("y = current year"),
        tags$li(paste0("z = years before current year",em("y"))),
        tags$li("Modifer = "),
        tags$ol(
          tags$li("Indicator (I) compared to reference point (RP). For example, is the indicator more or less than the reference point?"),
          tags$li("Control rule (CR)= rule to adjust the managaement metric based on comparison to reference point.")
        ),
      ),
      p("The control rule would then be the Modifier used in the general equation."),
      p("An example of a simple control rule based on an Indicator (I) and Reference Point (RP) is:."),
      div(
        style = "text-align: center; font-size:24px; margin: 20px 0;",
        "$$\\ CR = \\frac{I}{RP}$$"
      ),
      
    ),
    
    # Comparison results card
    card(
      card_header("Target Comparison"),
      tableOutput("comparison_results")
    )
  ),
  
  # Time series plot
  card(
    card_header("Time Series Plot with Average and Target"),
    plotOutput("time_series_plot", height = "400px")
  ),
  
  # Distribution plot
  card(
    card_header("Value Distribution"),
    plotOutput("distribution_plot", height = "300px")
  )
)





server <- function(input, output, session) {
  
  # Reactive function to parse and process time series data
  
  stock.data <- readRDS("stock_data.RDS")
  observe({print(input$stock.choice)})
  browser()
  
  
  stock.data.choice<-reactive({
    stock.data.choice<-subset(stock.data,Stock=="A")
      })
  
  input$stock.choice
  output$time_series_plot <- renderPlot({
        
    })
  
  processed_data <- eventReactive(input$update_analysis, {
    # Parse the input string
    values_text <- gsub("\\s", "", input$time_series) # Remove whitespace
    values <- as.numeric(unlist(strsplit(values_text, ",")))
    
    # Remove NA values (invalid entries)
    values <- values[!is.na(values)]
    
    if(length(values) == 0) {
      return(NULL)
    }
    
    # Create time series data frame
    data.frame(
      time = 1:length(values),
      value = values
    )
  }, ignoreNULL = FALSE)
  
  # Calculate statistics
  stats <- reactive({
    data <- processed_data()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    values <- data$value
    avg <- mean(values)
    
    list(
      count = length(values),
      mean = avg,
      median = median(values),
      sd = sd(values),
      min = min(values),
      max = max(values),
      target = input$target_value,
      difference = avg - input$target_value,
      percent_diff = ((avg - input$target_value) / input$target_value) * 100
    )
  })
  
  # Summary statistics table
  output$summary_stats <- renderTable({
    s <- stats()
    if(is.null(s)) return(data.frame(Statistic = "No valid data", Value = ""))
    
    data.frame(
      Statistic = c("Count", "Mean", "Median", "Std Dev", "Min", "Max"),
      Value = c(
        s$count,
        round(s$mean, 3),
        round(s$median, 3),
        round(s$sd, 3),
        round(s$min, 3),
        round(s$max, 3)
      )
    )
  }, striped = TRUE)
  
  # Comparison results table
  output$comparison_results <- renderTable({
    s <- stats()
    if(is.null(s)) return(data.frame(Metric = "No valid data", Value = ""))
    
    status <- ifelse(s$difference > 0, "Above Target", 
                     ifelse(s$difference < 0, "Below Target", "On Target"))
    
    data.frame(
      Metric = c("Average", "Target", "Difference", "% Difference", "Status"),
      Value = c(
        round(s$mean, 3),
        round(s$target, 3),
        round(s$difference, 3),
        paste0(round(s$percent_diff, 2), "%"),
        status
      )
    )
  }, striped = TRUE)
  
  # Time series plot
  output$time_series_plot <- renderPlot({
    data <- processed_data()
    if(is.null(data) || nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot", size = 6) +
               theme_minimal())
    }
    
    s <- stats()
    
    ggplot(data, aes(x = time, y = value)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      geom_hline(yintercept = s$mean, color = "red", linetype = "dashed", size = 1, alpha = 0.8) +
      geom_hline(yintercept = s$target, color = "green", linetype = "dashed", size = 1, alpha = 0.8) +
      annotate("text", x = max(data$time) * 0.8, y = s$mean, 
               label = paste("Average:", round(s$mean, 2)), 
               color = "red", vjust = -0.5) +
      annotate("text", x = max(data$time) * 0.8, y = s$target, 
               label = paste("Target:", round(s$target, 2)), 
               color = "green", vjust = -0.5) +
      labs(
        title = "Time Series Values with Average and Target",
        x = "Time Point",
        y = "Value"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()
      )
  })
  
  # Distribution plot
  output$distribution_plot <- renderPlot({
    data <- processed_data()
    if(is.null(data) || nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot", size = 6) +
               theme_minimal())
    }
    
    s <- stats()
    
    ggplot(data, aes(x = value)) +
      geom_histogram(bins = min(10, length(data$value)), fill = "lightblue", alpha = 0.7, color = "black") +
      geom_vline(xintercept = s$mean, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = s$target, color = "green", linetype = "dashed", size = 1) +
      annotate("text", x = s$mean, y = Inf, label = "Avg", color = "red", vjust = 1.2, hjust = -0.1) +
      annotate("text", x = s$target, y = Inf, label = "Target", color = "green", vjust = 1.2, hjust = -0.1) +
      labs(
        title = "Distribution of Values",
        x = "Value",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank()
      )
  })
  
  # Initialize the analysis on app start
  observe({
    updateActionButton(session, "update_analysis")
  })
}

shinyApp(ui = ui, server = server)
