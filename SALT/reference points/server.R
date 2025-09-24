# Define server logic
server <- function(input, output, session) {
  
  # Reactive function to calculate control rule
  control_rule_data <- reactive({
    # Validate inputs
    req(input$b_limit, input$b_target, input$f_target, input$max_catch)
    
    # Ensure b_target > b_limit
    if (input$b_target <= input$b_limit) {
      updateNumericInput(session, "b_target", value = input$b_limit + 0.1)
    }
    
    # Create sequence of stock sizes
    stock_ratio <- seq(0, 1.2, by = 0.01)
    
    # Calculate catch based on control rule type
    catch_values <- sapply(stock_ratio, function(b) {
      if (b <= input$b_limit) {
        # Below limit: no fishing
        return(0)
      } else if (b >= input$b_target) {
        # Above target: maximum sustainable catch
        return(input$max_catch)
      } else {
        # Between limit and target: depends on rule type
        ratio <- (b - input$b_limit) / (input$b_target - input$b_limit)
        
        if (input$rule_type == "linear") {
          return(input$max_catch * ratio)
        } else if (input$rule_type == "hockey") {
          return(ifelse(ratio > 0.5, input$max_catch, input$max_catch * ratio * 2))
        } else if (input$rule_type == "smooth") {
          # Smooth S-curve transition
          smooth_ratio <- 1 / (1 + exp(-10 * (ratio - 0.5)))
          return(input$max_catch * smooth_ratio)
        }
      }
    })
    
    data.frame(
      stock_ratio = stock_ratio,
      catch = catch_values
    )
  })
  
  # Generate the control rule plot
  output$control_rule_plot <- renderPlot({
    data <- control_rule_data()
    
    p <- ggplot(data, aes(x = stock_ratio, y = catch)) +
      geom_line(color = "blue", size = 2) +
      geom_vline(xintercept = input$b_limit, 
                 color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = input$b_target, 
                 color = "green", linetype = "dashed", size = 1) +
      geom_vline(xintercept = input$current_stock, 
                 color = "orange", linetype = "solid", size = 1.5) +
      geom_hline(yintercept = 0, color = "black", linetype = "solid", alpha = 0.3) +
      
      # Add reference point labels
      annotate("text", x = input$b_limit, y = input$max_catch * 0.9, 
               label = paste("B_limit =", input$b_limit), 
               color = "red", hjust = -0.1) +
      annotate("text", x = input$b_target, y = input$max_catch * 0.8, 
               label = paste("B_target =", input$b_target), 
               color = "green", hjust = -0.1) +
      annotate("text", x = input$current_stock, y = input$max_catch * 0.7, 
               label = "Current\nStock", 
               color = "orange", hjust = 0.5) +
      
      # Styling
      labs(
        title = paste("Fisheries Control Rule -", stringr::str_to_title(input$rule_type), "Type"),
        x = "Relative Stock Size (B/B₀)",
        y = "Relative Catch",
        subtitle = "Red = Limit Reference Point, Green = Target Reference Point, Orange = Current Stock"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray60"),
        panel.grid.minor = element_blank()
      ) +
      xlim(0, 1.2) +
      ylim(0, input$max_catch * 1.1)
    
    # Add zone coloring
    p <- p + 
      annotate("rect", xmin = 0, xmax = input$b_limit, 
               ymin = 0, ymax = input$max_catch * 1.1, 
               alpha = 0.1, fill = "red") +
      annotate("rect", xmin = input$b_limit, xmax = input$b_target, 
               ymin = 0, ymax = input$max_catch * 1.1, 
               alpha = 0.1, fill = "yellow") +
      annotate("rect", xmin = input$b_target, xmax = 1.2, 
               ymin = 0, ymax = input$max_catch * 1.1, 
               alpha = 0.1, fill = "green")
    
    print(p)
  })
  
  # Generate stock status summary
  output$stock_status <- renderText({
    current_catch <- control_rule_data() %>%
      filter(abs(stock_ratio - input$current_stock) == min(abs(stock_ratio - input$current_stock))) %>%
      pull(catch) %>%
      first()
    
    status <- if (input$current_stock <= input$b_limit) {
      "CRITICAL - Below Limit Reference Point"
    } else if (input$current_stock < input$b_target) {
      "CAUTIOUS - Between Limit and Target"
    } else {
      "HEALTHY - Above Target Reference Point"
    }
    
    paste0(
      "Current Stock Status: ", status, "\n",
      "Current Stock Size (B/B₀): ", round(input$current_stock, 3), "\n",
      "Recommended Catch: ", round(current_catch, 3), " (relative units)\n",
      "Limit Reference Point: ", input$b_limit, "\n",
      "Target Reference Point: ", input$b_target, "\n\n",
      "Management Zones:\n",
      "• RED (0 - ", input$b_limit, "): Overfished - No fishing allowed\n",
      "• YELLOW (", input$b_limit, " - ", input$b_target, "): Rebuilding - Reduced fishing\n",
      "• GREEN (", input$b_target, "+): Healthy - Full fishing allowed"
    )
  })
}