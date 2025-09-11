#Draw selectivity curve

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

ui <- page_sidebar(
  title = "Interactive Selectivity Curve Designer",
  sidebar = sidebar(
    h4("Length Bin Settings"),
    numericInput("min_length", "Minimum Length:", 
                 value = 0, min = 0, step = 1),
    numericInput("max_length", "Maximum Length:", 
                 value = 100, min = 1, step = 1),
    numericInput("bin_size", "Bin Size:", 
                 value = 5, min = 1, max = 10, step = 1),
    actionButton("update_bins", "Update Bins", class = "btn-primary"),
    br(), br(),
    
    h4("Instructions"),
    p("Click on the plot to add points and create your selectivity curve."),
    br(),
    actionButton("clear", "Clear All Points", class = "btn-warning"),
    br(), br(),
    actionButton("smooth", "Apply Smoothing", class = "btn-info"),
    br(), br(),
    
    h4("Curve Properties"),
    textOutput("num_points"),
    textOutput("bin_info"),
    br(),
    downloadButton("download_data", "Download Curve Data", class = "btn-success")
  ),
  
  card(
    card_header("Draw Your Selectivity Curve"),
    card_body(
      plotOutput("selectivity_plot", 
                 click = "plot_click",
                 height = "500px"),
      br(),
      p("Click anywhere on the plot to add points. The curve represents the probability of selection at each length bin."),
      br(),
      card(
        card_header("Length Bin Values"),
        card_body(
          DT::dataTableOutput("bin_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to store the points and bin settings
  values <- reactiveValues(
    points = data.frame(x = numeric(0), y = numeric(0)),
    smooth_curve = NULL,
    min_length = 0,
    max_length = 100,
    bin_size = 5,
    length_bins = NULL
  )
  
  # Initialize bins on startup
  observe({
    values$length_bins <- create_length_bins(values$min_length, values$max_length, values$bin_size)
  })
  
  # Function to create length bins
  create_length_bins <- function(min_len, max_len, bin_size) {
    bin_breaks <- seq(min_len, max_len, by = bin_size)
    if (tail(bin_breaks, 1) < max_len) {
      bin_breaks <- c(bin_breaks, max_len)
    }
    
    data.frame(
      bin_start = bin_breaks[-length(bin_breaks)],
      bin_end = bin_breaks[-1],
      bin_center = (bin_breaks[-length(bin_breaks)] + bin_breaks[-1]) / 2,
      selectivity = NA_real_
    )
  }
  
  # Update bins when user clicks update
  observeEvent(input$update_bins, {
    # Validate inputs
    if (input$max_length <= input$min_length) {
      showNotification("Maximum length must be greater than minimum length", type = "error")
      return()
    }
    
    values$min_length <- input$min_length
    values$max_length <- input$max_length
    values$bin_size <- input$bin_size
    values$length_bins <- create_length_bins(values$min_length, values$max_length, values$bin_size)
    
    # Clear existing points when bins are updated
    values$points <- data.frame(x = numeric(0), y = numeric(0))
    values$smooth_curve <- NULL
  })
  
  # Add points when user clicks on plot
  observeEvent(input$plot_click, {
    # Snap to nearest bin center
    bin_centers <- values$length_bins$bin_center
    nearest_bin <- which.min(abs(bin_centers - input$plot_click$x))
    snapped_x <- bin_centers[nearest_bin]
    
    new_point <- data.frame(
      x = snapped_x,
      y = pmax(0, pmin(1, input$plot_click$y))  # Constrain y between 0 and 1
    )
    
    # Add the new point and sort by x value
    values$points <- rbind(values$points, new_point) %>%
      arrange(x) %>%
      distinct(x, .keep_all = TRUE)  # Remove duplicates at same x
    
    # Update the corresponding bin's selectivity value
    bin_idx <- which(abs(values$length_bins$bin_center - snapped_x) < 0.001)
    if (length(bin_idx) > 0) {
      values$length_bins$selectivity[bin_idx] <- new_point$y
    }
    
    # Clear smooth curve when new point is added
    values$smooth_curve <- NULL
  })
  
  # Clear all points
  observeEvent(input$clear, {
    values$points <- data.frame(x = numeric(0), y = numeric(0))
    values$smooth_curve <- NULL
    values$length_bins$selectivity <- NA_real_
  })
  
  # Apply smoothing to the curve
  observeEvent(input$smooth, {
    req(nrow(values$points) >= 3)
    
    # Create smooth curve using loess
    x_seq <- seq(values$min_length, values$max_length, length.out = 100)
    
    tryCatch({
      smooth_model <- loess(y ~ x, data = values$points, span = 0.5)
      y_smooth <- predict(smooth_model, newdata = data.frame(x = x_seq))
      y_smooth <- pmax(0, pmin(1, y_smooth))  # Constrain between 0 and 1
      
      values$smooth_curve <- data.frame(x = x_seq, y = y_smooth)
      
      # Update bin selectivities based on smooth curve
      for (i in 1:nrow(values$length_bins)) {
        bin_center <- values$length_bins$bin_center[i]
        closest_idx <- which.min(abs(x_seq - bin_center))
        values$length_bins$selectivity[i] <- y_smooth[closest_idx]
      }
      
    }, error = function(e) {
      showNotification("Need at least 3 points to apply smoothing", type = "warning")
    })
  })
  
  # Main plot
  output$selectivity_plot <- renderPlot({
    p <- ggplot() +
      xlim(values$min_length, values$max_length) +
      ylim(0, 1) +
      labs(x = "Length", y = "Selectivity (Probability)", 
           title = "Click to Draw Your Selectivity Curve") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      ) +
      geom_hline(yintercept = c(0, 0.5, 1), linetype = "dashed", alpha = 0.3)
    
    # Add vertical lines for bin centers
    if (!is.null(values$length_bins)) {
      p <- p + geom_vline(xintercept = values$length_bins$bin_center, 
                          linetype = "dotted", alpha = 0.4, color = "gray50")
    }
    
    # Add smooth curve if available
    if (!is.null(values$smooth_curve)) {
      p <- p + geom_line(data = values$smooth_curve, 
                         aes(x = x, y = y), 
                         color = "blue", size = 1.2, alpha = 0.7)
    }
    
    # Add user points and connecting lines
    if (nrow(values$points) > 0) {
      if (nrow(values$points) > 1) {
        p <- p + geom_line(data = values$points, 
                           aes(x = x, y = y), 
                           color = "red", size = 1, alpha = 0.8)
      }
      
      p <- p + geom_point(data = values$points, 
                          aes(x = x, y = y), 
                          color = "red", size = 4, alpha = 0.8)
    }
    
    p
  })
  
  # Display number of points and bin info
  output$num_points <- renderText({
    paste("Points added:", nrow(values$points))
  })
  
  output$bin_info <- renderText({
    if (!is.null(values$length_bins)) {
      paste("Total bins:", nrow(values$length_bins), 
            "| Bin size:", values$bin_size)
    }
  })
  
  # Display bin table
  output$bin_table <- DT::renderDataTable({
    if (!is.null(values$length_bins)) {
      display_data <- values$length_bins %>%
        mutate(
          Length_Range = paste(bin_start, "-", bin_end),
          Bin_Center = round(bin_center, 2),
          Selectivity = round(selectivity, 3)
        ) %>%
        select(Length_Range, Bin_Center, Selectivity)
      
      DT::datatable(display_data, 
                    options = list(pageLength = 10, scrollY = "300px", scrollCollapse = TRUE),
                    rownames = FALSE)
    }
  })
  
  # Download handler for curve data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("selectivity_curve_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$smooth_curve)) {
        # Include both smooth curve and bin data
        write.csv(list(
          smooth_curve = values$smooth_curve,
          bin_data = values$length_bins
        ), file, row.names = FALSE)
      } else if (!is.null(values$length_bins)) {
        write.csv(values$length_bins, file, row.names = FALSE)
      } else {
        # Create empty file if no data
        write.csv(data.frame(x = numeric(0), y = numeric(0)), file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui = ui, server = server)
