
server <- function(input, output, session) {
  
  # Reactive values to store selectivity data
  values <- reactiveValues(
    selectivity_data = NULL,
    smooth_curve = NULL
  )

  # Initialize or update selectivity data when parameters change
  observe({
    if (input$bin_type == "length") {
      #bins <- seq(input$min_length, input$max_length, length.out = input$n_bins + 1)
      bins <- seq(input$min_length, input$max_length, by = input$n_bins)
      bin_mids <- (bins[-1] + bins[-length(bins)]) / 2
      bin_labels <- paste0(round(bins[-length(bins)], 1), "-", round(bins[-1], 1))
    } else {
      #bins <- seq(input$min_age, input$max_age, length.out = input$n_bins + 1)
      bins <- seq(input$min_age, input$max_age, by = input$n_bins)
      bin_mids <- (bins[-1] + bins[-length(bins)]) / 2
      bin_labels <- paste0(round(bins[-length(bins)], 1), "-", round(bins[-1], 1))
    }
    
    # Initialize with default values if data doesn't exist or dimensions changed
    #if (is.null(values$selectivity_data) || nrow(values$selectivity_data) != input$n_bins) {
    if (is.null(values$selectivity_data) || nrow(values$selectivity_data) != (length(bins)-1)) {
      values$selectivity_data <- data.frame(
        bin = 1:(length(bins)-1),
        bin_mid = bin_mids,
        bin_label = bin_labels,
        #selectivity = rep(0.5, input$n_bins),
        selectivity = rep(0.5, (length(bins)-1)),
        bin_type = input$bin_type
      )
    } else {
      # Update existing data with new bin information
      values$selectivity_data$bin_mid <- bin_mids
      values$selectivity_data$bin_label <- bin_labels
      values$selectivity_data$bin_type <- input$bin_type
    }
  })
  
  # Generate sliders for each bin
  output$selectivity_sliders <- renderUI({
    req(values$selectivity_data)
    
    slider_list <- lapply(1:nrow(values$selectivity_data), function(i) {
      bin_data <- values$selectivity_data[i, ]
      sliderInput(
        inputId = paste0("sel_", i),
        label = paste("Bin", i, ":", bin_data$bin_label),
        min = 0,
        max = 1,
        value = bin_data$selectivity,
        step = 0.01,
        width = "100%"
      )
    })
    
    do.call(tagList, slider_list)
  })
  
  # Update selectivity data when sliders change
  observe({
    req(values$selectivity_data)
    
    for (i in 1:nrow(values$selectivity_data)) {
      slider_value <- input[[paste0("sel_", i)]]
      if (!is.null(slider_value)) {
        values$selectivity_data$selectivity[i] <- slider_value
      }
    }
  })
  
  # Handle plot clicks to update selectivity values
  observeEvent(input$plot_click, {
    req(values$selectivity_data)
    
    # Find closest bin to click
    click_x <- input$plot_click$x
    closest_bin <- which.min(abs(values$selectivity_data$bin_mid - click_x))
    
    # Update selectivity value based on y-coordinate
    new_selectivity <- max(0, min(1, input$plot_click$y))
    values$selectivity_data$selectivity[closest_bin] <- new_selectivity
    
    # Update the corresponding slider
    updateSliderInput(
      session,
      paste0("sel_", closest_bin),
      value = new_selectivity
    )
  })
  
  # Preset functions
  observeEvent(input$preset_logistic, {
    req(values$selectivity_data)
    
    # Logistic curve: low selectivity for small sizes/ages, high for large
    x <- values$selectivity_data$bin_mid
    x_norm <- (x - min(x)) / (max(x) - min(x))  # Normalize to 0-1
    logistic_sel <- 1 / (1 + exp(-10 * (x_norm - 0.5)))
    
    values$selectivity_data$selectivity <- logistic_sel
    
    # Update all sliders
    for (i in 1:nrow(values$selectivity_data)) {
      updateSliderInput(session, paste0("sel_", i), value = logistic_sel[i])
    }
  })
  
  observeEvent(input$preset_dome, {
    req(values$selectivity_data)

    # Dome-shaped curve: low at extremes, high in middle
    n <- nrow(values$selectivity_data)
    dome_sel <- dnorm(1:n, mean = n/2, sd = n/4)
    dome_sel <- dome_sel / max(dome_sel)  # Normalize to 0-1
    
    values$selectivity_data$selectivity <- dome_sel
    
    # Update all sliders
    for (i in 1:nrow(values$selectivity_data)) {
      updateSliderInput(session, paste0("sel_", i), value = dome_sel[i])
    }
  })
  
  observeEvent(input$preset_flat, {
    req(values$selectivity_data)
    
    # Flat-top curve: low for first few bins, then high
    n <- nrow(values$selectivity_data)
    flat_sel <- c(rep(0, ceiling(n/3)), rep(1.0, n - ceiling(n/3)))
    flat_sel <- flat_sel[1:n]  # Ensure correct length
    
    values$selectivity_data$selectivity <- flat_sel
    
    # Update all sliders
    for (i in 1:nrow(values$selectivity_data)) {
      updateSliderInput(session, paste0("sel_", i), value = flat_sel[i])
    }
  })
  
  observeEvent(input$reset_all, {
    req(values$selectivity_data)
    
    values$selectivity_data$selectivity <- rep(0.5, nrow(values$selectivity_data))
    values$smooth_curve<-NULL
    # Update all sliders
    for (i in 1:nrow(values$selectivity_data)) {
      updateSliderInput(session, paste0("sel_", i), value = 0.5)
    }
  })
  
  # Apply smoothing to the curve
  observeEvent(input$smooth, {
    req(length(values$selectivity_data$selectivity) >= 3)
    # Create smooth curve using loess
    x_seq <- seq(input$min_length, input$max_length, length.out = 200)

    tryCatch({
      smooth_model <- loess(selectivity ~ bin_mid, data = values$selectivity_data, span = 0.5)
      y_smooth <- predict(smooth_model, newdata = data.frame(bin_mid = x_seq))
      y_smooth <- pmax(0, pmin(1, y_smooth))  # Constrain between 0 and 1
      
      values$smooth_curve <- data.frame(x = x_seq, y = y_smooth)
      
      # Update bin selectivities based on smooth curve
      for (i in 1:nrow(values$selectivity_data)) {
        bin_center <- values$selectivity_data$bin_mid[i]
        closest_idx <- which.min(abs(x_seq - bin_center))
        values$selectivity_data$selectivity[i] <- y_smooth[closest_idx]
      }
      
    }, error = function(e) {
      showNotification("Need at least 3 points to apply smoothing", type = "warning")
    })
  })
  
  
  # Create the selectivity plot
  output$selectivity_plot <- renderPlot({
    req(values$selectivity_data)
    
    x_label <- ifelse(input$bin_type == "length", "Length (cm)", "Age (years)")
    
    minx<-ifelse(input$bin_type == "length", input$min_length, input$min_age)
    maxx<-ifelse(input$bin_type == "length", input$max_length, input$max_age)
    
    p<-ggplot(values$selectivity_data, aes(x = bin_mid, y = selectivity)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(color = "blue", size = 3, alpha = 0.8) +
      geom_point(color = "white", size = 1.5) +
      geom_hline(yintercept=c(0,1),col="black",lwd=c(1,1.5)) +
      annotate("text", x=maxx*0.85, y=1.025, label= "maximum possible selectivity",size = 18/.pt)+
      xlim(minx,maxx)+
      scale_y_continuous(limits = c(0, 1.025), breaks = seq(0, 1, 0.2)) +
      labs(
        x = x_label,
        y = "Selectivity",
        title = paste("Selectivity Curve by", tools::toTitleCase(input$bin_type))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_line(),
        panel.grid.major = element_line(),
        legend.text = element_text(size=16)
      )+
      geom_ribbon(ymin=0, aes(ymax=selectivity,fill='Selected'), alpha=0.2) +
      geom_ribbon(aes(ymin=selectivity, fill='Not selected'), ymax=1, alpha=0.2)+
      scale_fill_manual(name='', values=c("Selected" = "green","Not selected" = "black" ))

    # Add smooth curve if available
    if (!is.null(values$smooth_curve)) {
     p <- p + geom_line(data = values$smooth_curve, 
                         aes(x = x, y = y), 
                         color = "purple", size = 1.2, alpha = 0.7)
    }
    p
      })
  
  # Download handler for selectivity data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("selectivity_curve_", input$bin_type, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$selectivity_data)
      write.csv(values$selectivity_data, file, row.names = FALSE)
    }
  )
}
