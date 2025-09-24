server <- function(input, output, session) {
  
  # Generate fish data for 25 cells (5x5 grid)
  
  set.seed(runif(1,1,2000000))  # For reproducible results

  fish_data <- data.frame(
    cell_id = 1:25,
    row = rep(1:5, each = 5),
    col = rep(1:5, times = 5),
    fish_count = sample(0:100, 25, replace = TRUE,prob = (c(0.05,rep(((1-0.05)/100),100)))),
    stringsAsFactors = FALSE
  )
  
  fish_data$fish_count<-round((fish_data$fish_count/sum(fish_data$fish_count))*1000,0)
  
  fish_data_use<-reactiveVal(fish_data)
  pop_samples<-reactiveVal(data.frame(Sampled_pop="",True_pop=""))
  
  
  #Set-up fishing cells with user defined population size
  observeEvent(input$pick_pop, {
    #set.seed(runif(1,1,2000000))
    fish_data <- data.frame(
      cell_id = 1:25,
      row = rep(1:5, each = 5),
      col = rep(1:5, times = 5),
      fish_count = sample(0:100, 25, replace = TRUE,prob = (c(input$zero_cells,rep(((1-input$zero_cells)/100),100)))),
      stringsAsFactors = FALSE
    )
    fish_data$fish_count<-round((fish_data$fish_count/sum(fish_data$fish_count))*input$pop_size,0)
    fish_data_use(fish_data)
    })
  
  
  # Reactive values to store selected cells
  selected_cells <- reactiveVal(sample(1:25,5))  # Initial selection
  
    # Initial selection
  
  observeEvent(input$random_cells, {
    random_choice <- sample(1:25,input$cell_num)
    selected_cells(random_choice)
#    output$random.cells.out<-renderText({
#      random_choice
#    })
  })

  observeEvent(input$fish_hot, {
    fish.dat<-fish_data_use()
    hot_choice <- sort(fish.dat$fish_count,decreasing=TRUE,index.return=TRUE)$ix[1:input$cell_num]
    selected_cells(hot_choice)
  })
  
  # Handle plot clicks
  observeEvent(input$plot_click, {
    # Convert plot coordinates to cell ID
    x <- round(input$plot_click$x)
    y <- round(input$plot_click$y)
    
    # Check if click is within grid bounds
    if(x >= 1 && x <= 5 && y >= 1 && y <= 5) {
      # Convert to cell ID (remember y is flipped in the plot)
      row_num <- 6 - y  # Flip y coordinate
      cell_id <- (row_num - 1) * 5 + x
      
      # Toggle cell selection
      current_selection <- selected_cells()
      if(cell_id %in% current_selection) {
        # Remove from selection
        new_selection <- current_selection[current_selection != cell_id]
      } else {
        # Add to selection
        new_selection <- c(current_selection, cell_id)
      }
      selected_cells(sort(new_selection))
    }
  })
  
  # Select all cells
  observeEvent(input$select_all, {
    selected_cells(1:25)
  })
  
  # Clear all selections
  observeEvent(input$clear_all, {
    selected_cells(numeric(0))
  })
  
  # Display selected cells
  output$selected_cells_display <- renderText({
    sel_cells <- selected_cells()
    if(length(sel_cells) == 0) {
      return("No cells selected")
    }
    paste("Cells:", paste(sel_cells, collapse = ", "))
  })
  
  # Reactive data for selected cells
  selected_fish_data <- reactive({
    sel_cells <- selected_cells()
    if(length(sel_cells) == 0) {
      return(data.frame())
    }
    fish.dat<-fish_data_use()
    fish.dat[fish.dat$cell_id %in% sel_cells, ]
  })
  
  # Population statistics (all cells)
  population_summary <- reactive({
    fish.dat<-fish_data_use()
    list(
      total_fish = sum(fish.dat$fish_count),
      mean_fish = round(mean(fish.dat$fish_count), 2),
      median_fish = median(fish.dat$fish_count),
      min_fish = min(fish.dat$fish_count),
      max_fish = max(fish.dat$fish_count),
      sd_fish = round(sd(fish.dat$fish_count), 2),
      cells_total = 25
    )
  })
  
  # Sample statistics (selected cells)
  sample_summary <- reactive({
    selected_data <- selected_fish_data()
    
    if(nrow(selected_data) == 0) {
      return(NULL)
    }
  
    list(
      total_fish = sum(selected_data$fish_count),
      mean_fish = round(mean(selected_data$fish_count), 2),
      median_fish = median(selected_data$fish_count),
      min_fish = min(selected_data$fish_count),
      max_fish = max(selected_data$fish_count),
      sd_fish = round(sd(selected_data$fish_count), 2),
      cells_sampled = nrow(selected_data)
    )
  })
  
  # Grid plot with click functionality
  output$grid_plot <- renderPlot({
    # Create colors based on selection
    fish.dat<-fish_data_use()
    sel_cells <- selected_cells()
    colors <- ifelse(fish.dat$cell_id %in% sel_cells, "lightblue", "lightgray")
    
    par(mar = c(3, 3, 2, 1))
    plot(0, 0, type = "n", xlim = c(0.5, 5.5), ylim = c(0.5, 5.5),
         xlab = "Column", ylab = "Row", main = "",
         xaxt = "n", yaxt = "n")
    
    # Add grid lines and cell information
    for(i in 1:5) {
      for(j in 1:5) {
        cell_id <- (i-1) * 5 + j
        fish_count <- fish.dat$fish_count[cell_id]
        
        # Draw rectangle for each cell with thicker border for selected cells
        border_width <- ifelse(cell_id %in% sel_cells, 3, 1)
        border_color <- ifelse(cell_id %in% sel_cells, "darkblue", "black")
        
        rect(j-0.4, (6-i)-0.4, j+0.4, (6-i)+0.4, 
             col = colors[cell_id], border = border_color, lwd = border_width)
        
        # Add cell ID and fish count
        text(j, (6-i), paste("Cell", cell_id, "\n", fish_count, "fish"), 
             cex = 1.5, font = 2)
      }
    }
    
    # Add axis labels
    axis(1, at = 1:5, labels = 1:5)
    axis(2, at = 1:5, labels = 5:1)
    
    # Add legend
    legend("topright", legend = c("Selected", "Not Selected"), 
           fill = c("lightblue", "lightgray"), cex = 0.8)
    
    # Add instruction text
    mtext("Click on cells to toggle selection", side = 3, line = 0.5, cex = 0.8, col = "gray50")
  })
  
  # Population statistics display
  output$population_stats <- renderText({
    pop_stats <- population_summary()
    
    paste(
      "Total Cells: 25\n",
      "Total Fish:", pop_stats$total_fish, "\n",
      "Mean Fish/Cell:", pop_stats$mean_fish, "\n",
      "Median Fish/Cell:", pop_stats$median_fish, "\n",
      "Std Dev:", pop_stats$sd_fish, "\n",
      "CV:", round(pop_stats$sd_fish/pop_stats$mean_fish,2), "\n",
      "Range:", pop_stats$min_fish, "-", pop_stats$max_fish
    )
  })
  
  # Sample statistics display
  output$sample_stats <- renderText({
    sample_stats <- sample_summary()
    
    if(is.null(sample_stats)) {
      return("No cells selected for sampling")
    }
    
    paste(
      "Sampled Cells:", sample_stats$cells_sampled, "\n",
      "Total Fish:", sample_stats$total_fish, "\n",
      "Mean Fish/Cell:", sample_stats$mean_fish, "\n",
      "Median Fish/Cell:", sample_stats$median_fish, "\n",
      "Std Dev:", sample_stats$sd_fish, "\n",
      "CV:", round(sample_stats$sd_fish/sample_stats$mean_fish,2), "\n",
      "Range:", sample_stats$min_fish, "-", sample_stats$max_fish
    )
  })
  
  # Comparison statistics
  output$comparison_stats <- renderText({
    pop_stats <- population_summary()
    sample_stats <- sample_summary()
    
    if(is.null(sample_stats)) {
      return("No sample selected for comparison")
    }
    
    # Calculate differences and sampling coverage
    mean_diff <- sample_stats$mean_fish - pop_stats$mean_fish
    mean_error_pct <- round((mean_diff / pop_stats$mean_fish) * 100, 1)
    sampling_pct <- round((sample_stats$cells_sampled / pop_stats$cells_total) * 100, 1)
    
    paste(
      "Sampling Coverage:", sampling_pct, "%\n",
      "Mean Difference:", round(mean_diff, 2), "\n",
      "Mean Error:", mean_error_pct, "%\n",
      "Sample Representativeness:",
      ifelse(abs(mean_error_pct) < 10, "Good", 
             ifelse(abs(mean_error_pct) < 20, "Fair", "Poor"))
    )
  })
  
  #Capture index measures for chosen sampling
  observeEvent(input$save_sample, {
    #browser()
    pop_stats <- population_summary()
    sample_stats <- sample_summary()
    pop_samples_cap<-rbind(pop_samples(),c(sample_stats$mean_fish,pop_stats$mean_fish))
    #rownames(pop_samples_cap)<-c("Sampled Population","True Population")
    pop_samples(pop_samples_cap)
  })
  
  output$pop_samples_out <- renderTable({
    pop_samples()
    })
  
  # Data table for selected cells
  output$cell_table <- renderDT({
    selected_data <- selected_fish_data()
    
    if(nrow(selected_data) == 0) {
      return(data.frame(Message = "No cells selected"))
    }
    
    # Format the data for display
    display_data <- selected_data
    display_data$position <- paste("Row", display_data$row, "Col", display_data$col)
    display_data <- display_data[, c("cell_id", "position", "fish_count")]
    names(display_data) <- c("Cell ID", "Position", "Fish Count")
    
    datatable(display_data, 
              options = list(pageLength = 10, searching = FALSE),
              rownames = FALSE)
  })
}
