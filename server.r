library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(DT)
library(r4ss)
library(viridis)
library(reshape2)


server <- function(input, output, session) {

  # observeEvent(input$goto_manage_loop, {
  #   nav_select("navbar", "reports")
  # })
  # 
  # observeEvent(input$goto_SAdiag, {
  #   nav_select("navbar", "reports")
  # })

  
  # Navigation from landing page cards
  observeEvent(input$goto_LHP, {
    nav_select("navbar", "LHP")
    # Reactive data generation
    plot_data <- reactive({
      maxage<-5.4/input$M+0.2*(5.4/input$M)
      ages <- seq(0, maxage, by = 0.1)
      lengths <- seq(1,input$Linf+0.2*input$Linf , by = 1)
      
      # Growth curve (von Bertalanffy)
      length_at_age <- input$Linf * (1 - exp(-input$K * (ages - input$t0)))
      
      # Natural mortality
      mortality <- rep(input$M, length(ages))  # Constant mortality
      survival <- exp(-input$M * ages)  # Survival probability
      
      # Maturity ogive (logistic function)
      maturity_prob <- 1 / (1 + exp(-log(19) * (lengths - input$L50) / (input$L95 - input$L50)))
      
      # Weight-length relationship
      weight <- input$a * lengths^input$b
      
      list(
        ages = ages,
        lengths = lengths,
        length_at_age = length_at_age,
        mortality = mortality,
        survival = survival,
        maturity_prob = maturity_prob,
        weight = weight
      )
    })
    
    # Natural Mortality Plot
    output$mortality_plot <- renderPlotly({
      data <- plot_data()
      maxage<-5.4/input$M+0.2*(5.4/input$M)
      p <- ggplot(data.frame(Age = data$ages, Survival = data$survival), 
                  aes(x = Age, y = Survival)) +
        geom_line(color = "red", size = 1.2) +
        labs(title = paste("Natural mortality (M =", input$M, ")"),
             x = "Age (years)", y = "Survival Probability") +
        xlim(c(0,maxage))+
        theme_minimal()+
        annotate("text",x=maxage*0.8,y=0.9,label=paste0("Max age = ",5.4/input$M),col="red",size=unit(5, "pt"))
      
      ggplotly(p)
    })
    
    # Growth Plot
    output$growth_plot <- renderPlotly({
      data <- plot_data()
      maxage<-5.4/input$M+0.2*(5.4/input$M)
      age_at_size<-input$t0-((log(1-(c(input$L50,input$L95)/input$Linf))/input$K))
      size_at_maxage<-input$Linf * (1 - exp(-input$K * (maxage - input$t0)))
      
      p <- ggplot(data.frame(Age = data$ages, Length = data$length_at_age), 
                  aes(x = Age, y = Length)) +
        geom_line(color = "blue", size = 1.2) +
        geom_hline(yintercept = input$Linf, linetype = "dashed", color = "gray") +
        xlim(c(0,maxage))+
        geom_point(aes(x=age_at_size[1],y=input$L50),col="purple",size=4)+
        geom_point(aes(x=age_at_size[2],y=input$L95),col="purple",size=4)+
        geom_point(aes(x=5.4/input$M,y=size_at_maxage),col="red",size=4)+
        #geom_point(aes(x=c(age_at_size,5.4/input$M),y=c(input$L50,input$L95,size_at_maxage)))+
        labs(title = "von Bertalanffy Growth Curve",
             x = "Age (years)", y = "Length (cm)") +
        annotate("text", x = maxage * 0.7, y = input$Linf + 5, 
                 label = paste("L∞ =", input$Linf), color = "gray") +
        annotate("text", x = c(age_at_size+age_at_size*0.1,5.4/input$M), y = c(y=input$L50-input$L50*0.1,y=input$L95-input$L95*0.1,size_at_maxage-0.1*size_at_maxage), 
                 label = c("L50","L95","Max age"), color = c("purple","purple","red"),hjust = 0) +
        theme_minimal()
      
      ggplotly(p)
    })
    
    # Maturity Plot
    output$maturity_plot <- renderPlotly({
      data <- plot_data()
      
      p <- ggplot(data.frame(Length = data$lengths, Maturity = data$maturity_prob), 
                  aes(x = Length, y = Maturity)) +
        geom_line(color = "green", size = 1.2) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
        geom_vline(xintercept = input$L50, linetype = "dashed", color = "gray") +
        labs(title = "Proportion Mature at Length",
             x = "Length (cm)", y = "Proportion Mature") +
        ylim(0, 1) +
        theme_minimal()
      
      ggplotly(p)
    })
    
    # Weight-Length Plot
    output$weight_length_plot <- renderPlotly({
      data <- plot_data()
      
      p <- ggplot(data.frame(Length = data$lengths, Weight = data$weight), 
                  aes(x = Length, y = Weight)) +
        geom_line(color = "purple", size = 1.2) +
        labs(title = paste("Weight-Length: W =", input$a, "× L^", input$b),
             x = "Length (cm)", y = "Weight (kg)") +
        theme_minimal()
      
      ggplotly(p)
    })
  })
  
#Productivity
  # observeEvent(input$goto_productivity, {
  #   nav_select("navbar", "reports")
  # })

    observeEvent(input$goto_selectivity, {
    nav_select("navbar", "selectivity")
      
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
        })

  #Sampling abundance
  observeEvent(input$goto_abundance, {
    nav_select("navbar", "abundance")
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
    pop_samples<-reactiveVal(data.frame(Sampled="",Population=""))
    
    
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
    
    #Sample table
    output$sample_table <- renderTable({
      sample_stats <- sample_summary()
      pop_stats <- population_summary()
      sample_dt<-data.frame(Sample=c(sample_stats$cells_sampled,sample_stats$total_fish,sample_stats$mean_fish*25,sample_stats$mean_fish,sample_stats$median_fish,sample_stats$sd_fish,round(sample_stats$sd_fish/sample_stats$mean_fish,2),sample_stats$min_fish,sample_stats$max_fish),
                            Population=c(25,NA,pop_stats$total_fish,pop_stats$mean_fish,pop_stats$median_fish,pop_stats$sd_fish,round(pop_stats$sd_fish/pop_stats$mean_fish,2),pop_stats$min_fish, pop_stats$max_fish))
      rownames(sample_dt)<-c("Total Cells","Sampled Fish","Total Fish","Mean Fish/Cell","Median Fish/Cell","Std Dev","CV","Sample min","Sample max")
      sample_dt
    }, rownames = TRUE
    )
    
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
        "Total Fish Sampled:", sample_stats$total_fish, "\n",
        "Total Fish Estimated:",sample_stats$mean_fish*25 , "\n",
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
      pop_stats <- population_summary()
      sample_stats <- sample_summary()
      pop_samples_cap<-rbind(pop_samples(),c(sample_stats$mean_fish,pop_stats$mean_fish))
      #rownames(pop_samples_cap)<-c("Sampled Population","True Population")
      pop_samples(pop_samples_cap)
    })
    
    output$pop_samples_out <- renderTable({
      pop_samples()
    })
    
    #Clear the saved samples  
    observeEvent(input$clear_samples, {
      pop_samples(data.frame(Sampled="",Population=""))
    })
    
    
    # Data table for selected cells
    #  output$cell_table <- renderDT({
    #    selected_data <- selected_fish_data()
    
    #    if(nrow(selected_data) == 0) {
    #      return(data.frame(Message = "No cells selected"))
    #    }
    
    #    # Format the data for display
    #    display_data <- selected_data
    #    display_data$position <- paste("Row", display_data$row, "Col", display_data$col)
    #    display_data <- display_data[, c("cell_id", "position", "fish_count")]
    #    names(display_data) <- c("Cell ID", "Position", "Fish Count")
    
    #    datatable(display_data, 
    #              options = list(pageLength = 10, searching = FALSE),
    #              rownames = FALSE)
    #  })
    
    output$index_plot <- renderPlotly({
      #browser()
      plotdata<-pop_samples()
      if(any(plotdata>0))
      {
        plotdata<-plotdata[-1,]
        plot.index.s<-data.frame(Sample=1:length(plotdata$Sampled),Index=as.numeric(plotdata$Sampled),Type="Sampled")
        plot.index.p<-data.frame(Sample=1:length(plotdata$Population),Index=as.numeric(plotdata$Population),Type="Population")
        plot.index<-rbind(plot.index.s,plot.index.p)
        
        index_plot<-ggplot(plot.index,aes(x=Sample,y=Index,color=Type))+
          geom_point(aes(shape=Type))+
          geom_smooth(method='lm', formula= y~x, se=FALSE)+
          ylim(c(0,NA))+
          guides(color = "none")+
          theme_bw()
        
        index_plot
      }
    })
    
    
    })

  observeEvent(input$goto_ssp, {
    nav_select("navbar", "SSP")
    #Folder names
    ssp.folder.names<-c("Status20%",
                        "Status60%",
                        "Status20%_Scale40%_estCt",
                        "Status60%_Scale40%_estCt",
                        "Status40%_h_low",
                        "Status40%_h_hi",
                        "Status40%_M_low",
                        "Status40%_M_hi",
                        "Status40%_Scale40%_estCt_h_low",
                        "Status40%_Scale40%_estCt_h_hi",
                        "Status40%_Scale40%_estCt_M_low",
                        "Status40%_Scale40%_estCt_M_hi",
                        "Status_est_Scale40%_h_low",
                        "Status_est_Scale40%_h_ref",
                        "Status_est_Scale40%_h_hi",
                        "Status_est_Scale40%_M_low",
                        "Status_est_Scale40%_M_ref",
                        "Status_est_Scale40%_M_hi")
    ssp.folder.names.in.status<-ssp.folder.names[1:2]
    ssp.folder.names.in.scale<-ssp.folder.names[3:4]
    ssp.folder.names.in.prod<-ssp.folder.names[5:8]
    ssp.folder.names.in.status_prod<-ssp.folder.names[13:18]
    ssp.folder.names.in.scale_prod<-ssp.folder.names[9:12]
    
    #SSP choices for user and to be used in plots. These should read better.
    SSP_choices<-c(
      "Status 20%",
      "Status 60%",
      "Status 20%, Scale estimate through catch",
      "Status 60%, Scale estimate through catch",
      "Steepness low",
      "Steepness high",
      "M low",
      "M high",
      "Scale estimate through catch, Steepness low",
      "Scale estimate through catch, Steepness high",
      "Scale estimate through catch, M low",
      "Scale estimate through catch, M high",
      "Estimate status, M low",
      "Estimate status, M reference",
      "Estimate status, M high",
      "Estimate status, Steepness low",
      "Estimate status, Steepness reference",
      "Estimate status, Steepness high")
    
    SSP_choices.in.status<-SSP_choices[1:2]
    SSP_choices.in.scale<-SSP_choices[3:4]
    SSP_choices.in.prod<-SSP_choices[5:8]
    SSP_choices.in.status_prod<-SSP_choices[13:18]
    SSP_choices.in.scale_prod<-SSP_choices[9:12]
    
    output$SSP_model_picks_status<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_status",
        label = "Status",
        choices = SSP_choices.in.status,
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })
    
    output$SSP_model_picks_scale<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_scale",
        label = "Scale",
        choices = SSP_choices.in.scale,
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })
    
    output$SSP_model_picks_prod<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_prod",
        label = "Productivity",
        choices = SSP_choices.in.prod,
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })
    
    output$SSP_model_picks_status_prod<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_status_prod",
        label = "Status and productivity",
        choices = SSP_choices.in.status_prod,
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })
    
    output$SSP_model_picks_scale_prod<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_scale_prod",
        label = "Scale and productivity",
        choices = SSP_choices.in.scale_prod,
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })
    
    output$SSP_model_picks_grouped<-renderUI({
      pickerInput(
        inputId = "myPicker_SSP_grouped",
        label = "Organized by change in the model",
        choices = list(
          "Status"= SSP_choices.in.status,
          "Scale"= SSP_choices.in.scale,
          "Productivity"= SSP_choices.in.prod,
          "Status and productivity"= SSP_choices.in.status_prod,
          "Scale and productivity" = SSP_choices.in.scale_prod
        ),
        options = list(
          `actions-box` = TRUE,
          size = 12,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      )
    })
    
    
    
    
    
    #load model summaries
    
    
    observeEvent(req(input$run_SSP_comps),{
      load(paste0(getwd(),"/mod_summary.RDS"))  
      load(paste0(getwd(),"/Catches.RDS"))
      #myPicker_SSP<-c(input$myPicker_SSP_status,input$myPicker_SSP_scale,input$myPicker_SSP_prod,input$myPicker_SSP_status_prod,input$myPicker_SSP_scale_prod)
      #Dir_SSP<-getwd()  
      #SSP_mod_dir <-NULL
      #SSP_mod_dir[1]<-paste0(Dir_SSP,"/Models/Status40%")
      #browser()
      #for(i in 1:(length(input$myPicker_SSP)))
      #{SSP_mod_dir[i+1] <- paste0(Dir_SSP,"/Models/" ,paste(ssp.folder.names[SSP_choices%in%input$myPicker_SSP][i],collapse="_"))}
      #ssp.mod.prep<-r4ss::SSgetoutput(
      #dirvec=SSP_mod_dir,
      #  keyvec=1:length(SSP.model.list), 
      #  getcovar=FALSE
      #)
      
      #ssp_summary<- SSsummarize(ssp.mod.prep)
      
      colnames(ssp_summary$SpawnBio)[1:(length(colnames(ssp_summary$SpawnBio))-2)]<-colnames(ssp_summary$Bratio)[1:(length(colnames(ssp_summary$Bratio))-2)]<-c("Status 40%",SSP_choices)
      mod_indices<-c(2:ssp_summary$n)[SSP_choices%in%input$myPicker_SSP_grouped]
      SpawnOutput<-melt(id.vars=c("Yr"),ssp_summary$SpawnBio[-nrow(ssp_summary$SpawnBio),c(1,mod_indices,ncol(ssp_summary$SpawnBio))],value.name="Scale")
      Bratio<-melt(id.vars=c("Yr"),ssp_summary$Bratio[-nrow(ssp_summary$Bratio),c(1,mod_indices,ncol(ssp_summary$Bratio))],value.name="Status")
      #Catches<-Catches[Catches$Model%in%c("Status40%",ssp.folder.names[SSP_choices%in%input$myPicker_SSP]),]
      Catches<-as.data.frame(Catches)
      Catches<-Catches[Catches$Model%in%c("Status 40%",input$myPicker_SSP_grouped),]
      Catches$Model<-c("Status 40%",SSP_choices[SSP_choices%in%input$myPicker_SSP_grouped])
      #try(SSplotComparisons(ssp_summary, subplots=c(1,3),legendlabels = c("Status40%",input$myPicker_SSP),endyrvec=2020, ylimAdj = 1.30, new = FALSE,plot=FALSE,print=TRUE, legendloc = 'topleft',uncertainty=TRUE,plotdir=paste0(Dir_SSP,"/Comparisons"),btarg=0.4,minbthresh=0.25))
      
      #Pull future catch values
      OFL<-ssp_summary$quants[ssp_summary$quants$Label=="OFLCatch_2021",c(-(ncol(ssp_summary$quants)-1),-ncol(ssp_summary$quants))]
      colnames(OFL)<-c("Status 40%",SSP_choices)
      OFL<-OFL[c(1,mod_indices)]
      OFL.rel<-(OFL-as.numeric(OFL[1]))/as.numeric(OFL[1])
      Forecatch<-ssp_summary$quants[ssp_summary$quants$Label=="ForeCatch_2021",c(-(ncol(ssp_summary$quants)-1),-ncol(ssp_summary$quants))]
      colnames(Forecatch)<-c("Status 40%",SSP_choices)
      Forecatch<-Forecatch[c(1,mod_indices)]
      Forecatch.rel<-(Forecatch-as.numeric(Forecatch[1]))/as.numeric(Forecatch[1])
      MSY<-ssp_summary$quants[ssp_summary$quants$Label=="Dead_Catch_MSY",c(-(ncol(ssp_summary$quants)-1),-ncol(ssp_summary$quants))]
      colnames(MSY)<-c("Status 40%",SSP_choices)
      MSY<-MSY[c(1,mod_indices)]
      MSY.rel<-(MSY-as.numeric(MSY[1]))/as.numeric(MSY[1])
      Proj.rel<-rbind(MSY.rel,OFL.rel,Forecatch.rel)
      Proj.rel$metric<-c("MSY","OFL","ABC")
      Proj.rel<-Proj.rel[,-1]
      Proj.rel.gg<-melt(Proj.rel)
      Proj.rel.gg$metric<-factor(Proj.rel.gg$metric,levels=c("MSY","OFL","ABC"))
      
      #Create plots
      output$Catches <- renderPlotly({
        p_catch<-ggplot(Catches,aes(Yr,dead_bio,color=Model))+
          geom_line()+
          xlab("Year")+
          ylab("Remvoals (in biomass)")+
          ylim(0,NA)+
          scale_color_viridis_d()+
          theme_bw()+
          labs(color = "Models")+
          ggtitle("Removals (Scale measure)")+
          theme(plot.title = element_text(size = 40, face = "bold"))
        
        ggplotly(p_catch)
      })
      
      output$Scale <- renderPlotly({
        p_scale<-ggplot(SpawnOutput,aes(Yr,Scale,color=variable))+
          geom_line(lwd=1.25)+
          xlab("Year")+
          ylab("Scale (Spawning Output)")+
          ylim(0,NA)+
          scale_color_viridis_d()+
          theme_bw()+
          labs(color = "Models")+
          ggtitle("Scale")+
          theme(plot.title = element_text(size = 40, face = "bold"))
        
        ggplotly(p_scale)
      })
      
      output$Status <- renderPlotly({
        p_status<-ggplot(Bratio,aes(Yr,Status,color=variable))+
          geom_line(lwd=1.25)+
          xlab("Year")+
          ylab("Status (Size relative to unfished)")+
          ylim(0,NA)+
          scale_color_viridis_d()+
          theme_bw()+
          labs(color = "Models")+
          ggtitle("Status")+
          theme(plot.title = element_text(size = 40, face = "bold"))
        
        ggplotly(p_status)
      })
      
      output$Proj <- renderPlotly({
        p_proj<-ggplot(Proj.rel.gg,aes(variable,value*100,color=metric))+
          geom_point(aes(shape=metric),size=4)+
          xlab("Model")+
          ylab("% change relative to the 40% stock status model")+
          geom_hline(yintercept=0)+
          scale_color_viridis_d()+
          theme_bw()+
          labs(color = "Catch metric",shape="")+
          ggtitle("Projected catch")+
          theme(plot.title = element_text(size = 40, face = "bold"))+
          coord_flip()
        
        ggplotly(p_proj)
      })
      
      
      #     output$SSP_SSBcomp_plot <- renderImage({
      #     image.path<-normalizePath(file.path(paste0(Dir_SSP,"/Comparisons/compare1_spawnbio.png")),mustWork=FALSE)
      #     return(list(
      #       src = image.path,
      #       contentType = "image/png",
      #       width = 400,
      #       height = 600,
      #       style='height:60vh'))
      #   },deleteFile=FALSE)
      #  
      #   output$SSP_relSSBcomp_plot <- renderImage({
      #     image.path<-normalizePath(file.path(paste0(Dir_SSP,"/Comparisons/compare3_Bratio.png")),mustWork=FALSE)
      #     return(list(
      #       src = image.path,
      #       contentType = "image/png",
      #       width = 400,
      #       height = 600,
      #       style='height:60vh'))
      #   },deleteFile=FALSE)
    })
    
  })


  
  
#Refernce Points and Control Rules  
  observeEvent(input$goto_refpts, {
    nav_select("navbar", "refpts")
    # Reactive function to calculate control rule
    control_rule_data <- reactive({
      # Validate inputs
      req(input$b_nocatch, input$b_target, input$E_msy, 1)
      
      # Ensure b_target > b_nocatch
      #    if (input$b_target <= input$b_nocatch) {
      #      updateNumericInput(session, "b_target", value = input$b_nocatch + 0.1)
      #    }
      
      # Create sequence of stock sizes
      stock_ratio <- round(seq(0, 1, by = 0.01),2)
      data.frame(
        stock_ratio = stock_ratio,
        catch = stock_ratio*input$E_msy
      )
      
      
      # Create linear models
      
      # Calculate catch based on control rule type
      #   catch_values <- sapply(stock_ratio, function(b) {
      #     browser()
      #     if (b <= input$b_target) {
      #       # Below limit: no fishing
      #       return(stock_ratio*input$E_msy)
      #       } 
      #     else if (b >= input$b_target) {
      #       # Above target: maximum sustainable catch
      #       #return(input$max_catch)
      #       #return(1)
      #     #} else {
      #       # Between limit and target: depends on rule type
      #       #ratio <- (b - input$b_nocatch) / (input$b_target - input$b_nocatch)
      #       
      #       if(input$rule_type == "linear"){
      #         #return(input$max_catch * ratio)
      #         #return(lm(c(0,input$E_msy)~c(0,input$b_target))$coeff[2]*ratio)
      #         return(stock_ratio*input$E_msy)
      #       } 
      #       
      #       if(input$rule_type == "hockey"){
      #         #return(ifelse(ratio > 0.5, input$max_catch, input$max_catch * ratio * 2))
      #         return(ifelse(stock_ratio>=input$b_target,input$b_target*input$E_msy))
      #       } 
      #       
      #       #else if (input$rule_type == "smooth") {
      #         # Smooth S-curve transition
      #       #  smooth_ratio <- 1 / (1 + exp(-10 * (ratio - 0.5)))
      #       #  return(input$max_catch * smooth_ratio)
      #       #}
      #     }
      #   })
      # 
      #       data.frame(
      #     stock_ratio = stock_ratio,
      #     catch = catch_values
      #   )
    })
    
    # Generate the control rule plot
    output$control_rule_plot <- renderPlot({
      data <- control_rule_data()
      #Add threshhold option  
      data$thresh<-data$constant<-NA
      thresh.coefs<-coef(lm(c(0,data[data$stock_ratio==input$b_target,]$catch)~c(input$b_nocatch,input$b_target)))
      data$thresh[data$stock_ratio>=input$b_nocatch & data$stock_ratio<=input$b_target]<-thresh.coefs[2]*data$stock_ratio[data$stock_ratio>=input$b_nocatch & data$stock_ratio<=input$b_target]+thresh.coefs[1]
      data$constant[data$stock_ratio>=input$b_target]<-thresh.coefs[2]*data$stock_ratio[data$stock_ratio==input$b_target]+thresh.coefs[1]
      
      p <- ggplot(data, aes(x = stock_ratio, y = catch)) +
        geom_line(color = "blue", size = 2) +
        geom_line(aes(x = stock_ratio, y = catch*input$buffer),color = "#390878", size = 2,linetype="dotted") +
        geom_line(aes(x = stock_ratio, y = thresh),color = "orange", size = 2) +
        geom_line(aes(x = stock_ratio, y = thresh*input$buffer),color = "#390878", size = 2,linetype="dotted") +
        geom_line(aes(x = stock_ratio, y = constant),color = "#005595", size = 2) +
        geom_line(aes(x = stock_ratio, y = constant*input$buffer),color = "#390878", size = 2,linetype="dotted") +
        geom_point(aes(data[data$stock_ratio==input$current_stock,1],data[data$stock_ratio==input$current_stock,2]),size=5, color="black",fill="white")+
        #geom_abline(intercept = thresh.coefs[1],slope = thresh.coefs[2], 
        #           color = "orange", linetype = "dashed", size = 1) +
        geom_vline(xintercept = input$b_limit, 
                   color = "red", linetype = "dashed", size = 1) +
        geom_vline(xintercept = input$b_target, 
                   color = "#5D9741", linetype = "dashed", size = 1) +
        #geom_vline(xintercept = input$current_stock, 
        #           color = "orange", linetype = "solid", size = 1.5) +
        geom_hline(yintercept = 0, color = "black", linetype = "solid", alpha = 0.3) +
        coord_cartesian(clip = "off", ylim = c(-0.025*input$E_msy, input$E_msy)) +
        xlim(0, 1)+ 
        
        # Add reference point labels
        annotate("text", x = input$b_limit, y = 0.025*input$E_msy, 
                 label = paste("Limit RP =", input$b_limit), 
                 color = "red", hjust = -0.1) +
        annotate("text", x = input$b_target, y = 0.025*input$E_msy,
                 label = paste("Target RP =", input$b_target), 
                 color = "#5D9741", hjust = -0.1) +
        annotate("text", x = data$stock_ratio[93], y =input$E_msy ,
                 label = paste("Constant fishing rate"), 
                 color = "blue", hjust = 0.1) +
        annotate("text", x = data$stock_ratio[97], y = max(data$constant,na.rm = TRUE),
                 label = paste("Constant catch"), 
                 color = "#005595", vjust = -1.5) +
        annotate("text", x = input$b_nocatch, y = -0.025*input$E_msy,
                 label = paste("No catch =", input$b_nocatch), 
                 color = "black", hjust = -0.1) +
        annotate("text", x = input$current_stock, y = data[data$stock_ratio==input$current_stock,2] * 1, 
                 label = "Current Stock", 
                 color = "black", hjust = 0.5,vjust =-2.5) +
        
        # Styling
        labs(
          #title = paste("Fisheries Control Rule -", stringr::str_to_title(input$rule_type), "Type"),
          title = paste("Harvest Control Rule"),
          x = "Relative Stock Size (SB/SB₀)",
          y = "Relative Catch",
          subtitle = "Red = Limit Reference Point; Green = Target Reference Point; Black dot = Current Stock; Purple dots= Buffered catches rule"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "gray60"),
          panel.grid.minor = element_blank()
        ) 
      #ylim(0, input$E_msy)
      
      # Add zone coloring
      p <- p + 
        annotate("rect", xmin = 0, xmax = input$b_limit, 
                 ymin = 0, ymax = input$E_msy, 
                 alpha = 0.1, fill = "red") +
        annotate("rect", xmin = input$b_limit, xmax = input$b_target, 
                 ymin = 0, ymax = input$E_msy, 
                 alpha = 0.1, fill = "yellow") +
        annotate("rect", xmin = input$b_target, xmax = 1, 
                 ymin = 0, ymax = input$E_msy, 
                 alpha = 0.1, fill = "#5D9741")
      
      print(p)
    })
    
    # Generate stock status summary
    output$stock_status <- renderText({
      current_catch <- control_rule_data() %>%
        filter(abs(stock_ratio - input$current_stock) == min(abs(stock_ratio - input$current_stock))) %>%
        pull(catch) %>%
        first()
      #browser()
      
      data <- control_rule_data()
      #Add threshhold option  
      data$thresh<-data$constant<-NA
      thresh.coefs<-coef(lm(c(0,data[data$stock_ratio==input$b_target,]$catch)~c(input$b_nocatch,input$b_target)))
      data$thresh[data$stock_ratio>=input$b_nocatch & data$stock_ratio<=input$b_target]<-thresh.coefs[2]*data$stock_ratio[data$stock_ratio>=input$b_nocatch & data$stock_ratio<=input$b_target]+thresh.coefs[1]
      data$constant[data$stock_ratio>=input$b_target]<-thresh.coefs[2]*data$stock_ratio[data$stock_ratio==input$b_target]+thresh.coefs[1]
      
      curr_stock_catch<-data[data$stock_ratio==input$current_stock,]
      
      paste0(
        #"Overfishing limit at current stock size: ", round(current_catch, 3), " (relative units)\n",
        "Overfishing limit (OFL) at current stock size: ", round(curr_stock_catch[2],3), " (relative units)\n",
        "Threshhold control rule catch: ", round(curr_stock_catch[4],3), " (relative units)\n",
        "Buffered catch (e.g., ABC): ", round(curr_stock_catch[4]*input$buffer,3), " (relative units)\n",
        "Constant catch (catch at FMSY proxy at target biomass): ", round(curr_stock_catch[3]*input$buffer,3), " (relative units)\n"
      )
    })
    
    output$stock_status_RPs <- renderText({
      status <- if (input$current_stock <= input$b_nocatch) {
        "CRITICAL - Below Limit Reference Point"
      } else if (input$current_stock < input$b_target) {
        "CAUTIOUS - Between Limit and Target"
      } else {
        "HEALTHY - At or above Target Reference Point"
      }
      
      
      paste0(
        "Current Stock Size (SB/SB₀): ", round(input$current_stock, 3), "\n",
        "Current Stock Status: ", status, "\n",
        "No Catch Point: ", input$b_nocatch, "\n",
        "Limit (Overfished) Reference Point: ", input$b_limit, "\n",
        "Target Reference Point: ", input$b_target, "\n\n",
        "Management Zones:\n",
        "• RED (0 - ", input$b_limit, "): Overfished - rebuilding plan\n",
        "• YELLOW (", input$b_limit, " - ", input$b_target, "): Precautionary - Reduced fishing\n",
        "• GREEN (", input$b_target, "+): Healthy - Full fishing allowed"
      )
    })  
  })

  # observeEvent(input$goto_reports, {
  #   nav_select("navbar", "reports4")
  # })
  
  observeEvent(input$goto_baseline, {
    nav_select("navbar", "baseline")
    spp.out<-SS_output(paste0(getwd(),"/Spp_Reports/REBS_2025"))
    observe({
      
      Spp.dervout <- data.frame(Year=spp.out$timeseries$Yr,TotalB=spp.out$timeseries$Bio_all,SummaryB=spp.out$timeseries$Bio_smry,SpawnOut<-spp.out$timeseries$SpawnBio,Dep<-spp.out$timeseries$SpawnBio/spp.out$timeseries$SpawnBio[1])
      
      
      if(!any(spp.out$timeseries$Yr==input$Year_comp))
      {
        Spp.dervout.gg <- rbind(data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_all/spp.out$timeseries$Bio_all[1],Metric="Total Biomass"),
                                data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_smry/spp.out$timeseries$Bio_smry[1],Metric="Summary Biomass"),
                                data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$SpawnBio/spp.out$timeseries$SpawnBio[1],Metric="Spawning Output"))
      }
      
      if(any(spp.out$timeseries$Yr==input$Year_comp))
      {
        Spp.dervout.gg <- rbind(data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_all/spp.out$timeseries$Bio_all[spp.out$timeseries$Yr==input$Year_comp],Metric="Total Biomass"),
                                data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$Bio_smry/spp.out$timeseries$Bio_smry[spp.out$timeseries$Yr==input$Year_comp],Metric="Summary Biomass"),
                                data.frame(Year=spp.out$timeseries$Yr,Value=spp.out$timeseries$SpawnBio/spp.out$timeseries$SpawnBio[spp.out$timeseries$Yr==input$Year_comp],Metric="Spawning Output"))
      }
      output$CompPlot <- renderPlotly({
        comp1<-ggplot(Spp.dervout.gg,aes(Year,Value,col=Metric))+
          geom_line(lwd=1.25)+
          ylab("Value relative to chosen year")+
          ylim(0,NA)+
          geom_hline(yintercept=1,col="orange",linetype="dashed")+
          geom_vline(xintercept=input$Year_comp,col="orange",linetype="dashed")+
          theme_bw()
        
        ggplotly(comp1)
      })
      
      
      output$DepPlot <- renderPlotly({
        p1<-ggplot(Spp.dervout,aes(Year,Dep))+
          geom_line(lwd=1.25)+
          ylab("Relative Stock Status")+
          ylim(0,NA)+
          theme_bw()
        ggplotly(p1)
      })
      
      output$SpawnOutPlot <- renderPlotly({
        p2<-ggplot(Spp.dervout,aes(Year,SpawnOut))+
          geom_line(lwd=1.25)+
          ylab("Spawning Output")+
          ylim(0,NA)+
          theme_bw()
        ggplotly(p2)
      })
      
      output$SummaryBPlot <- renderPlotly({
        p3<-ggplot(Spp.dervout,aes(Year,SummaryB))+
          geom_line(lwd=1.25)+
          ylab("Summary Biomass")+
          ylim(0,NA)+
          theme_bw()
        ggplotly(p3)
      })
      
      
      output$TotalBPlot <- renderPlotly({
        p4<-ggplot(Spp.dervout,aes(Year,TotalB))+
          geom_line(lwd=1.25)+
          ylab("Total Biomass")+
          ylim(0,NA)+
          theme_bw()
        ggplotly(p4)
      })
      
    }) 
    
  })
  
  # Back to home buttons
  observeEvent(input$back_to_home_1, {
    nav_select("navbar", "home")
  })
  
  observeEvent(input$back_to_home_2, {
    nav_select("navbar", "home")
  })
  
  observeEvent(input$back_to_home_3, {
    nav_select("navbar", "home")
  })
  

}
