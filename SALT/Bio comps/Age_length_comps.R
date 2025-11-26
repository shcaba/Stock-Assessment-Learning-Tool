library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)

# Helper functions for fisheries calculations
von_bertalanffy <- function(age, Linf, K, t0 = 0) {
  Linf * (1 - exp(-K * (age - t0)))
}

calc_selectivity <- function(lengths, L50_asc, L95_asc, peak_length, desc_sd) {
  
  # Ascending limb parameters (logistic)
  slope_asc <- log(19) / (L95_asc - L50_asc)  # 19 = ln(0.95/0.05)
 
  # Calculate selectivity for each length
  selectivity <- numeric(length(lengths))
  
  for (i in 1:length(lengths)) {
    L <- lengths[i]
    
    # Ascending limb (logistic)
    sel_asc <- 1 / (1 + exp(-slope_asc * (L - L50_asc)))
    
    # Descending limb (normal/Gaussian curve)
    # Normal distribution with peak at peak_length and standard deviation desc_sd
    sel_desc <- exp(-0.5 * ((L - peak_length) / desc_sd)^2)
    
    # Combined selectivity
    # Use minimum for lengths before peak, normal curve after peak
    if (L <= peak_length) {
      selectivity[i] <- sel_asc
    } else {
      selectivity[i] <- sel_desc
    }
  }
  
  return(selectivity)
}

calculate_population <- function(ages, Linf, K, t0, M, R0, F_mort = 0, L50_asc, L95_asc, peak_length, desc_sd) {
  # Calculate lengths at age using von Bertalanffy growth
  lengths <- von_bertalanffy(ages, Linf, K, t0)
 
  # Calculate selectivity
  if (F_mort > 0) {
    selectivity <- calc_selectivity(
      length = lengths,
      L50_asc, 
      L95_asc, 
      peak_length, 
      desc_sd)
  } else {
    selectivity <- rep(0, length(lengths))
  }
  
  # Calculate total mortality
  Z <- M + F_mort * selectivity
  
  # Calculate numbers at age (exponential decay)
  numbers <- R0 * exp(-cumsum(c(0, Z[-length(Z)])))
  
  data.frame(
    age = ages,
    length = lengths,
    numbers = numbers,
    selectivity = selectivity,
    mortality = Z
  )
}

calculate_stock_status <- function(fished_pop, unfished_pop) {
  # Spawning biomass (assuming weight proportional to length^3 and maturity at 50% Linf)
  fished_pop$weight <- fished_pop$length^3
  unfished_pop$weight <- unfished_pop$length^3
  
  # Assume 50% maturity at 50% of Linf
  maturity_length <- max(fished_pop$length) * 0.5
  fished_pop$mature <- ifelse(fished_pop$length >= maturity_length, 1, 0)
  unfished_pop$mature <- ifelse(unfished_pop$length >= maturity_length, 1, 0)
  
  # Calculate spawning biomass
  SSB_fished <- sum(fished_pop$numbers * fished_pop$weight * fished_pop$mature, na.rm = TRUE)
  SSB_unfished <- sum(unfished_pop$numbers * unfished_pop$weight * unfished_pop$mature, na.rm = TRUE)
  
  # Calculate total biomass
  B_fished <- sum(fished_pop$numbers * fished_pop$weight, na.rm = TRUE)
  B_unfished <- sum(unfished_pop$numbers * unfished_pop$weight, na.rm = TRUE)
  
  list(
    SSB_ratio = SSB_fished / SSB_unfished,
    B_ratio = B_fished / B_unfished,
    SSB_fished = SSB_fished,
    SSB_unfished = SSB_unfished,
    B_fished = B_fished,
    B_unfished = B_unfished
  )
}

ui <- page_sidebar(
  title = "Fish Population Structure & Stock Assessment",
  
  sidebar = sidebar(
    width = 350,
    
    card(
      card_header("Growth Parameters"),
      numericInput("Linf", "Asymptotic Length (Linf)", value = 100, min = 50, max = 200, step = 5),
      numericInput("K", "Growth Coefficient (K)", value = 0.2, min = 0.05, max = 0.5, step = 0.01),
      numericInput("t0", "Theoretical Age at Length 0 (t0)", value = -0.5, min = -2, max = 0, step = 0.1)
    ),
    
    card(
      card_header("Population Parameters"),
      numericInput("M", "Natural Mortality (M)", value = 0.2, min = 0.05, max = 0.5, step = 0.01),
      numericInput("R0", "Recruitment (R0)", value = 1000, min = 100, max = 10000, step = 100),
      numericInput("max_age", "Maximum Age", value = 20, min = 10, max = 50, step = 1)
    ),
    
    card(
      card_header("Fishing Parameters"),
      numericInput("F_mort", "Fishing Mortality (F)", value = 0.3, min = 0, max = 1, step = 0.01),
      numericInput("L50_asc", "L50 (50% selectivity):", value = 25, min = 0, step = 0.1),
      numericInput("L95_asc", "L95 (95% selectivity):", value = 35, min = 0, step = 0.1),
      helpText("L95 should be greater than L50 for ascending limb"),
      numericInput("peak_length", "Peak Length (mode):", value = 50, min = 0, step = 0.1),
      numericInput("desc_sd", "Standard Deviation:", value = 15, min = 0.1, step = 0.1),
      helpText("Controls the width of the descending normal curve")
    )
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    
    card(
      card_header("Age Structure"),
      plotlyOutput("age_plot")
    ),
    
    card(
      card_header("Length Structure"),
      plotlyOutput("length_plot")
    ),
    
    card(
      card_header("Sampled age compositions"),
      plotlyOutput("age_sel_plot")
    ),

        card(
      card_header("Sampled length compositions"),
      plotlyOutput("length_sel_plot")
    ),
    
    card(
      card_header("Selectivity Curve"),
      plotlyOutput("selectivity_plot")
    ),
    
    card(
      card_header("Stock Status"),
      tableOutput("stock_status"),
      br(),
      textOutput("stock_interpretation")
    )
  )
)

server <- function(input, output, session) {

  # Reactive validation and warnings
  observe({
    # Validate ascending limb
    if (input$L95_asc <= input$L50_asc) {
      showNotification("Warning: L95 should be greater than L50 for ascending limb", 
                       type = "warning", duration = 3)
    }
    
    # Check for logical peak
    if (input$peak_length <= input$L95_asc) {
      showNotification("Warning: Peak length should be greater than ascending L95 for proper dome shape", 
                       type = "warning", duration = 3)
    }
    
    # Check standard deviation
    if (input$desc_sd <= 0) {
      showNotification("Warning: Standard deviation must be positive", 
                       type = "warning", duration = 3)
    }
  })
  
  # Calculate L50 and L95 equivalents for the descending limb
  desc_params <- reactive({
    # For a normal curve, calculate where selectivity drops to 50% and 5% (equivalent to 95% on ascending)
    # These occur at distances from the peak
    L50_desc <- input$peak_length + input$desc_sd * sqrt(2 * log(2))  # ~0.83 * sd from peak
    L05_desc <- input$peak_length + input$desc_sd * sqrt(2 * log(20))  # ~2.45 * sd from peak
    
    list(L50 = L50_desc, L05 = L05_desc)
  })
  
  # Reactive calculations
  populations <- reactive({
    
    ages <- 1:input$max_age
    
    # Calculate unfished population
    unfished <- calculate_population(
      ages = ages,
      Linf = input$Linf,
      K = input$K,
      t0 = input$t0,
      M = input$M,
      R0 = input$R0,
      F_mort = 0
    )
    
    # Calculate fished population
    fished <- calculate_population(
      ages = ages,
      Linf = input$Linf,
      K = input$K,
      t0 = input$t0,
      M = input$M,
      R0 = input$R0,
      F_mort = input$F_mort,
      L50_asc=input$L50_asc, 
      L95_asc=input$L95_asc, 
      peak_length=input$peak_length, 
      desc_sd=input$desc_sd
    )
    
    list(unfished = unfished, fished = fished)
  })
  
  stock_status <- reactive({
    pops <- populations()
    calculate_stock_status(pops$fished, pops$unfished)
  })
  
  # Age structure plot
  output$age_plot <- renderPlotly({
    pops <- populations()
    
    plot_data <- rbind(
      data.frame(pops$unfished, population = "Unfished"),
      data.frame(pops$fished, population = "Fished")
    )
    
    p <- ggplot(plot_data, aes(x = age, y = numbers, color = population)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Unfished" = "#2E86C1", "Fished" = "#E74C3C")) +
      labs(x = "Age", y = "Number of Fish", title = "Population Age Structure") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Age compositions
  output$age_sel_plot <- renderPlotly({
    pops <- populations()
    
    plot_data <- rbind(
      data.frame(age=pops$unfished$age,prop=(pops$unfished$numbers*pops$fished$selectivity)/sum(pops$unfished$numbers*pops$fished$selectivity), population = "Unfished"),
      data.frame(age=pops$fished$age,prop=(pops$fished$numbers*pops$fished$selectivity)/sum(pops$fished$numbers*pops$fished$selectivity), population = "Fished")
    )
    
    p <- ggplot(plot_data, aes(x = age, y = prop, color = population)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Unfished" = "#2E86C1", "Fished" = "#E74C3C")) +
      labs(x = "Age", y = "Proportion", title = "Sampled Age Structure") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  
  # Length structure plot
  output$length_plot <- renderPlotly({
    pops <- populations()
    plot_data <- rbind(
      data.frame(pops$unfished, population = "Unfished"),
      data.frame(pops$fished, population = "Fished")
    )
    
    p <- ggplot(plot_data, aes(x = length, y = numbers, color = population)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Unfished" = "#2E86C1", "Fished" = "#E74C3C")) +
      labs(x = "Length", y = "Number of Fish", title = "Population Length Structure") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Length structure plot
  output$length_sel_plot <- renderPlotly({

    pops <- populations()
    
    plot_data <- rbind(
      data.frame(length=pops$unfished$length,prop=(pops$unfished$numbers*pops$fished$selectivity)/sum(pops$unfished$numbers*pops$fished$selectivity), population = "Unfished"),
      data.frame(length=pops$fished$length,prop=(pops$fished$numbers*pops$fished$selectivity)/sum(pops$fished$numbers*pops$fished$selectivity), population = "Fished")
    )
    
    p <- ggplot(plot_data, aes(x = length, y = prop, color = population)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Unfished" = "#2E86C1", "Fished" = "#E74C3C")) +
      labs(x = "Length", y = "Proportion", title = "Sampled Length Structure") +
      theme_minimal() +
      theme(legend.position = "bottom")+
      geom_vline(xintercept = input$Linf)
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  
  # Selectivity plot
  output$selectivity_plot <- renderPlot({

    ages <- 1:input$max_age
    lengths <- von_bertalanffy(ages, input$Linf, input$K, input$t0)
    selectivity <- calc_selectivity(
        length = lengths,
        L50_asc=input$L50_asc, 
        L95_asc=input$L95_asc, 
        peak_length=input$peak_length, 
        desc_sd=input$desc_sd)
    
    sel_data<-data.frame(Length=lengths,Selectivity=selectivity)
    
    desc_vals <- desc_params()
    
    p<-ggplot(sel_data, aes(x = Length, y = Selectivity)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed", alpha = 0.6, color = "red") +
      geom_vline(xintercept = c(input$L50_asc, input$L95_asc), 
                 linetype = "dashed", alpha = 0.6, color = "darkgreen") +
      geom_vline(xintercept = input$peak_length, 
                 linetype = "solid", alpha = 0.8, color = "purple", size = 1) +
      geom_vline(xintercept = c(desc_vals$L50, desc_vals$L05), 
                 linetype = "dashed", alpha = 0.6, color = "orange") +
      labs(
        title = "Double-Normal Dome-Shaped Selectivity Curve",
        subtitle = paste("Peak selectivity at length", input$peak_length, "| Descending SD =", input$desc_sd),
        x = "Length",
        y = "Selectivity",
        caption = "Green lines: Ascending limb | Purple line: Peak | Orange lines: Descending normal curve (50% & 5%)"
      ) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom"
      )
    p
  })
  
  # Stock status table
  output$stock_status <- renderTable({
    status <- stock_status()
    
    data.frame(
      Metric = c("Spawning Biomass Ratio (SSB/SSB0)", 
                 "Total Biomass Ratio (B/B0)",
                 "Spawning Biomass (Fished)",
                 "Spawning Biomass (Unfished)",
                 "Total Biomass (Fished)",
                 "Total Biomass (Unfished)"),
      Value = c(round(status$SSB_ratio, 3),
                round(status$B_ratio, 3),
                round(status$SSB_fished, 0),
                round(status$SSB_unfished, 0),
                round(status$B_fished, 0),
                round(status$B_unfished, 0))
    )
  }, striped = TRUE, hover = TRUE)
  
  # Stock status interpretation
  output$stock_interpretation <- renderText({
    status <- stock_status()
    ssb_ratio <- status$SSB_ratio
    
    if (ssb_ratio > 0.4) {
      interpretation <- "Stock Status: HEALTHY - Spawning biomass is above sustainable levels."
    } else if (ssb_ratio > 0.2) {
      interpretation <- "Stock Status: CAUTION - Spawning biomass is at moderate risk levels."
    } else {
      interpretation <- "Stock Status: OVERFISHED - Spawning biomass is below sustainable levels."
    }
    
    paste(interpretation, 
          sprintf("\n\nThe fished population has %.1f%% of the unfished spawning biomass.", 
                  ssb_ratio * 100))
  })
  
  # Update L95 to be greater than L50
#  observeEvent(input$L50, {
#    if (input$L95 <= input$L50) {
#      updateNumericInput(session, "L95", value = input$L50 + 20)
#    }
#  })
}

shinyApp(ui = ui, server = server)
