library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)

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

calculate_population <- function(ages, Linf, K, t0, M,R0=1000, F_mort = 0, L50_asc, L95_asc, peak_length, desc_sd) {
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

calculate_stock_status <- function(fished_pop, unfished_pop, L50,L95) {
  # Spawning biomass (assuming weight proportional to length^3 and maturity at 50% Linf)
  fished_pop$weight <- 0.000001*fished_pop$length^3
  unfished_pop$weight <- 0.000001*unfished_pop$length^3
  #browser()
  # Assume 50% maturity at 65% of Linf
#  maturity_length50 <- max(fished_pop$length) * 0.65
#  maturity_length95 <- max(fished_pop$length) * 0.80
#  maturity_prob <- 1 / (1 + exp(-log(19) * (lengths - input$L50) / (input$L95 - input$L50)))
#  fished_pop$mature <- ifelse(fished_pop$length >= maturity_length, 1, 0)
#  unfished_pop$mature <- ifelse(unfished_pop$length >= maturity_length, 1, 0)
  fished_pop$mature <-unfished_pop$mature <-1 / (1 + exp(-log(19) * (unfished_pop$length - L50) / (L95 - L50)))

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
      card_header("Life History Parameters"),
      fluidRow(
        column(width = 6,numericInput("M", "Natural Mortality (M)", value = 0.2, min = 0.05, max = 0.5, step = 0.01)),
        column(width = 6,numericInput("Linf", "Asymptotic Length (Linf)", value = 60, min = 5, max = 2000, step = 1))),
      fluidRow(
        column(width = 6,numericInput("K", "Growth Coefficient (K)", value = 0.13, min = 0.001, max = 2, step = 0.01)),
        column(width = 6,numericInput("t0", "Age at Size 0 (t0)", value = -1, min = -10, max = 0, step = 0.1))),
      h6("Length at Maturity"),
      fluidRow(
        column(width = 6,numericInput("L50", "L50%", value = 60*0.65, min = 0.1, max = 10000, step = 0.1)),
        column(width = 6,numericInput("L95", "L95%", value = 60*0.8, min = 0.2, max = 10000, step = 0.1)))
    ),
    
#    card(
#      card_header("Population Parameters"),
#      numericInput("M", "Natural Mortality (M)", value = 0.2, min = 0.05, max = 0.5, step = 0.01),
#      numericInput("R0", "Recruitment (R0)", value = 1000, min = 100, max = 10000, step = 100),
#      numericInput("max_age", "Maximum Age", value = 5.4/0.2, min = 1, max = 500, step = 1)
#    ),
    
    card(
      card_header("Fishing Parameters"),
      numericInput("F_mort", "Fishing Mortality (F)", value = 0.2, min = 0, max = 1, step = 0.01),
      fluidRow(
        column(width = 6,numericInput("L50_asc", "L50 (50% selectivity):", value = 30, min = 0, step = 0.1)),
        column(width = 6,numericInput("L95_asc", "L95 (95% selectivity):", value = 40, min = 0, step = 0.1))),
      helpText("Selectivity at L95 should be greater than selectivity at L50 for ascending limb"),
      fluidRow(
        column(width = 6,numericInput("peak_length", "Peak Length (mode):", value = 60, min = 0, step = 0.1)),
        column(width = 6,numericInput("desc_sd", "Standard Deviation:", value = 15, min = 0.1, step = 0.1))),
      helpText("The standard deviation controls the width of the dome."),
      helpText("To make logistic selectivity, the peak length can be made larger than the largest size in the population.")
    ),
actionButton("save_results", "Save results", class = "btn-outline-secondary",style="color: #fff; background-color: #eb860c; border-color: #eb860c"),

card(
  card_header("Reference Points"),
  fluidRow(
    column(width = 6,numericInput("TRP", "Target RP", value = 0.4, min = 0, max=1, step = 0.1)),
    column(width = 6,numericInput("LRP", "Limit RP", value = 0.25, min = 0, max=1, step = 0.1)))
)
  

  ),
  
  layout_columns(
    col_widths = c(4,4,4),
    
#    card(
#      card_header("Age Structure"),
#      plotlyOutput("age_plot")
#    ),
    
#    card(
#      card_header("Length Structure"),
#      plotlyOutput("length_plot")
#    ),
    
      card(
      card_header("Selectivity Curve"),
      plotOutput("selectivity_plot")
    ),

       card(
      card_header("Growth and Mortality"),
      plotOutput("growth_M_plot", height = "500px")
    ),
      card(
        card_header("Stock Status"),
        tableOutput("stock_status"),
        textOutput("stock_interpretation")
      )

  ),

layout_columns(
  col_widths = c(4,4,4),
        card(
      card_header("Sampled age compositions"),
      plotlyOutput("age_sel_plot")
    ),

        card(
      card_header("Sampled length compositions"),
      plotlyOutput("length_sel_plot")
    ),
  
  card(
    card_header("Sensitivity results"),
    tableOutput("results_out"),
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
    
    ages <- 0:(5.4/input$M) #Using the Cope and Hamel tmax to M relationship
    
    # Calculate unfished population
    unfished <- calculate_population(
      ages = ages,
      Linf = input$Linf,
      K = input$K,
      t0 = input$t0,
      M = input$M,
      R0 = 1000,
      F_mort = 0
    )
    
    # Calculate fished population
    fished <- calculate_population(
      ages = ages,
      Linf = input$Linf,
      K = input$K,
      t0 = input$t0,
      M = input$M,
      R0 = 1000,
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
    calculate_stock_status(pops$fished, pops$unfished,input$L50,input$L95)
  })
  
  mean_bio_comp <- reactive({
    pops <- populations()
    
    #Mean Age
    age_data_fished<-do.call(c,mapply(function(x) rep(pops$fished$age[x],round(pops$fished$numbers[x]*pops$fished$selectivity[x],0)),x=1:nrow(pops$fished),SIMPLIFY=TRUE))
    age_data_fish_df<-data.frame(age=age_data_fished,population="Fished")
    age_data_unfished<-do.call(c,mapply(function(x) rep(pops$unfished$age[x],round(pops$unfished$numbers[x]*pops$fished$selectivity[x],0)),x=1:nrow(pops$unfished),SIMPLIFY=TRUE))
    age_data_unfish_df<-data.frame(age=age_data_unfished,population="Unfished")
    mean_age_status<-mean(age_data_fish_df$age)/mean(age_data_unfish_df$age)
    
    #Mean Length
    lt_data_fished<-do.call(c,mapply(function(x) rnorm(round(pops$fished$numbers[x],0),pops$fished$length[x],pops$fished$length[x]*0.1),x=1:nrow(pops$fished),SIMPLIFY=TRUE))
    lt_data_fish_df<-data.frame(length=lt_data_fished,population="Fished")
    lt_data_unfished<-do.call(c,mapply(function(x) rnorm(round(pops$unfished$numbers[x],0),pops$unfished$length[x],pops$unfished$length[x]*0.1),x=1:nrow(pops$unfished),SIMPLIFY=TRUE))
    lt_data_unfish_df<-data.frame(length=lt_data_unfished,population="Unfished")
    mean_lt_status<-mean(lt_data_fish_df$length)/mean(lt_data_unfish_df$length)
    
    mean_bio_comp<-c(mean_age_status,mean_lt_status)
    mean_bio_comp
  })
  
  # Age structure plot
  output$age_plot <- renderPlotly({
    pops <- populations()
#      plot_data <- rbind(
#      data.frame(pops$unfished, population = "Unfished"),
#      data.frame(pops$fished, population = "Fished")
#    )
    plot_data_fished<-do.call(c,mapply(function(x) rep(pops$fished$age[x],round(pops$fished$numbers[x],0)),x=1:nrow(pops$fished),SIMPLIFY=TRUE))
    plot_data_fish_df<-data.frame(age=plot_data_fished,population="Fished")
    plot_data_unfished<-do.call(c,mapply(function(x) rep(pops$unfished$age[x],round(pops$unfished$numbers[x],0)),x=1:nrow(pops$unfished),SIMPLIFY=TRUE))
    plot_data_unfish_df<-data.frame(age=plot_data_unfished,population="Unfished")
    plot_data<-rbind(plot_data_fish_df,plot_data_unfish_df)
      
      p <- ggplot(plot_data, aes(x = age, color = population)) +
      geom_density(alpha = 0.5, size = 1,show.legend = FALSE)+
      #geom_line(size = 1.2) +
      #geom_point(size = 2) +
        #scale_fill_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
        scale_color_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
        labs(
          x = "Age (years)",
          y = "Frequency",
          fill = "Population Type",
          title = "Total Age Distribution: Fished vs Unfished"
        ) +
        theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Age compositions
  output$age_sel_plot <- renderPlotly({
    pops <- populations()

    #plot_data <- rbind(
    #  data.frame(age=pops$unfished$age,prop=(pops$unfished$numbers*pops$fished$selectivity)/sum(pops$unfished$numbers*pops$fished$selectivity), population = "Unfished"),
    #  data.frame(age=pops$fished$age,prop=(pops$fished$numbers*pops$fished$selectivity)/sum(pops$fished$numbers*pops$fished$selectivity), population = "Fished")
    #)
    
    plot_data_fished<-do.call(c,mapply(function(x) rep(pops$fished$age[x],round(pops$fished$numbers[x]*pops$fished$selectivity[x],0)),x=1:nrow(pops$fished),SIMPLIFY=TRUE))
    plot_data_fish_df<-data.frame(age=plot_data_fished,population="Fished")
    plot_data_unfished<-do.call(c,mapply(function(x) rep(pops$unfished$age[x],round(pops$unfished$numbers[x]*pops$fished$selectivity[x],0)),x=1:nrow(pops$unfished),SIMPLIFY=TRUE))
    plot_data_unfish_df<-data.frame(age=plot_data_unfished,population="Unfished")
    plot_data<-rbind(plot_data_fish_df,plot_data_unfish_df)
    
    
    p <- ggplot(plot_data, aes(x = age, color = population)) +
      geom_density(alpha = 0.5, size = 1,show.legend = FALSE)+
      #scale_fill_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
      scale_color_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
      labs(
        x = "Age (years)",
        y = "Frequency",
        fill = "Population Type",
        title = "Selected Age Distribution: Fished vs Unfished"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  
  # Total Length structure plot
  output$length_plot <- renderPlotly({
    set.seed(19)
    pops <- populations()
    plot_data <- rbind(
      data.frame(pops$unfished, population = "Unfished"),
      data.frame(pops$fished, population = "Fished")
    )

    plot_data_fished<-do.call(c,mapply(function(x) rnorm(round(pops$fished$numbers[x],0),pops$fished$length[x],pops$fished$length[x]*0.1),x=1:nrow(pops$fished),SIMPLIFY=TRUE))
    plot_data_fish_df<-data.frame(length=plot_data_fished,population="Fished")
    plot_data_unfished<-do.call(c,mapply(function(x) rnorm(round(pops$unfished$numbers[x],0),pops$unfished$length[x],pops$unfished$length[x]*0.1),x=1:nrow(pops$unfished),SIMPLIFY=TRUE))
    plot_data_unfish_df<-data.frame(length=plot_data_unfished,population="Unfished")
    plot_data<-rbind(plot_data_fish_df,plot_data_unfish_df)
    
    p <- ggplot(plot_data, aes(x = length, color = population)) +
      geom_density(alpha = 0.5, size = 1,show.legend = FALSE)+
      #geom_line(size = 1.2) +
      #geom_point(size = 2) +
      #scale_fill_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
      scale_color_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
      labs(x = "Length", y = "Frequency", title = "Total Length Distribution: Fished vs Unfished") +
      theme_minimal() +
      theme(legend.position = "bottom")+
      geom_vline(xintercept = c(input$L50,input$Linf),linetype="dashed",col=c("green","orange"))
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Selected Length structure plot
  output$length_sel_plot <- renderPlotly({
    set.seed(19)
    pops <- populations()
#     plot_data <- rbind(
#      data.frame(length=pops$unfished$length,prop=(pops$unfished$numbers*pops$fished$selectivity)/sum(pops$unfished$numbers*pops$fished$selectivity), population = "Unfished"),
#      data.frame(length=pops$fished$length,prop=(pops$fished$numbers*pops$fished$selectivity)/sum(pops$fished$numbers*pops$fished$selectivity), population = "Fished")
#    )

    plot_data_fished<-do.call(c,mapply(function(x) rnorm(round(pops$fished$numbers[x]*pops$fished$selectivity[x],0),pops$fished$length[x],pops$fished$length[x]*0.1),x=1:nrow(pops$fished),SIMPLIFY=TRUE))
    plot_data_fish_df<-data.frame(length=plot_data_fished,population="Fished")
    plot_data_unfished<-do.call(c,mapply(function(x) rnorm(round(pops$unfished$numbers[x]*pops$fished$selectivity[x],0),pops$unfished$length[x],pops$unfished$length[x]*0.1),x=1:nrow(pops$unfished),SIMPLIFY=TRUE))
    plot_data_unfish_df<-data.frame(length=plot_data_unfished,population="Unfished")
    plot_data<-rbind(plot_data_fish_df,plot_data_unfish_df)
    max.d<-max(density(plot_data$length)$y)
            
    p <- ggplot(plot_data, aes(x = length, color = population)) +
      geom_density(alpha = 0.5, size = 1,show.legend = FALSE)+
      #scale_fill_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
      scale_color_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
      labs(x = "Length", y = "Proportion", title = "Selected Length Distribution: Fished vs Unfished") +
      theme_minimal() +
      theme(legend.position = "bottom")+
      geom_vline(xintercept = c(input$L50,input$Linf),linetype="dashed",col=c("green","orange"))+
      annotate("text",x=input$L50+input$Linf*0.05,y=max.d+max.d*0.2,label="L50",col="green")+
      annotate("text",x=input$Linf+input$Linf*0.05,y=max.d+max.d*0.2,label="Linf",col="orange")
      
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  
  # Selectivity plot
  output$selectivity_plot <- renderPlot({

    ages <- 1:(5.4/input$M)
    lengths <- von_bertalanffy(ages, input$Linf, input$K, input$t0)
    selectivity <- calc_selectivity(
        length = lengths,
        L50_asc=input$L50_asc, 
        L95_asc=input$L95_asc, 
        peak_length=input$peak_length, 
        desc_sd=input$desc_sd)
    
    sel_data<-data.frame(Length=lengths,Selectivity=selectivity)
    sel.pts<-data.frame(Length=c(input$L50_asc,input$L95_asc),Prop=c(0.5,0.95))
    maturity_lengths<-1 / (1 + exp(-log(19) * (lengths - input$L50) / (input$L95 - input$L50)))
    mat.pts<-data.frame(Length=c(input$L50,input$L95),Prop=c(0.5,0.95))
    
    desc_vals <- desc_params()
    
    p<-ggplot(sel_data, aes(x = Length, y = Selectivity)) +
      geom_line(aes(color = "black"), size = 1.5) +
      geom_point(data=sel.pts,aes(x=Length,y=Prop,col="black"),fill="gray",size=5,pch=21)+
      annotate("text",sel.pts$Length-(input$Linf*0.08),sel.pts$Prop,label=c("Sel50%","Sel95%"))+
      #geom_point(aes(x=input$L95_asc,y=0.95,col="black"),fill="gray",size=4,pch=21)+
      geom_line(aes(y = maturity_lengths,col="pink"),size = 1.5)+
      geom_point(data=mat.pts,aes(x=Length,y=Prop,col="pink"),fill="pink4",size=5,pch=21)+
      annotate("text",mat.pts$Length+input$Linf*0.08,mat.pts$Prop,label=c("Lmat50%","Lmat95%"))+
      #geom_point(aes(x=input$L95,y=0.95,col="pink"),fill="pink4",size=4,pch=21)+
      #geom_point(aes(x=input$peak_length,y=1,col="purple"),fill="violet",size=4,pch=21)+
      #geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed", alpha = 0.6, color = "blue") +
      #geom_vline(xintercept = c(input$L50_asc, input$L95_asc), 
      #           linetype = "dashed", alpha = 0.6, color = "darkgreen") +
      geom_vline(xintercept = input$peak_length, 
                 linetype = "dotted", alpha = 0.8, color = "purple", size = 1)+
#      if(input$peak_length<input$Linf){geom_vline(xintercept = input$peak_length, 
 #                linetype = "solid", alpha = 0.8, color = "purple", size = 1)} +
      geom_vline(xintercept = c(desc_vals$L50, desc_vals$L05), 
                 linetype = "dashed", alpha = 0.6, color = "orange") +
      labs(
        title = "Double-Normal Dome-Shaped Selectivity Curve",
        subtitle = paste("Peak selectivity at length", input$peak_length, "| Descending SD =", input$desc_sd),
        x = "Length",
        y = "Selectivity",
#        caption = "Green lines: Ascending limb | Purple line: Peak | Orange lines: Descending normal curve (50% & 5%)"
        caption = "Dots: 50% and 95% selectivity/maturity | Purple line: Peak | Orange lines: Descending normal curve (50% & 5%)"
      ) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      xlim(0,(input$Linf+round(input$Linf*0.1,0)))+
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "bottom"
      )+
      scale_color_manual(labels = c("Selectivity", "Maturity"), values = c("black", "pink"))+
      labs(color="")
    p
  })
  
  #Growth and Mortality plot
  output$growth_M_plot <- renderPlot({
    set.seed(19)
    
    pops<-populations()
    #Ages
    age_data_fished<-do.call(c,mapply(function(x) rep(pops$fished$age[x],round(pops$fished$numbers[x]*pops$fished$selectivity[x],0)),x=1:nrow(pops$fished),SIMPLIFY=TRUE))
    age_data_fish_df<-data.frame(age=age_data_fished,population="Fished")
    age_data_unfished<-do.call(c,mapply(function(x) rep(pops$unfished$age[x],round(pops$unfished$numbers[x]*pops$fished$selectivity[x],0)),x=1:nrow(pops$unfished),SIMPLIFY=TRUE))
    age_data_unfish_df<-data.frame(age=age_data_unfished,population="Unfished")
    
    #Lengths
    lt_data_fished<-do.call(c,mapply(function(x) rnorm(round(pops$fished$numbers[x],0),pops$fished$length[x],pops$fished$length[x]*0.1),x=1:nrow(pops$fished),SIMPLIFY=TRUE))
    lt_data_fish_df<-data.frame(length=lt_data_fished,population="Fished")
    lt_data_unfished<-do.call(c,mapply(function(x) rnorm(round(pops$unfished$numbers[x],0),pops$unfished$length[x],pops$unfished$length[x]*0.1),x=1:nrow(pops$unfished),SIMPLIFY=TRUE))
    lt_data_unfish_df<-data.frame(length=lt_data_unfished,population="Unfished")
    
    #Calculate age at maturity lengths
    age.mat<-input$t0 - (log(1 - (c(input$L50,input$L95) / input$Linf)) / input$K)
    age.lt.mat<-data.frame(Age=age.mat,Length=c(input$L50,input$L95))
    
    data <- populations()$unfished
    #browser()
    # Create the plot with dual y-axes
    # First, we need to scale the population data to fit with length data
    length_range <- range(data$length)
    pop_range <- c(0,1)
    
    # Scale population to length scale for plotting
    pop_scaled <- (data$numbers/1000 - pop_range[1]) / (pop_range[2] - pop_range[1]) * 
      (length_range[2] - length_range[1]) + length_range[1]
    #pop_scaled <-data$numbers/1000
    color.line.in<-viridis(3)
    # Create the plot
    p<-ggplot(data, aes(x = age)) +
      geom_line(aes(y = length, color = "Length"), size = 1.2) +
      geom_point(aes(y = length, color = "Length"), size = 2) +
      geom_line(aes(y = pop_scaled, color = "Population"), size = 1.2, linetype = "dashed") +
      geom_point(aes(y = pop_scaled, color = "Population"), size = 2, shape = 17) +
      geom_point(data=age.lt.mat, aes(x = Age,y=Length), size = 5, shape = 21,col="black",fill="lightblue") +
      annotate("text",age.lt.mat$Age+(5.4/input$M)*0.08,age.lt.mat$Length,label=c("Lmat50%","Lmat95%"))+
      
      # Add second y-axis
      scale_y_continuous(
        name = "Length",
        sec.axis = sec_axis(
          trans = ~ (. - length_range[1]) / (length_range[2] - length_range[1]) * 
            (pop_range[2] - pop_range[1]) + pop_range[1],
#           trans = ~ (. *length_range[2]*),
          name = "Proportion at Age"
        )
      ) +
      
      scale_color_manual(
        name = "Measure",
        values = c("Length" = color.line.in[1], "Population" = color.line.in[2]),
        labels = c("Length" = "Length (cm)", "Population" = "Proportion at Age")
      ) +
      
      labs(
        title = "Age-Length Relationship and Population Decline by Natural Mortality",
        subtitle = paste0("von Bertalanffy: L∞=", input$Linf, ", k=", input$K, ", t₀=", input$t0, 
                          " | Natural Mortality: M=", input$M," | Maximum age: M=",5.4/input$M),
        x = "Age (years)",
        caption = "Solid line: Length-at-age | Dashed line: Population decline due to natural mortality"
      ) +
      
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
 #       plot.caption = element_text(size = 9, style = "bold")
      ) +
      
      # Color the y-axis labels to match the lines
      theme(
        axis.title.y.left = element_text(color = color.line.in[1]),
        axis.text.y.left = element_text(color = color.line.in[1]),
        axis.title.y.right = element_text(color = color.line.in[2]),
        axis.text.y.right = element_text(color = color.line.in[2])
      )
    p
  })
  
  
  # Stock status table
  output$stock_status <- renderTable({
    status <- stock_status()
    mean_age_lt<- mean_bio_comp()
    
    
    
    data.frame(
      Metric = c("Spawning Biomass Ratio (SSB/SSB0)", 
                 "Total Biomass Ratio (B/B0)",
                 "Mean Length Ratio (fished/unfished)",
                 "Mean Age Ratio (fished/unfished)"
                 #"Spawning Biomass (Fished)",
                 #"Spawning Biomass (Unfished)",
                 #"Total Biomass (Fished)",
                 #"Total Biomass (Unfished)"
                 ),
      Value = c(round(status$SSB_ratio, 3),
                round(status$B_ratio, 3),
                mean_age_lt[2],
                mean_age_lt[1]
                #round(status$SSB_fished, 0),
                #round(status$SSB_unfished, 0),
                #round(status$B_fished, 0),
                #round(status$B_unfished, 0)
                )
    )
  }, striped = TRUE, hover = TRUE)
  
  # Stock status interpretation
  output$stock_interpretation <- renderText({
    status <- stock_status()
    ssb_ratio <- status$SSB_ratio
    
    if (ssb_ratio >= input$TRP) {
      interpretation <- "Stock Status: HEALTHY - Spawning biomass is above target spawning biomass level."
    } else if (ssb_ratio >= input$LRP) {
      interpretation <- "Stock Status: CAUTION - Spawning biomass is in the precautionary zone."
    } else {
      interpretation <- "Stock Status: OVERFISHED - Spawning biomass is below the limit spawning biomass level."
    }
    
    paste(interpretation, 
          sprintf("The fished population has %.1f%% of the unfished spawning biomass.", 
                  ssb_ratio * 100))
  })
  
  results_out<-reactiveVal(data.frame(SSB_ratio="",TB_ratio="",Lt_ratio="",Age_ratio="",M="",Linf="",K="",t0="",L50="",L95="",Sel50="",Sel95=""))
  
  #Capture index measures for chosen sampling
  observeEvent(input$save_results, {
    status <- stock_status()
    mean_age_lt<- mean_bio_comp()
    
    results_cap<-rbind(results_out(),c(round(status$SSB_ratio, 2),round(status$B_ratio, 2),round(mean_age_lt[2],2),round(mean_age_lt[1],2),input$M,input$Linf,input$K,input$t0,input$L50,input$L95,input$L50_asc,input$L95_asc,input$peak_length,input$desc_sd))
    #rownames(pop_samples_cap)<-c("Sampled Population","True Population")
    results_out(results_cap)
  })

  
  output$results_out <- renderTable({
    results_out()
  })
  
  #Clear the saved samples  
  observeEvent(input$clear_samples, {
    pop_samples(data.frame(Sampled="",Population=""))
  })
  
  
  
  # Update L95 to be greater than L50
#  observeEvent(input$L50, {
#    if (input$L95 <= input$L50) {
#      updateNumericInput(session, "L95", value = input$L50 + 20)
#    }
#  })
}

shinyApp(ui = ui, server = server)
