library(shiny)
library(bslib)
library(ggplot2)
library(shinyWidgets)
library(FSAsim)
library(r4ss)
library(plotly)
library(twosamples)


source('Functions.r', local = FALSE)

server <- function(input, output, session) {
  # Reactive expression to calculate population when button is clicked
  #Numages_simpop <- eventReactive(input$calculate, {
  Numages_simpop <- reactive({
    ages <- 0:(5.4 / input$M.pval)

    simpop <- calculate_population(
      ages = ages,
      Linf = input$Linf.pval,
      K = input$K.pval,
      t0 = input$t0.pval,
      M = input$M.pval,
      R0 = 10000000,
      F_mort = input$F.pval,
      L50_asc = input$L50_asc.pval,
      L95_asc = input$L95_asc.pval,
      peak_length = input$peak_length.pval,
      desc_sd = input$desc_sd.pval
    )

    samp.select <- calc_selectivity(
      length = simpop$Length,
      L50_asc = input$L50_asc.pval,
      L95_asc = input$L95_asc.pval,
      peak_length = input$peak_length.pval,
      desc_sd = input$desc_sd.pval
    )

    Numages_simpop <- data.frame(
      Age = simpop$Age,
      Length = simpop$Length,
      Numbers = simpop$Numbers * samp.select,
      Selectivity = samp.select
    )
    #browser()
    minage.in <- Numages_simpop$Age[min(which(
      Numages_simpop$Selectivity > 0.99
    ))]

    updateNumericInput(
      session,
      "CC.sel_agemin",
      value = minage.in
    )

    #max.age.in<-max(which((Numages_simpop$Numbers / 10000000) < 0.005))
    max.age.in <- which(
      (Numages_simpop$Numbers / 10000000) < 0.005 &
        Numages_simpop$Age > minage.in
    )

    if (any(max.age.in)) {
      max.age.in = min(max.age.in)
    } else {
      max.age.in <- input$M.pval
    }

    updateNumericInput(
      session,
      "CC.sel_agemax",
      value = max.age.in
    )

    return(Numages_simpop)
  })

  # Selectivity plot
  output$selectivity_plot <- renderPlotly({
    data <- Numages_simpop()
    p <- ggplot(data, aes(Age, Selectivity)) +
      geom_line(color = "#2C3E50", linewidth = 1.2) +
      theme_bw() +
      labs(title = "Selectivity by Age", x = "Age", y = "Selectivity") +
      theme(plot.title = element_text(hjust = 0.5))
    print(ggplotly(p))
  })

  # Age distribution plot
  output$age_plot <- renderPlotly({
    data <- Numages_simpop()
    p <- ggplot(data, aes(Age, Numbers / max(Numbers))) +
      geom_line(color = "#E74C3C", linewidth = 1.2) +
      theme_bw() +
      labs(title = "Age Distribution", x = "Age", y = "Proportion") +
      theme(plot.title = element_text(hjust = 0.5))
    print(ggplotly(p))
  })

  # Length at age plot
  output$length_plot <- renderPlotly({
    data <- Numages_simpop()
    p <- ggplot(data, aes(Age, Length)) +
      geom_line(color = "#3498DB", linewidth = 1.2) +
      theme_bw() +
      labs(title = "Von Bertalanffy Growth Curve", x = "Age", y = "Length") +
      theme(plot.title = element_text(hjust = 0.5))
    print(ggplotly(p))
  })

  Pvals_profile <- eventReactive(input$calculate.pval, {
    #test.name <- c("KS", "AD", "CVM", "WASS", "DTS")

    withProgress(message = 'Calculating distribution tests', value = 0, {
      Pvals_profile <- Pval.calc.plot(
        Numages_in = Numages_simpop(),
        numvec = c(25, 50, seq(100, 1000, 100), seq(1500, input$maxsamp, 500)),
        #test.opt.in = which(test.name == input$dist_test),
        test.opt.in = 5,
        reps = input$reps.pval,
        Plim = input$Plim.pval,
        age.min = input$CC.sel_agemin,
        age.max = input$CC.sel_agemax,
        Title.in = "Sim. Pop. with F=0.1"
      )
    })
    return(Pvals_profile)
  })

  output$pvals_plot <- renderPlotly({
    test_samp_num_rep <- Pvals_profile()$test_samp_num_rep

    p <- ggplot(test_samp_num_rep, aes(Sample, pvalue)) +
      stat_summary(fun.data = "mean_cl_boot", col = "blue") +
      stat_summary(fun = "mean", geom = "point", size = 0.75) +
      geom_hline(yintercept = input$Plim.pval, col = "red") +
      xlab("Sample size") +
      ylab(paste0(input$dist_test, " P-value")) +
      ylim(0, 1) +
      ggtitle("Distribution test p-values based on sample size") +
      theme_bw()
    print(ggplotly(p))
  })

  output$Z_comp_plot <- renderPlotly({
    Z_samp_num_rep <- Pvals_profile()$Z_samp_num_rep

    p <- ggplot(Z_samp_num_rep, aes(Sample, est_Z)) +
      stat_summary(fun.data = "mean_cl_boot", col = "blue") +
      stat_summary(fun = "mean", geom = "point", size = 0.75) +
      geom_hline(yintercept = -(input$M.pval + input$F.pval), col = "red") +
      xlab("Sample size") +
      ylab(paste0("Total mortality")) +
      expand_limits(y = 0) +
      ggtitle(
        "Total mortality estimates based on sample size. True value is horizontal line."
      ) +
      theme_bw()
    print(ggplotly(p))
  })

  output$Z_comp_plot_II <- renderPlotly({
    Z_samp_num_rep <- Pvals_profile()$Z_samp_num_rep
    true_Z <- mean(Z_samp_num_rep$true_Z)

    p <- ggplot(Z_samp_num_rep, aes(Sample, est_Z)) +
      stat_summary(fun.data = "mean_cl_boot", col = "blue") +
      stat_summary(fun = "mean", geom = "point", size = 0.75) +
      geom_hline(yintercept = true_Z, col = "red") +
      xlab("Sample size") +
      ylab(paste0("Total mortality")) +
      expand_limits(y = 0) +
      ggtitle(
        "Total mortality estimates based on sample size. True value is horizontal line."
      ) +
      theme_bw()
    print(ggplotly(p))
  })

  Samp_ages_comp <- eventReactive(input$calculate.sampsize, {
    Pvals_profile <- Pvals_profile()
    Samp_ages_comp <- Age_samp_check(Pvals_profile$Numages_in, input$sampsize)
    return(Samp_ages_comp)
  })

  output$Ageprop_plot <- renderPlot({
    mod <- subset(Samp_ages_comp()$Proportions, Source == "Modelled")
    samp <- subset(Samp_ages_comp()$Proportions, Source == "Sampled")

    mod.nums.plot <- mod$Prop[(input$CC.sel_agemin + 1):(input$CC.sel_agemax)]
    samp.nums.plot <- samp$Prop[(input$CC.sel_agemin + 1):(input$CC.sel_agemax)]
    age.range <- mod$Age[(input$CC.sel_agemin + 1):(input$CC.sel_agemax)]

    if (any(mod.nums.plot == 0)) {
      mod.nums.plot[mod.nums.plot == 0] <- NA
    }
    if (any(samp.nums.plot == 0)) {
      samp.nums.plot[samp.nums.plot == 0] <- NA
    }

    mod.Z <- lm(
      log(mod.nums.plot) ~ age.range
    )$coeff[2]

    samp.Z <- lm(
      log(samp.nums.plot) ~ age.range
    )$coeff[2]

    ggplot(Samp_ages_comp()$Proportions, aes(Age, Prop, col = Source)) +
      geom_line() +
      theme_bw() +
      ggtitle(paste0(
        "Sample size = ",
        input$sampsize,
        "; True Z = ",
        round(mod.Z, 3),
        " & Est. Z = ",
        round(samp.Z, 3)
      ))
  })

  output$CDF_plot <- renderPlot({
    ggplot(Samp_ages_comp()$Proportions, aes(Age, CDF, col = Source)) +
      geom_line() +
      theme_bw() +
      ggtitle(paste0("Sample size = ", input$sampsize))
  })
}
