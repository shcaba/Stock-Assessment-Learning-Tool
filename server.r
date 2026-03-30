library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(DT)
library(r4ss)
library(viridis)
library(reshape2)
library(shinyFiles)
library(shinybusy)
library(wesanderson)
library(bslib)
library(shinyWidgets)
library(FSAsim)
library(twosamples)

#################
### Functions ###
#################
von_bertalanffy <- function(age, Linf, K, t0 = 0) {
  Linf * (1 - exp(-K * (age - t0)))
}

calc_selectivity <- function(lengths, L50_asc, L95_asc, peak_length, desc_sd) {
  # Ascending limb parameters (logistic)
  #slope_asc <- 1/(1+exp(-log(19)*((lengths-L50_asc)/(L95_asc - L50_asc))))
  #  log(19) / (L95_asc - L50_asc) # 19 = ln(0.95/0.05)

  # Calculate selectivity for each length
  selectivity <- numeric(length(lengths))

  for (i in 1:length(lengths)) {
    L <- lengths[i]

    # Ascending limb (logistic)
    sel_asc <- 1 / (1 + exp(-log(19) * ((L - L50_asc) / (L95_asc - L50_asc))))
    #  1 / (1 + exp(-slope_asc * (L - L50_asc)))

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

calculate_population <- function(
  ages,
  Linf,
  K,
  t0,
  M,
  R0 = 1000,
  F_mort = 0,
  L50_asc,
  L95_asc,
  peak_length,
  desc_sd
) {
  # Calculate lengths at age using von Bertalanffy growth
  lengths <- von_bertalanffy(ages, Linf, K, t0)

  # Calculate selectivity
  if (F_mort > 0) {
    selectivity <- calc_selectivity(
      lengths = lengths,
      L50_asc = L50_asc,
      L95_asc = L95_asc,
      peak_length = peak_length,
      desc_sd = desc_sd
    )
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

calculate_stock_status <- function(fished_pop, unfished_pop, L50, L95) {
  # Spawning biomass (assuming weight proportional to length^3 and maturity at 50% Linf)
  fished_pop$weight <- 0.000001 * fished_pop$length^3
  unfished_pop$weight <- 0.000001 * unfished_pop$length^3

  # Assume 50% maturity at 65% of Linf
  #  maturity_length50 <- max(fished_pop$length) * 0.65
  #  maturity_length95 <- max(fished_pop$length) * 0.80
  #  maturity_prob <- 1 / (1 + exp(-log(19) * (lengths - input$L50) / (input$L95 - input$L50)))
  #  fished_pop$mature <- ifelse(fished_pop$length >= maturity_length, 1, 0)
  #  unfished_pop$mature <- ifelse(unfished_pop$length >= maturity_length, 1, 0)
  fished_pop$mature <- unfished_pop$mature <- 1 /
    (1 + exp(-log(19) * ((unfished_pop$length - L50) / (L95 - L50))))

  #1 /
  #(1 + exp(-log(19) * (unfished_pop$length - L50) / (L95 - L50)))

  # Calculate spawning biomass
  SSB_fished <- sum(
    fished_pop$numbers * fished_pop$weight * fished_pop$mature,
    na.rm = TRUE
  )
  SSB_unfished <- sum(
    unfished_pop$numbers * unfished_pop$weight * unfished_pop$mature,
    na.rm = TRUE
  )

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

# Calculate Beverton-Holt recruitment (relative form)
beverton_holt_relative <- function(S_rel, h) {
  # Beverton-Holt stock recruitment relationship in relative form
  # S_rel = S/S0, returns R/R0
  numerator <- 4 * h * S_rel
  denominator <- (1 - h) + S_rel * (5 * h - 1)
  R_rel <- numerator / denominator
  return(R_rel)
}

# Calculate selectivity-at-age
calculate_selectivity <- function(age, a50, a95) {
  #slope <- log(19) / (a95 - a50) # Slope for logistic selectivity
  #selectivity <- 1 / (1 + exp(-slope * (age - a50)))
  selectivity <- 1 /
    (1 + exp(-log(19) * ((age - a50) / (a95 - a50))))
  return(selectivity)
}

# Calculate spawning biomass per recruit
calculate_spawning_biomass <- function(
  F_rate,
  M,
  ages,
  selectivity,
  maturity,
  weight_at_age
) {
  # Survival to each age
  Z <- F_rate * selectivity + M # Total mortality
  survival <- exp(-cumsum(Z))
  survival <- c(1, survival[-length(survival)]) # Add age-0 survival

  # Spawning biomass per recruit
  spawning_biomass <- sum(survival * maturity * weight_at_age)
  return(spawning_biomass)
}

# Calculate yield per recruit
calculate_yield <- function(F_rate, M, ages, selectivity, weight_at_age) {
  Z <- F_rate * selectivity + M

  # Calculate numbers at age using Baranov catch equation
  survival <- exp(-cumsum(Z))
  survival <- c(1, survival[-length(survival)])

  # Yield calculation
  catch_at_age <- (F_rate * selectivity / Z) * (1 - exp(-Z)) * survival
  catch_at_age[Z == 0] <- 0 # Handle division by zero

  yield <- sum(catch_at_age * weight_at_age)
  return(yield)
}

#Run distributional tests
#test.opt: 1= ks; 2=ad; 3=cvm; 4= wass; 5= dts (the default)
Age_samp_check <- function(True_pop_mat, sampN, test.opt = 5) {
  probs_in <- True_pop_mat$Number / sum(True_pop_mat$Number)
  samp.size <- sample(True_pop_mat$Age, sampN, replace = TRUE, prob = probs_in)
  age_vec <- data.frame(
    age = min(True_pop_mat$Age):max(True_pop_mat$Age),
    number = 0
  )
  tab.samp <- table(samp.size)
  age_vec$number[age_vec$age %in% as.numeric(names(tab.samp))] <- tab.samp
  age_vec$props <- age_vec$number / sum(age_vec$number)
  True_pop_mat$Props <- True_pop_mat$Number / sum(True_pop_mat$Number)

  #Run test
  if (test.opt == 1) {
    dts.out <- ks_test(True_pop_mat$Props, age_vec$props)
  }
  if (test.opt == 2) {
    dts.out <- ad_test(True_pop_mat$Props, age_vec$props)
  }
  if (test.opt == 3) {
    dts.out <- cvm_test(True_pop_mat$Props, age_vec$props)
  }
  if (test.opt == 4) {
    dts.out <- wass_test(True_pop_mat$Props, age_vec$props)
  }
  if (test.opt == 5) {
    dts.out <- dts_test(True_pop_mat$Props, age_vec$props)
  }
  #Create proportions object
  True_ages_props <- data.frame(
    Age = True_pop_mat$Age,
    Prop = True_pop_mat$Props,
    CDF = cumsum(True_pop_mat$Props),
    Source = "Modelled"
  )
  Samp_ages_props <- data.frame(
    Age = age_vec$age,
    Prop = age_vec$props,
    CDF = cumsum(age_vec$props),
    Source = "Sampled"
  )
  True_samp_props <- rbind(True_ages_props, Samp_ages_props)

  #Make object list
  age_samps_ktest <- list(
    True_pop_mat,
    age_vec,
    True_samp_props,
    dts.out[2]
  )
  names(age_samps_ktest) <- c(
    "True Ages",
    "Sampled Ages",
    "Proportions",
    "P-value"
  )
  return(age_samps_ktest)
}

Pval.calc.plot <- function(
  Numages_in,
  numvec,
  test.opt.in = 5,
  reps = 100,
  Plim = 0.85,
  age.min = 10,
  age.max = 27,
  Title.in = ""
) {
  test_samp_num <- data.frame(Sample = numvec, pvalue = NA)
  Z_samp_num <- data.frame(Sample = numvec, true_Z = NA, est_Z = NA)
  for (ii in 1:reps) {
    for (i in 1:length(numvec)) {
      Samp_ages_tests <- Age_samp_check(
        Numages_in,
        numvec[i],
        test.opt = test.opt.in
      )
      test_samp_num$pvalue[i] <- Samp_ages_tests$`P-value`

      #Calculate catch curve estimate of Z
      True.nums.minmax <- Numages_in$Number[(age.min + 1):(age.max)]
      Samp.nums.minmax <- Samp_ages_tests$`Sampled Ages`$number[
        (age.min + 1):(age.max)
      ]
      age.minmax.lm <- Numages_in$Age[(age.min + 1):(age.max)]

      if (any(True.nums.minmax == 0)) {
        True.nums.minmax[True.nums.minmax == 0] <- NA
      }

      if (any(Samp.nums.minmax == 0)) {
        Samp.nums.minmax[Samp.nums.minmax == 0] <- NA
      }

      ln_true_nums <- log(True.nums.minmax)
      ln_est_nums <- log(Samp.nums.minmax)

      true_linear_points <- length(True.nums.minmax) -
        sum(is.na(True.nums.minmax))
      est_linear_points <- length(Samp.nums.minmax) -
        sum(is.na(Samp.nums.minmax))

      if (true_linear_points > 1) {
        Z_samp_num$true_Z[i] <- lm(ln_true_nums ~ age.minmax.lm)$coeff[2]
      } else {
        Z_samp_num$true_Z[i] <- NA
      }

      if (est_linear_points > 1) {
        Z_samp_num$est_Z[i] <- lm(ln_est_nums ~ age.minmax.lm)$coeff[2]
      } else {
        Z_samp_num$est_Z[i] <- NA
      }
    }

    if (ii == 1) {
      test_samp_num_rep <- test_samp_num
      Z_samp_num_rep <- Z_samp_num
    }
    if (ii > 1) {
      test_samp_num_rep <- rbind(test_samp_num_rep, test_samp_num)
      Z_samp_num_rep <- rbind(Z_samp_num_rep, Z_samp_num)
    }
    incProgress(1 / reps, detail = paste("Doing rep ", ii))
  }

  return(list(
    Numages_in = Numages_in,
    test_samp_num_rep = test_samp_num_rep,
    test_samp_num = test_samp_num,
    Z_samp_num = Z_samp_num,
    Z_samp_num_rep = Z_samp_num_rep
  ))
}


##################################################################

server <- function(input, output, session) {
  # observeEvent(input$goto_manage_loop, {
  #   nav_select("navbar", "reports")
  # })
  #
  # observeEvent(input$goto_SAdiag, {
  #   nav_select("navbar", "reports")
  # })

  ####################
  # Life history tab #
  ####################
  nav_hide("navbar", "LHP")

  observeEvent(input$goto_LHP, {
    nav_show("navbar", "LHP")
    nav_select("navbar", "LHP")
    # Reactive data generation
    plot_data <- reactive({
      maxage <- 5.4 / input$M + 0.2 * (5.4 / input$M)
      ages <- seq(0, maxage, by = 0.1)
      lengths <- seq(1, input$Linf + 0.2 * input$Linf, by = 1)

      # Growth curve (von Bertalanffy)
      length_at_age <- input$Linf * (1 - exp(-input$K * (ages - input$t0)))

      # Natural mortality
      mortality <- rep(input$M, length(ages)) # Constant mortality
      survival <- exp(-input$M * ages) # Survival probability

      # Maturity ogive (logistic function)
      maturity_prob <- 1 /
        (1 + exp(-log(19) * (lengths - input$L50) / (input$L95 - input$L50)))

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
      maxage <- 5.4 / input$M + 0.2 * (5.4 / input$M)
      p <- ggplot(
        data.frame(Age = data$ages, Survival = data$survival),
        aes(x = Age, y = Survival)
      ) +
        geom_line(color = "red", size = 1.2) +
        labs(
          title = paste("Natural mortality (M =", input$M, ")"),
          x = "Age (years)",
          y = "Survival Probability"
        ) +
        xlim(c(0, maxage)) +
        theme_minimal() +
        annotate(
          "text",
          x = maxage * 0.8,
          y = 0.9,
          label = paste0("Max age = ", 5.4 / input$M),
          col = "red",
          size = unit(5, "pt")
        )

      ggplotly(p)
    })

    # Growth Plot
    output$growth_plot <- renderPlotly({
      data <- plot_data()
      maxage <- 5.4 / input$M + 0.2 * (5.4 / input$M)
      age_at_size <- input$t0 -
        ((log(1 - (c(input$L50, input$L95) / input$Linf)) / input$K))
      size_at_maxage <- input$Linf *
        (1 - exp(-input$K * (5.4 / input$M - input$t0)))

      p <- ggplot(
        data.frame(Age = data$ages, Length = data$length_at_age),
        aes(x = Age, y = Length)
      ) +
        geom_line(color = "blue", size = 1.2) +
        geom_hline(
          yintercept = input$Linf,
          linetype = "dashed",
          color = "gray"
        ) +
        xlim(c(0, maxage)) +
        geom_point(
          aes(x = age_at_size[1], y = input$L50),
          col = "purple",
          size = 4
        ) +
        geom_point(
          aes(x = age_at_size[2], y = input$L95),
          col = "purple",
          size = 4
        ) +
        geom_point(
          aes(x = 5.4 / input$M, y = size_at_maxage),
          col = "red",
          size = 4
        ) +
        #geom_point(aes(x=c(age_at_size,5.4/input$M),y=c(input$L50,input$L95,size_at_maxage)))+
        labs(
          title = "von Bertalanffy Growth Curve",
          x = "Age (years)",
          y = "Length (cm)"
        ) +
        annotate(
          "text",
          x = maxage * 0.7,
          y = input$Linf + 5,
          label = paste("L∞ =", input$Linf),
          color = "gray"
        ) +
        annotate(
          "text",
          x = c(age_at_size + age_at_size * 0.1, 5.4 / input$M),
          y = c(
            y = input$L50 - input$L50 * 0.1,
            y = input$L95 - input$L95 * 0.1,
            size_at_maxage - 0.1 * size_at_maxage
          ),
          label = c("L50", "L95", "Max age"),
          color = c("purple", "purple", "red"),
          hjust = 0
        ) +
        theme_minimal()

      ggplotly(p)
    })

    # Maturity Plot
    output$maturity_plot <- renderPlotly({
      data <- plot_data()

      p <- ggplot(
        data.frame(Length = data$lengths, Maturity = data$maturity_prob),
        aes(x = Length, y = Maturity)
      ) +
        geom_line(color = "green", size = 1.2) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
        geom_vline(
          xintercept = input$L50,
          linetype = "dashed",
          color = "gray"
        ) +
        labs(
          title = "Proportion Mature at Length",
          x = "Length (cm)",
          y = "Proportion Mature"
        ) +
        ylim(0, 1) +
        theme_minimal()

      ggplotly(p)
    })

    # Weight-Length Plot
    output$weight_length_plot <- renderPlotly({
      data <- plot_data()

      p <- ggplot(
        data.frame(Length = data$lengths, Weight = data$weight),
        aes(x = Length, y = Weight)
      ) +
        geom_line(color = "purple", size = 1.2) +
        labs(
          title = paste("Weight-Length: W =", input$a, "× L^", input$b),
          x = "Length (cm)",
          y = "Weight (kg)"
        ) +
        theme_minimal()

      ggplotly(p)
    })
  })

  ###############
  # Selectivity #
  ###############
  nav_hide("navbar", "selectivity")

  observeEvent(input$goto_selectivity, {
    nav_show("navbar", "selectivity")
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
        bin_labels <- paste0(
          round(bins[-length(bins)], 1),
          "-",
          round(bins[-1], 1)
        )
      } else {
        #bins <- seq(input$min_age, input$max_age, length.out = input$n_bins + 1)
        bins <- seq(input$min_age, input$max_age, by = input$n_bins)
        bin_mids <- (bins[-1] + bins[-length(bins)]) / 2
        bin_labels <- paste0(
          round(bins[-length(bins)], 1),
          "-",
          round(bins[-1], 1)
        )
      }

      # Initialize with default values if data doesn't exist or dimensions changed
      #if (is.null(values$selectivity_data) || nrow(values$selectivity_data) != input$n_bins) {
      if (
        is.null(values$selectivity_data) ||
          nrow(values$selectivity_data) != (length(bins) - 1)
      ) {
        values$selectivity_data <- data.frame(
          bin = 1:(length(bins) - 1),
          bin_mid = bin_mids,
          bin_label = bin_labels,
          #selectivity = rep(0.5, input$n_bins),
          selectivity = rep(0.5, (length(bins) - 1)),
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
      x_norm <- (x - min(x)) / (max(x) - min(x)) # Normalize to 0-1
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
      dome_sel <- dnorm(1:n, mean = n / 2, sd = n / 4)
      dome_sel <- dome_sel / max(dome_sel) # Normalize to 0-1

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
      flat_sel <- c(rep(0, ceiling(n / 3)), rep(1.0, n - ceiling(n / 3)))
      flat_sel <- flat_sel[1:n] # Ensure correct length

      values$selectivity_data$selectivity <- flat_sel

      # Update all sliders
      for (i in 1:nrow(values$selectivity_data)) {
        updateSliderInput(session, paste0("sel_", i), value = flat_sel[i])
      }
    })

    observeEvent(input$reset_all, {
      req(values$selectivity_data)

      values$selectivity_data$selectivity <- rep(
        0.5,
        nrow(values$selectivity_data)
      )
      values$smooth_curve <- NULL
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

      tryCatch(
        {
          smooth_model <- loess(
            selectivity ~ bin_mid,
            data = values$selectivity_data,
            span = 0.5
          )
          y_smooth <- predict(
            smooth_model,
            newdata = data.frame(bin_mid = x_seq)
          )
          y_smooth <- pmax(0, pmin(1, y_smooth)) # Constrain between 0 and 1

          values$smooth_curve <- data.frame(x = x_seq, y = y_smooth)

          # Update bin selectivities based on smooth curve
          for (i in 1:nrow(values$selectivity_data)) {
            bin_center <- values$selectivity_data$bin_mid[i]
            closest_idx <- which.min(abs(x_seq - bin_center))
            values$selectivity_data$selectivity[i] <- y_smooth[closest_idx]
          }
        },
        error = function(e) {
          showNotification(
            "Need at least 3 points to apply smoothing",
            type = "warning"
          )
        }
      )
    })

    # Create the selectivity plot
    output$selectivity_plot_out <- renderPlot({
      req(values$selectivity_data)

      x_label <- ifelse(
        input$bin_type == "length",
        "Length (cm)",
        "Age (years)"
      )

      minx <- ifelse(
        input$bin_type == "length",
        input$min_length,
        input$min_age
      )
      maxx <- ifelse(
        input$bin_type == "length",
        input$max_length,
        input$max_age
      )

      p <- ggplot(values$selectivity_data, aes(x = bin_mid, y = selectivity)) +
        geom_line(color = "blue", size = 1.2) +
        geom_point(color = "blue", size = 3, alpha = 0.8) +
        geom_point(color = "white", size = 1.5) +
        geom_hline(yintercept = c(0, 1), col = "black", lwd = c(1, 1.5)) +
        annotate(
          "text",
          x = maxx * 0.85,
          y = 1.025,
          label = "maximum possible selectivity",
          size = 18 / .pt
        ) +
        xlim(minx, maxx) +
        scale_y_continuous(limits = c(0, 1.025), breaks = seq(0, 1, 0.2)) +
        labs(
          x = x_label,
          y = "Selectivity",
          title = paste(
            "Selectivity Curve by",
            tools::toTitleCase(input$bin_type)
          )
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.grid.minor = element_line(),
          panel.grid.major = element_line(),
          legend.text = element_text(size = 16)
        ) +
        geom_ribbon(
          ymin = 0,
          aes(ymax = selectivity, fill = 'Selected'),
          alpha = 0.2
        ) +
        geom_ribbon(
          aes(ymin = selectivity, fill = 'Not selected'),
          ymax = 1,
          alpha = 0.2
        ) +
        scale_fill_manual(
          name = '',
          values = c("Selected" = "green", "Not selected" = "black")
        )

      # Add smooth curve if available
      if (!is.null(values$smooth_curve)) {
        p <- p +
          geom_line(
            data = values$smooth_curve,
            aes(x = x, y = y),
            color = "purple",
            size = 1.2,
            alpha = 0.7
          )
      }
      p
    })

    # Download handler for selectivity data
    output$download_data <- downloadHandler(
      filename = function() {
        paste(
          "selectivity_curve_",
          input$bin_type,
          "_",
          Sys.Date(),
          ".csv",
          sep = ""
        )
      },
      content = function(file) {
        req(values$selectivity_data)
        write.csv(values$selectivity_data, file, row.names = FALSE)
      }
    )
  })

  ################
  # Productivity #
  ################
  nav_hide("navbar", "productivity")
  observeEvent(input$goto_prod_eqyield, {
    nav_show("navbar", "productivity")
    nav_select("navbar", "productivity")
    output$maturity.in <- renderUI({
      fluidRow(
        column(
          width = 6,
          numericInput(
            "A50",
            "50% Maturity:",
            min = 1,
            max = 1000,
            value = round((5.4 / input$natural_mortality) * 0.1, 0),
            step = 0.01
          )
        ),
        column(
          width = 6,
          numericInput(
            "A95",
            "95% Maturity:",
            min = 2,
            max = 15,
            value = round((5.4 / input$natural_mortality) * 0.2, 0),
            step = 0.01
          )
        )
      )
    })

    output$selectivity.in <- renderUI({
      fluidRow(
        column(
          width = 6,
          numericInput(
            "sel_50",
            "50% Sel:",
            min = 1,
            max = 10,
            value = round((5.4 / input$natural_mortality) * 0.1, 0),
            step = 0.01
          )
        ),
        column(
          width = 6,
          numericInput(
            "sel_95",
            "95% Sel:",
            min = 2,
            max = 15,
            value = round((5.4 / input$natural_mortality) * 0.2, 0),
            step = 0.01
          )
        )
      )
    })

    # Create reactive data
    sr_data <- reactive({
      # Create range of relative spawning biomass values (S/S0)
      S_rel_values <- seq(0, 1, length.out = 100)

      # Calculate relative recruitment for each relative spawning biomass
      R_rel_values <- beverton_holt_relative(S_rel_values, input$steepness)

      data.frame(
        Relative_Spawning_Biomass = S_rel_values,
        Relative_Recruitment = R_rel_values
      )
    })

    # Generate the plot
    output$sr_plot <- renderPlot({
      data <- sr_data()

      # Calculate key reference points (in relative terms)
      S_rel_20 <- 0.2
      R_rel_20 <- beverton_holt_relative(S_rel_20, input$steepness)

      ggplot(
        data,
        aes(x = Relative_Spawning_Biomass, y = Relative_Recruitment)
      ) +
        geom_line(color = "blue", size = 1.2) +

        # Add reference lines
        geom_vline(
          xintercept = 1.0,
          linetype = "dashed",
          color = "red",
          alpha = 0.7
        ) +
        geom_vline(
          xintercept = 0.2,
          linetype = "dotted",
          color = "orange",
          alpha = 0.7
        ) +
        geom_hline(
          yintercept = 1.0,
          linetype = "dashed",
          color = "red",
          alpha = 0.7
        ) +
        geom_hline(
          yintercept = R_rel_20,
          linetype = "dotted",
          color = "orange",
          alpha = 0.7
        ) +

        # Add reference points
        geom_point(aes(x = 1.0, y = 1.0), color = "red", size = 3) +
        geom_point(aes(x = 0.2, y = R_rel_20), color = "orange", size = 3) +

        # Add annotations
        annotate(
          "text",
          x = 1.0,
          y = 1.05,
          label = "(S₀/S₀, R₀/R₀)",
          color = "red",
          hjust = 0.5
        ) +
        annotate(
          "text",
          x = 0.2,
          y = R_rel_20 + 0.05,
          label = paste("(0.2, ", round(R_rel_20, 3), ")", sep = ""),
          color = "orange",
          hjust = 0.5
        ) +

        # Styling
        labs(
          x = "Relative Spawning Stock Biomass (S/S₀)",
          y = "Relative Recruitment (R/R₀)",
          title = paste(
            "Beverton-Holt Stock Recruitment Curve (h =",
            input$steepness,
            ")"
          )
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.grid.minor = element_blank()
        ) +
        xlim(0, 1.1) +
        ylim(0, max(data$Relative_Recruitment) * 1.1)
    })

    # Reactive population calculation
    yield_data <- reactive({
      ages <- 1:(5.4 / input$natural_mortality)

      # Selectivity-at-age (logistic)
      selectivity.yld <- calculate_selectivity(ages, input$sel_50, input$sel_95)

      # Maturity-at-age (assume knife-edge at age 2 for simplicity)
      #maturity <- ifelse(ages >= 2, 1, 0)
      maturity <- 1 /
        (1 + exp(-log(19) * (ages - input$A50) / (input$A95 - input$A50)))

      # Weight-at-age (assume linear growth for simplicity)
      weight_at_age <- ages * 0.1 # kg

      # Range of fishing mortality rates
      F_rates <- c(
        seq(0, 0.5, by = 0.0001),
        seq(0.501, 1, by = 0.001),
        seq(1.01, 5, by = 0.01),
        seq(5.1, 10, by = 0.1)
      )

      # Calculate metrics for each F rate
      results <- data.frame(
        F_rate = F_rates,
        spawning_biomass = numeric(length(F_rates)),
        yield = numeric(length(F_rates)),
        rel_spawning_biomass = numeric(length(F_rates))
      )

      # Virgin spawning biomass (F = 0)
      virgin_sb <- calculate_spawning_biomass(
        0,
        input$natural_mortality,
        ages,
        selectivity.yld,
        maturity,
        weight_at_age
      )

      for (i in seq_along(F_rates)) {
        sb <- calculate_spawning_biomass(
          F_rates[i],
          input$natural_mortality,
          ages,
          selectivity.yld,
          maturity,
          weight_at_age
        )
        yld <- calculate_yield(
          F_rates[i],
          input$natural_mortality,
          ages,
          selectivity.yld,
          weight_at_age
        )

        results$spawning_biomass[i] <- sb
        results$yield[i] <- yld
        results$rel_spawning_biomass[i] <- sb / virgin_sb
      }

      # Apply steepness effect (Beverton-Holt recruitment)
      # R = (4 * h * R0 * SSB) / (SSB0 * (1 - h) + SSB * (5 * h - 1))
      h <- input$steepness
      R0 <- 1000 # Unfished recruitment (arbitrary units)

      recruitment_multiplier <- (4 * h * results$rel_spawning_biomass) /
        ((1 - h) + results$rel_spawning_biomass * (5 * h - 1))

      # Adjust yield by recruitment
      results$adjusted_yield <- results$yield * recruitment_multiplier

      return(results)
    })

    # Yield curve plot
    output$yield_curve <- renderPlot({
      data <- yield_data()

      # Find MSY point
      #if(any(is.na(data$adjusted_yield)))
      #{
      msy_point <- data[which.max(data$adjusted_yield), ]
      pgy_points <- data[
        round(data$adjusted_yield, 4) ==
          round(input$PGY * msy_point$adjusted_yield, 4),
      ]
      #}

      ggplot(data, aes(x = rel_spawning_biomass, y = adjusted_yield)) +
        geom_line(color = "steelblue", size = 1.2) +
        geom_point(
          data = msy_point,
          aes(x = rel_spawning_biomass, y = adjusted_yield),
          color = "red",
          size = 3
        ) +
        geom_vline(
          xintercept = msy_point$rel_spawning_biomass,
          color = "red",
          linetype = "dashed",
          alpha = 0.7
        ) +
        geom_vline(
          xintercept = max(pgy_points$rel_spawning_biomass),
          color = "purple",
          linetype = "dashed",
          alpha = 0.7
        ) +
        geom_vline(
          xintercept = min(pgy_points$rel_spawning_biomass),
          color = "purple",
          linetype = "dashed",
          alpha = 0.7
        ) +
        geom_hline(
          yintercept = input$PGY * msy_point$adjusted_yield,
          color = "purple",
          linetype = "dashed",
          alpha = 0.7
        ) +
        geom_point(
          data = pgy_points,
          aes(x = rel_spawning_biomass, y = adjusted_yield),
          color = "purple",
          size = 3
        ) +
        labs(
          title = "Fishery Yield Curve",
          subtitle = paste(
            "Steepness =",
            input$steepness,
            "| MSY at SSB/SSB₀ =",
            round(msy_point$rel_spawning_biomass, 3)
          ),
          x = "Relative Spawning Stock Biomass (SSB/SSB₀)",
          y = "Yield (arbitrary units)",
          caption = "Red point shows Maximum Sustainable Yield (MSY)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        ) +
        scale_x_continuous(
          limits = c(0, 1),
          labels = scales::percent_format()
        ) +
        scale_y_continuous(labels = scales::comma_format())
    })

    # Parameters table
    output$parameters_table <- renderTable(
      {
        req(input$A50, input$A50, input$sel_50, input$sel_95)
        #data <- yield_data()
        #msy_point <- data[which.max(data$adjusted_yield), ]
        #pgy_points<- data[round(data$adjusted_yield,4)==round(input$PGY*msy_point$adjusted_yield,4),]

        params <- data.frame(
          Parameter = c(
            "Steepness (h)",
            "Natural Mortality (M)",
            "Maximum Age",
            "Age at 50% Maturity",
            "Age at  95% Maturity",
            "Age at 50% Selectivity",
            "Age at  95% Selectivity"
          ),
          Value = c(
            input$steepness,
            input$natural_mortality,
            5.4 / input$natural_mortality,
            input$A50,
            input$A95,
            input$sel_50,
            input$sel_95
          )
        )

        return(params)
      },
      striped = TRUE,
      hover = TRUE
    )

    # Outputs table
    output$outputs_table <- renderTable(
      {
        req(input$A50, input$A50, input$sel_50, input$sel_95)

        data <- yield_data()
        msy_point <- data[which.max(data$adjusted_yield), ]
        pgy_points <- data[
          round(data$adjusted_yield, 4) ==
            round(input$PGY * msy_point$adjusted_yield, 4),
        ]

        outputs <- data.frame(
          Output = c(
            "MSY SSB/SSB₀",
            "F at MSY",
            "Pretty Good Yield %",
            "High relative SB at Pretty Good Yield",
            "Low relative SB at Pretty Good Yield"
          ),
          Value = c(
            round(msy_point$rel_spawning_biomass, 3),
            round(msy_point$F_rate, 3),
            input$PGY,
            max(pgy_points$rel_spawning_biomass),
            min(pgy_points$rel_spawning_biomass)
          )
        )

        return(outputs)
      },
      striped = TRUE,
      hover = TRUE
    )
  })

  ###############
  # Uncertainty #
  ###############
  nav_hide("navbar", "uncertainty")
  observeEvent(input$goto_uncertainty, {
    nav_show("navbar", "uncertainty")
    nav_select("navbar", "uncertainty")
    true_value <- 100

    # Reactive values to store data
    values <- reactiveValues(
      data = NULL,
      seed = 123
    )

    # Generate data based on current parameters
    generate_data <- reactive({
      # Use seed for reproducibility until user clicks resample
      set.seed(values$seed)

      # Calculate standard deviation from precision
      sd_value <- input$CV * true_value

      # Generate measurements with bias and precision
      measurements <- rnorm(
        input$n_samples,
        mean = true_value + (true_value * (input$bias / 100)),
        sd = sd_value
      )

      data.frame(
        measurement = measurements,
        true_value = true_value,
        bias = input$bias,
        precision = input$CV
      )
    })

    # Update data when parameters change or resample is clicked
    observe({
      values$data <- generate_data()
    })

    # Resample button
    observeEvent(input$resample, {
      values$seed <- sample(1:10000, 1)
      values$data <- generate_data()
    })

    # # Predefined scenarios
    # observeEvent(input$scenario1, {
    #   updateSliderInput(session, "bias", value = 0)
    #   updateSliderInput(session, "precision", value = 3)
    #   values$seed <- sample(1:10000, 1)
    # })
    #
    # observeEvent(input$scenario2, {
    #   updateSliderInput(session, "bias", value = 0)
    #   updateSliderInput(session, "precision", value = 0.3)
    #   values$seed <- sample(1:10000, 1)
    # })
    #
    # observeEvent(input$scenario3, {
    #   updateSliderInput(session, "bias", value = 15)
    #   updateSliderInput(session, "precision", value = 3)
    #   values$seed <- sample(1:10000, 1)
    # })
    #
    # observeEvent(input$scenario4, {
    #   updateSliderInput(session, "bias", value = 15)
    #   updateSliderInput(session, "precision", value = 0.3)
    #   values$seed <- sample(1:10000, 1)
    # })

    # Main plot
    output$main_plot <- renderPlot({
      req(values$data)

      data <- values$data

      # Calculate mean and confidence interval
      mean_measurement <- mean(data$measurement)
      #se_measurement <- sd(data$measurement) / sqrt(nrow(data))
      ci_lower <- mean_measurement - 1.96 * sd(data$measurement)
      ci_upper <- mean_measurement + 1.96 * sd(data$measurement)

      # Create the plot
      p <- ggplot(data, aes(x = measurement)) +
        geom_histogram(
          aes(y = after_stat(density)),
          bins = 100,
          alpha = 0.7,
          fill = "steelblue",
          color = "white"
        ) +
        #      geom_density(alpha = 0.3, fill = "steelblue") +
        xlim(0, 200) +
        # Add vertical lines for true value, sample mean, and CI
        geom_vline(
          aes(xintercept = true_value),
          color = "black",
          size = 2,
          linetype = "solid",
          alpha = 0.8
        ) +
        geom_vline(
          aes(xintercept = mean_measurement),
          color = "orange",
          size = 2,
          linetype = "dashed",
          alpha = 0.8
        ) +
        geom_vline(
          aes(xintercept = ci_lower),
          color = "orange",
          size = 1,
          linetype = "dotted",
          alpha = 0.6
        ) +
        geom_vline(
          aes(xintercept = ci_upper),
          color = "orange",
          size = 1,
          linetype = "dotted",
          alpha = 0.6
        ) +

        # Add labels and annotations
        annotate(
          "text",
          x = true_value,
          y = Inf,
          label = "True Value",
          vjust = 2,
          hjust = -0.1,
          color = "black",
          fontface = "bold"
        ) +
        annotate(
          "text",
          x = mean_measurement,
          y = Inf,
          label = "Sample Mean",
          vjust = 2,
          hjust = 1.1,
          color = "orange",
          fontface = "bold"
        ) +

        labs(
          title = paste("Distribution of", input$n_samples, "measurements"),
          subtitle = paste(
            "Bias =",
            input$bias,
            "%| Precision =",
            round(input$CV, 2)
          ),
          x = "Sampled Metric (e.g., lengths, abundance, etc.)",
          y = "Density"
        ) +
        #theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position = "bottom"
        )

      print(p)
    })

    # Statistics table
    output$statistics_table <- renderTable(
      {
        req(values$data)

        data <- values$data

        # Calculate statistics
        sample_mean <- mean(data$measurement)
        sample_sd <- sd(data$measurement)
        bias_estimate <- sample_mean - true_value
        #    rmse <- sqrt(mean((data$measurement - true_value)^2))
        sd_value <- input$CV * true_value
        # Create summary table
        stats <- data.frame(
          Metric = c(
            "True Value",
            "Sample Mean",
            "% Estimated Bias",
            "True SD",
            "Sample SD"
          ),
          Value = c(
            round(true_value, 2),
            round(sample_mean, 2),
            round(bias_estimate, 2),
            round(sd_value, 2),
            round(sample_sd, 2)
            #round(rmse, 2),
          ),
          stringsAsFactors = FALSE
        )

        # if(abs(input$bias) >= 20 & input$CV < 0.2) {
        #   div(class = "alert alert-warning",
        #       "High precision but high bias: Measurements are consistent but systematically wrong!")
        # } else if(abs(input$bias) < 20 & input$CV >= 0.2) {
        #   div(class = "alert alert-info",
        #       "Low bias but low precision: Measurements are unbiased but highly variable.")
        # } else if(abs(input$bias) < 20 & input$CV < 0.2) {
        #   div(class = "alert alert-success",
        #       "Low bias and high precision: Ideal scenario with accurate and precise measurements!")
        # } else if(abs(input$bias) >= 20 & input$CV >= 0.2){
        #   div(class = "alert alert-danger",
        #       "High bias and low precision: Worst case with both systematic error and high variability.")
        # }

        stats
      },
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE
    )

    output$interp <- renderUI({
      if (abs(input$bias) >= 20 & input$CV < 0.2) {
        div(
          class = "alert alert-warning",
          "High precision but high bias: Measurements are consistent but systematically wrong!"
        )
      } else if (abs(input$bias) < 20 & input$CV >= 0.2) {
        div(
          class = "alert alert-info",
          "Low bias but low precision: Measurements are unbiased but highly variable."
        )
      } else if (abs(input$bias) < 20 & input$CV < 0.2) {
        div(
          class = "alert alert-success",
          "Low bias and high precision: Ideal scenario with accurate and precise measurements!"
        )
      } else if (abs(input$bias) >= 20 & input$CV >= 0.2) {
        div(
          class = "alert alert-danger",
          "High bias and low precision: Worst case with both systematic error and high variability."
        )
      }
    })

    # Uncertainty text
    output$uncertainty <- renderUI({
      # Generate interpretation
      uncertainty_text <- tagList(
        h5("Describing Uncertainty:", class = "text-primary"),
        tags$ul(
          tags$li(
            strong("Bias:"),
            "Systematic error that shifts all measurements away from the true value"
          ),
          tags$li(
            strong("Precision:"),
            "How tightly clustered measurements are (opposite of variance)"
          ),
        ),

        h5("Types of uncertainy:", class = "text-primary"),
        tags$ul(
          tags$li(
            strong("Measurement error:"),
            "Imperfect measures that lead to bias. This is possibly controllable or reducible with better measuring approaches."
          ),
          tags$li(
            strong("Process uncertainty:"),
            "Naturally occuring variability (e.g., length at age, recruitment). This is hard to control and a source of both imprecision. May also induce bias if ignored or mis-modelled."
          ),
        ),
      )
      uncertainty_text
    })

    # Sources text
    output$sources <- renderUI({
      # Sources of uncertainty
      sources_text <- tagList(
        h5("Where Does Model Uncertainty Come From?", class = "text-primary"),
        tags$ul(
          tags$li(
            strong("Data Representativeness:"),
            "When the data do not measure or represent what is intended. Major source of bias."
          ),
          tags$li(
            strong("Parameter estimation:"),
            "Unknown parameter values and estimating them via data and/or priors.  May produces both bias and imprecision"
          ),
          tags$li(
            strong("Model assumptions:"),
            "Models are approximations of reality and have assumptions based on how they are specified (i.e., which parameters are used and estimated or not). Those assumptions may cause bias if they are poor or poorly explored."
          ),
          tags$li(
            strong("Model type:"),
            "Different models will have different assumptions. Knowing these assumptions for each model type will identify areas of uncertainty (or overcertainty when pre-specifying parameters)."
          ),
          tags$li(
            strong("Natural Variability:"),
            "No matter how well the system is measured, it may not be stationary or static. Natural variability, even measured perfectly, causes uncertainty."
          ),
        ),
      )

      sources_text
    })

    # Sources of uncertainty
    output$estimation <- renderUI({
      estimation_text <- tagList(
        h5(
          "How Is Stock Assessment Uncertainty Estimated?",
          class = "text-primary"
        ),
        tags$ul(
          tags$li(
            strong("Within Model"),
            "When the data do not measure or represent what is intended. Major source of bias."
          ),
          tags$ol(
            tags$li(
              strong("Maximum Likelihood Estimation (MLE):"),
              "Produces asymptotic variances which are normally distributed. Much faster than Bayesian analyses, but may underestimate within model uncertainty compared to Bayesian analyses."
            ),
            tags$li(
              strong("Bayesian Estimation:"),
              "Uses the data, priors and MLE to explore and estimate uncertainty. Long estimation run times."
            ),
          ),
          tags$li(
            strong("Among Model Uncertainty:"),
            "Different models will have different assumptions. Knowing these assumptions is key.  "
          ),
          tags$ol(
            tags$li(
              strong("Sensitivity Analysis:"),
              "Changing model inputs or assumptions to explore how it changes model outputs. One of the most common and powerful ways to explore model uncertainty."
            ),
            tags$li(
              strong("Likelihood Profiles:"),
              "Changing one parameter or model specification across a series of values to see how the model fit and outputs change. A way to demonstrate both within model and among model uncertainty."
            ),
          ),
        ),
      )

      estimation_text
    })
  })

  ######################
  # Sampling abundance #
  ######################
  nav_hide("navbar", "abundance")
  observeEvent(input$goto_abundance, {
    nav_show("navbar", "abundance")
    nav_select("navbar", "abundance")
    set.seed(runif(1, 1, 2000000)) # For reproducible results

    fish_data <- data.frame(
      cell_id = 1:25,
      row = rep(1:5, each = 5),
      col = rep(1:5, times = 5),
      fish_count = sample(
        0:100,
        25,
        replace = TRUE,
        prob = (c(0.05, rep(((1 - 0.05) / 100), 100)))
      ),
      stringsAsFactors = FALSE
    )

    fish_data$fish_count <- round(
      (fish_data$fish_count / sum(fish_data$fish_count)) * 1000,
      0
    )

    fish_data_use <- reactiveVal(fish_data)
    pop_samples <- reactiveVal(data.frame(Sampled = "", Population = ""))

    #Set-up fishing cells with user defined population size
    observeEvent(input$pick_pop, {
      #set.seed(runif(1,1,2000000))
      fish_data <- data.frame(
        cell_id = 1:25,
        row = rep(1:5, each = 5),
        col = rep(1:5, times = 5),
        fish_count = sample(
          0:100,
          25,
          replace = TRUE,
          prob = (c(input$zero_cells, rep(((1 - input$zero_cells) / 100), 100)))
        ),
        stringsAsFactors = FALSE
      )
      fish_data$fish_count <- round(
        (fish_data$fish_count / sum(fish_data$fish_count)) * input$pop_size,
        0
      )
      fish_data_use(fish_data)
    })

    # Reactive values to store selected cells
    selected_cells <- reactiveVal(sample(1:25, 5)) # Initial selection

    # Initial selection

    observeEvent(input$random_cells, {
      random_choice <- sample(1:25, input$cell_num)
      selected_cells(random_choice)
      #    output$random.cells.out<-renderText({
      #      random_choice
      #    })
    })

    observeEvent(input$fish_hot, {
      fish.dat <- fish_data_use()
      hot_choice <- sort(
        fish.dat$fish_count,
        decreasing = TRUE,
        index.return = TRUE
      )$ix[1:input$cell_num]
      selected_cells(hot_choice)
    })

    # Handle plot clicks
    observeEvent(input$plot_click, {
      # Convert plot coordinates to cell ID
      x <- round(input$plot_click$x)
      y <- round(input$plot_click$y)

      # Check if click is within grid bounds
      if (x >= 1 && x <= 5 && y >= 1 && y <= 5) {
        # Convert to cell ID (remember y is flipped in the plot)
        row_num <- 6 - y # Flip y coordinate
        cell_id <- (row_num - 1) * 5 + x

        # Toggle cell selection
        current_selection <- selected_cells()
        if (cell_id %in% current_selection) {
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
      if (length(sel_cells) == 0) {
        return("No cells selected")
      }
      paste("Cells:", paste(sel_cells, collapse = ", "))
    })

    # Reactive data for selected cells
    selected_fish_data <- reactive({
      sel_cells <- selected_cells()
      if (length(sel_cells) == 0) {
        return(data.frame())
      }
      fish.dat <- fish_data_use()
      fish.dat[fish.dat$cell_id %in% sel_cells, ]
    })

    # Population statistics (all cells)
    population_summary <- reactive({
      fish.dat <- fish_data_use()
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

      if (nrow(selected_data) == 0) {
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
      fish.dat <- fish_data_use()
      sel_cells <- selected_cells()
      colors <- ifelse(
        fish.dat$cell_id %in% sel_cells,
        "lightblue",
        "lightgray"
      )

      par(mar = c(3, 3, 2, 1))
      plot(
        0,
        0,
        type = "n",
        xlim = c(0.5, 5.5),
        ylim = c(0.5, 5.5),
        xlab = "Column",
        ylab = "Row",
        main = "",
        xaxt = "n",
        yaxt = "n"
      )

      # Add grid lines and cell information
      for (i in 1:5) {
        for (j in 1:5) {
          cell_id <- (i - 1) * 5 + j
          fish_count <- fish.dat$fish_count[cell_id]

          # Draw rectangle for each cell with thicker border for selected cells
          border_width <- ifelse(cell_id %in% sel_cells, 3, 1)
          border_color <- ifelse(cell_id %in% sel_cells, "darkblue", "black")

          rect(
            j - 0.4,
            (6 - i) - 0.4,
            j + 0.4,
            (6 - i) + 0.4,
            col = colors[cell_id],
            border = border_color,
            lwd = border_width
          )

          # Add cell ID and fish count
          text(
            j,
            (6 - i),
            paste("Cell", cell_id, "\n", fish_count, "fish"),
            cex = 1.5,
            font = 2
          )
        }
      }

      # Add axis labels
      axis(1, at = 1:5, labels = 1:5)
      axis(2, at = 1:5, labels = 5:1)

      # Add legend
      legend(
        "topright",
        legend = c("Selected", "Not Selected"),
        fill = c("lightblue", "lightgray"),
        cex = 0.8
      )

      # Add instruction text
      mtext(
        "Click on cells to toggle selection",
        side = 3,
        line = 0.5,
        cex = 0.8,
        col = "gray50"
      )
    })

    #Sample table
    output$sample_table <- renderTable(
      {
        sample_stats <- sample_summary()
        pop_stats <- population_summary()
        sample_dt <- data.frame(
          Sample = c(
            sample_stats$cells_sampled,
            sample_stats$total_fish,
            sample_stats$mean_fish * 25,
            sample_stats$mean_fish,
            sample_stats$median_fish,
            sample_stats$sd_fish,
            round(sample_stats$sd_fish / sample_stats$mean_fish, 2),
            sample_stats$min_fish,
            sample_stats$max_fish
          ),
          Population = c(
            25,
            NA,
            pop_stats$total_fish,
            pop_stats$mean_fish,
            pop_stats$median_fish,
            pop_stats$sd_fish,
            round(pop_stats$sd_fish / pop_stats$mean_fish, 2),
            pop_stats$min_fish,
            pop_stats$max_fish
          )
        )
        rownames(sample_dt) <- c(
          "Total Cells",
          "Sampled Fish",
          "Total Fish",
          "Mean Fish/Cell",
          "Median Fish/Cell",
          "Std Dev",
          "CV",
          "Sample min",
          "Sample max"
        )
        sample_dt
      },
      rownames = TRUE
    )

    # Population statistics display
    output$population_stats <- renderText({
      pop_stats <- population_summary()

      paste(
        "Total Cells: 25\n",
        "Total Fish:",
        pop_stats$total_fish,
        "\n",
        "Mean Fish/Cell:",
        pop_stats$mean_fish,
        "\n",
        "Median Fish/Cell:",
        pop_stats$median_fish,
        "\n",
        "Std Dev:",
        pop_stats$sd_fish,
        "\n",
        "CV:",
        round(pop_stats$sd_fish / pop_stats$mean_fish, 2),
        "\n",
        "Range:",
        pop_stats$min_fish,
        "-",
        pop_stats$max_fish
      )
    })

    # Sample statistics display
    output$sample_stats <- renderText({
      sample_stats <- sample_summary()

      if (is.null(sample_stats)) {
        return("No cells selected for sampling")
      }

      paste(
        "Sampled Cells:",
        sample_stats$cells_sampled,
        "\n",
        "Total Fish Sampled:",
        sample_stats$total_fish,
        "\n",
        "Total Fish Estimated:",
        sample_stats$mean_fish * 25,
        "\n",
        "Mean Fish/Cell:",
        sample_stats$mean_fish,
        "\n",
        "Median Fish/Cell:",
        sample_stats$median_fish,
        "\n",
        "Std Dev:",
        sample_stats$sd_fish,
        "\n",
        "CV:",
        round(sample_stats$sd_fish / sample_stats$mean_fish, 2),
        "\n",
        "Range:",
        sample_stats$min_fish,
        "-",
        sample_stats$max_fish
      )
    })

    # Comparison statistics
    output$comparison_stats <- renderText({
      pop_stats <- population_summary()
      sample_stats <- sample_summary()

      if (is.null(sample_stats)) {
        return("No sample selected for comparison")
      }

      # Calculate differences and sampling coverage
      mean_diff <- sample_stats$mean_fish - pop_stats$mean_fish
      mean_error_pct <- round((mean_diff / pop_stats$mean_fish) * 100, 1)
      sampling_pct <- round(
        (sample_stats$cells_sampled / pop_stats$cells_total) * 100,
        1
      )

      paste(
        "Sampling Coverage:",
        sampling_pct,
        "%\n",
        "Mean Difference:",
        round(mean_diff, 2),
        "\n",
        "Mean Error:",
        mean_error_pct,
        "%\n",
        "Sample Representativeness:",
        ifelse(
          abs(mean_error_pct) < 10,
          "Good",
          ifelse(abs(mean_error_pct) < 20, "Fair", "Poor")
        )
      )
    })

    #Capture index measures for chosen sampling
    observeEvent(input$save_sample, {
      pop_stats <- population_summary()
      sample_stats <- sample_summary()
      pop_samples_cap <- rbind(
        pop_samples(),
        c(sample_stats$mean_fish, pop_stats$mean_fish)
      )
      #rownames(pop_samples_cap)<-c("Sampled Population","True Population")
      pop_samples(pop_samples_cap)
    })

    output$pop_samples_out <- renderTable({
      pop_samples()
    })

    #Clear the saved samples
    observeEvent(input$clear_samples, {
      pop_samples(data.frame(Sampled = "", Population = ""))
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
      plotdata <- pop_samples()
      if (any(plotdata > 0)) {
        plotdata <- plotdata[-1, ]
        plot.index.s <- data.frame(
          Sample = 1:length(plotdata$Sampled),
          Index = as.numeric(plotdata$Sampled),
          Type = "Sampled"
        )
        plot.index.p <- data.frame(
          Sample = 1:length(plotdata$Population),
          Index = as.numeric(plotdata$Population),
          Type = "Population"
        )
        plot.index <- rbind(plot.index.s, plot.index.p)

        index_plot <- ggplot(
          plot.index,
          aes(x = Sample, y = Index, color = Type)
        ) +
          geom_point(aes(shape = Type)) +
          geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
          ylim(c(0, NA)) +
          guides(color = "none") +
          theme_bw()

        index_plot
      }
    })
  })

  ######################
  #### Sampling age ####
  ######################
  nav_hide("navbar", "agesamp")
  observeEvent(input$goto_agesamp, {
    nav_show("navbar", "agesamp")
    nav_select("navbar", "agesamp")
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
        length = simpop$length,
        L50_asc = input$L50_asc.pval,
        L95_asc = input$L95_asc.pval,
        peak_length = input$peak_length.pval,
        desc_sd = input$desc_sd.pval
      )

      Numages_simpop <- data.frame(
        Age = simpop$age,
        Length = simpop$length,
        Numbers = simpop$numbers * samp.select,
        Selectivity = samp.select
      )

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
          numvec = c(
            25,
            50,
            seq(100, 1000, 100),
            seq(1500, input$maxsamp, 500)
          ),
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
        geom_hline(
          yintercept = input$Plim.pval,
          col = "red",
          linetype = "dotted"
        ) +
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
        geom_hline(
          yintercept = -(input$M.pval + input$F.pval),
          col = "red",
          linetype = "dotted"
        ) +
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
        geom_hline(yintercept = true_Z, col = "red", linetype = "dotted") +
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
      samp.nums.plot <- samp$Prop[
        (input$CC.sel_agemin + 1):(input$CC.sel_agemax)
      ]
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
  })

  ###########################
  # Biological compositions #
  ###########################
  nav_hide("navbar", "biocomps")

  observeEvent(input$goto_biocomps, {
    nav_show("navbar", "biocomps")
    nav_select("navbar", "biocomps")

    # Reactive validation and warnings
    observe({
      # Validate ascending limb
      if (input$L95_asc <= input$L50_asc) {
        showNotification(
          "Warning: L95 should be greater than L50 for ascending limb",
          type = "warning",
          duration = 3
        )
      }

      # Check for logical peak
      if (input$peak_length <= input$L95_asc) {
        showNotification(
          "Warning: Peak length should be greater than ascending L95 for proper dome shape",
          type = "warning",
          duration = 3
        )
      }

      # Check standard deviation
      if (input$desc_sd <= 0) {
        showNotification(
          "Warning: Standard deviation must be positive",
          type = "warning",
          duration = 3
        )
      }
    })

    # Calculate L50 and L95 equivalents for the descending limb
    desc_params <- reactive({
      # For a normal curve, calculate where selectivity drops to 50% and 5% (equivalent to 95% on ascending)
      # These occur at distances from the peak
      L50_desc <- input$peak_length + input$desc_sd * sqrt(2 * log(2)) # ~0.83 * sd from peak
      L05_desc <- input$peak_length + input$desc_sd * sqrt(2 * log(20)) # ~2.45 * sd from peak

      list(L50 = L50_desc, L05 = L05_desc)
    })

    # Reactive calculations
    populations <- reactive({
      ages <- 0:(5.4 / input$M_bc) #Using the Cope and Hamel tmax to M relationship

      # Calculate unfished population
      unfished <- calculate_population(
        ages = ages,
        Linf = input$Linf_bc,
        K = input$K_bc,
        t0 = input$t0_bc,
        M = input$M_bc,
        R0 = 1000,
        F_mort = 0
      )

      # Calculate fished population
      fished <- calculate_population(
        ages = ages,
        Linf = input$Linf_bc,
        K = input$K_bc,
        t0 = input$t0_bc,
        M = input$M_bc,
        R0 = 1000,
        F_mort = input$F_mort,
        L50_asc = input$L50_asc,
        L95_asc = input$L95_asc,
        peak_length = input$peak_length,
        desc_sd = input$desc_sd
      )

      maturity <- 1 /
        (1 +
          exp(
            -log(19) *
              ((unfished$length - input$L50_bc) / (input$L95_bc - input$L50_bc))
          ))
      unfished$maturity <- fished$maturity <- maturity

      list(unfished = unfished, fished = fished)
    })

    stock_status <- reactive({
      pops <- populations()
      calculate_stock_status(
        pops$fished,
        pops$unfished,
        input$L50_bc,
        input$L95_bc
      )
    })

    mean_bio_comp <- reactive({
      pops <- populations()

      #Mean Age
      age_data_fished <- do.call(
        c,
        mapply(
          function(x) rep(pops$fished$age[x], round(pops$fished$numbers[x], 0)),
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )
      age_data_fish_df <- data.frame(
        age = age_data_fished,
        population = "Fished"
      )
      age_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rep(pops$unfished$age[x], round(pops$unfished$numbers[x], 0))
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )
      age_data_unfish_df <- data.frame(
        age = age_data_unfished,
        population = "Unfished"
      )
      mean_age_status <- mean(age_data_fish_df$age) /
        mean(age_data_unfish_df$age)

      #Mean Length
      lt_data_fished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$fished$numbers[x], 0),
              pops$fished$length[x],
              pops$fished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )
      lt_data_fish_df <- data.frame(
        length = lt_data_fished,
        population = "Fished"
      )
      lt_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$unfished$numbers[x], 0),
              pops$unfished$length[x],
              pops$unfished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )
      lt_data_unfish_df <- data.frame(
        length = lt_data_unfished,
        population = "Unfished"
      )
      mean_lt_status <- mean(lt_data_fish_df$length) /
        mean(lt_data_unfish_df$length)

      mean_bio_comp <- c(mean_age_status, mean_lt_status)
      mean_bio_comp
    })

    sel_mean_bio_comp <- reactive({
      pops <- populations()

      #Calculate sampled unfished based on selectivity of fishery
      sel_mean_lt_unfished <- sum(
        pops$unfished$length * pops$unfished$numbers * pops$fished$selectivity
      ) /
        sum(pops$unfished$numbers * pops$fished$selectivity)
      sel_mean_lt_fished <- sum(
        pops$fished$length * pops$fished$numbers * pops$fished$selectivity
      ) /
        sum(pops$fished$numbers * pops$fished$selectivity)
      sel_mean_lt <- sel_mean_lt_fished / sel_mean_lt_unfished

      sel_mean_age_unfished <- sum(
        pops$unfished$age * pops$unfished$numbers * pops$fished$selectivity
      ) /
        sum(pops$unfished$numbers * pops$fished$selectivity)
      sel_mean_age_fished <- sum(
        pops$fished$age * pops$fished$numbers * pops$fished$selectivity
      ) /
        sum(pops$fished$numbers * pops$fished$selectivity)
      sel_mean_age <- sel_mean_age_fished / sel_mean_age_unfished

      sel_mean_bio_comp <- c(sel_mean_age, sel_mean_lt)
      sel_mean_bio_comp
    })

    # Download population outputs
    output$download_pops <- downloadHandler(
      filename = function() {
        paste0("pop_out_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(populations())
        write.csv(populations(), file, row.names = TRUE)
      }
    )

    # Download population outputs
    output$download_lengths <- downloadHandler(
      filename = function() {
        paste0("lengths_out_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(Lt.samples())
        write.csv(Lt.samples(), file, row.names = TRUE)
      }
    )

    # Age structure plot
    output$age_plot <- renderPlotly({
      pops <- populations()

      plot_data_fished <- do.call(
        c,
        mapply(
          function(x) rep(pops$fished$age[x], round(pops$fished$numbers[x], 0)),
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )
      plot_data_fish_df <- data.frame(
        age = plot_data_fished,
        population = "Fished"
      )
      plot_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rep(pops$unfished$age[x], round(pops$unfished$numbers[x], 0))
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )
      plot_data_unfish_df <- data.frame(
        age = plot_data_unfished,
        population = "Unfished"
      )
      plot_data <- rbind(plot_data_fish_df, plot_data_unfish_df)

      p <- ggplot(plot_data, aes(x = age, color = population)) +
        geom_density(alpha = 0.5, size = 1, show.legend = FALSE) +
        #geom_line(size = 1.2) +
        #geom_point(size = 2) +
        #scale_fill_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
        scale_color_manual(
          values = c(
            "Fished" = viridis(1, option = "plasma"),
            "Unfished" = "gray49"
          )
        ) +
        labs(
          x = "Age (years)",
          y = "Frequency",
          fill = "Population Type",
          title = "Total Age Distribution: Fished vs Unfished"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        geom_vline(
          xintercept = 5.4 / input$M_bc,
          linetype = "dashed",
          col = c("purple")
        )

      ggplotly(p, tooltip = c("x", "y", "colour"))
    })

    # Selected Age compositions
    output$age_sel_plot <- renderPlotly({
      pops <- populations()

      plot_data_fished <- do.call(
        c,
        mapply(
          function(x) {
            rep(
              pops$fished$age[x],
              round(pops$fished$numbers[x] * pops$fished$selectivity[x], 0)
            )
          },
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )
      plot_data_fish_df <- data.frame(
        age = plot_data_fished,
        population = "Fished"
      )
      plot_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rep(
              pops$unfished$age[x],
              round(pops$unfished$numbers[x] * pops$fished$selectivity[x], 0)
            )
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )
      plot_data_unfish_df <- data.frame(
        age = plot_data_unfished,
        population = "Unfished"
      )
      plot_data <- rbind(plot_data_fish_df, plot_data_unfish_df)

      mean.age.fish <- mean(plot_data_fish_df$age)
      mean.age.unfish <- mean(plot_data_unfish_df$age)

      dens_fished <- density(plot_data_fish_df$age, adjust = 1)

      mean_density_fished <- approx(
        dens_fished$x,
        dens_fished$y,
        xout = mean.age.fish
      )$y

      dens_unfished <- density(
        plot_data_unfish_df$age,
        adjust = 1
      )
      mean_density_unfished <- approx(
        dens_unfished$x,
        dens_unfished$y,
        xout = mean.age.unfish
      )$y

      max.d <- max(c(dens_fished$y, dens_unfished$y))

      p <- ggplot(plot_data, aes(x = age, color = population)) +
        geom_density(alpha = 0.5, size = 1, show.legend = FALSE) +
        #scale_fill_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
        scale_color_manual(
          values = c(
            "Fished" = viridis(1, option = "plasma"),
            "Unfished" = "gray49"
          )
        ) +
        labs(
          x = "Age (years)",
          y = "Frequency",
          fill = "Population Type",
          title = "Selected Age Distribution: Fished vs Unfished"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        geom_vline(
          xintercept = 5.4 / input$M_bc,
          linetype = "dashed",
          col = c("purple")
        ) +
        annotate(
          "text",
          x = 5.4 / input$M_bc,
          y = max.d + max.d * 0.1,
          label = "Amax",
          col = "purple",
          size = 2.5
        ) +
        geom_point(
          aes(
            x = mean.age.fish,
            y = mean_density_fished
          ),
          color = viridis(1, option = "plasma"),
          size = 2,
          shape = 19
        ) +
        geom_point(
          aes(
            x = mean.age.unfish,
            y = mean_density_unfished
          ),
          color = "gray49",
          size = 2,
          shape = 19
        )
      # +
      #   annotate(
      #     "text",
      #     x = mean.age.fish,
      #     y = mean_density_fished * 1.1,
      #     label = "Mean",
      #     color = viridis(1, option = "plasma"),
      #     fontface = "bold",
      #     hjust = -0.1
      #   ) +
      #   annotate(
      #     "text",
      #     x = mean.age.unfish,
      #     y = mean_density_unfished * 1.1,
      #     label = "Mean",
      #     color = "gray49",
      #     fontface = "bold",
      #     hjust = -0.1
      #   )

      ggplotly(p, tooltip = c("x", "y", "colour"))
    })

    #Length densities
    Lt.densities <- reactive({
      set.seed(19)
      pops <- populations()

      #Total fish
      lt_data <- rbind(
        data.frame(pops$unfished, population = "Unfished"),
        data.frame(pops$fished, population = "Fished")
      )

      lt_data_fished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$fished$numbers[x], 0),
              pops$fished$length[x],
              pops$fished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )

      lt_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$unfished$numbers[x], 0),
              pops$unfished$length[x],
              pops$unfished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )

      dens_fished <- density(lt_data_fished, adjust = 1)

      dens_unfished <- density(
        lt_data_unfished,
        adjust = 1
      )

      # lt_data_fish_dens <- data.frame(
      #   length = dens_fished,
      #   population = "Fished"
      # )

      # lt_data_unfish_dens <- data.frame(
      #   length = dens_unfished,
      #   population = "Fished"
      # )

      #Selected fish
      sel_lt_data_fished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$fished$numbers[x] * pops$fished$selectivity[x], 0),
              pops$fished$length[x],
              pops$fished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )

      sel_lt_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$unfished$numbers[x] * pops$fished$selectivity[x], 0),
              pops$unfished$length[x],
              pops$unfished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )

      sel_dens_fished <- density(sel_lt_data_fished, adjust = 1)

      sel_dens_unfished <- density(
        sel_lt_data_unfished,
        adjust = 1
      )

      # sel_lt_data_unfish_dens <- data.frame(
      #   length = sel_dens_unfished,
      #   population = "Fished"
      # )

      # sel_lt_data_fish_dens <- data.frame(
      #   length = sel_dens_unfished,
      #   population = "Fished"
      # )

      Lt_densities <- list(
        Total_den_unfished = dens_unfished,
        Total_den_fished = dens_fished,
        Selected_den_unfished = sel_dens_unfished,
        Selected_den_fished = sel_dens_fished
      )

      Lt_densities
    })

    Lt.samples <- reactive({
      Lt.densities <- Lt.densities()
      dens_unfished <- Lt.densities$Total_den_unfished
      dens_fished <- Lt.densities$Total_den_fished
      sel_dens_unfished <- Lt.densities$Selected_den_unfished
      sel_dens_fished <- Lt.densities$Selected_den_fished

      lt_samp_tot_unfished <- sample(
        dens_unfished$x,
        input$lt_den_samples,
        replace = TRUE,
        prob = dens_unfished$y
      )
      lt_samp_tot_fished <- sample(
        dens_fished$x,
        input$lt_den_samples,
        replace = TRUE,
        prob = dens_fished$y
      )
      lt_samp_sel_unfished <- sample(
        sel_dens_unfished$x,
        input$lt_den_samples,
        replace = TRUE,
        prob = sel_dens_unfished$y
      )
      lt_samp_sel_fished <- sample(
        sel_dens_fished$x,
        input$lt_den_samples,
        replace = TRUE,
        prob = sel_dens_fished$y
      )

      Lt.samples <- data.frame(
        Total_fish = lt_samp_tot_fished,
        Total_unfished = lt_samp_tot_unfished,
        Selected_fish = lt_samp_sel_fished,
        Selected_unfished = lt_samp_sel_unfished
      )
      Lt.samples
    })

    # Total Length structure plot
    output$length_plot <- renderPlotly({
      set.seed(19)
      pops <- populations()
      plot_data <- rbind(
        data.frame(pops$unfished, population = "Unfished"),
        data.frame(pops$fished, population = "Fished")
      )

      plot_data_fished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$fished$numbers[x], 0),
              pops$fished$length[x],
              pops$fished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )
      plot_data_fish_df <- data.frame(
        length = plot_data_fished,
        population = "Fished"
      )
      plot_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$unfished$numbers[x], 0),
              pops$unfished$length[x],
              pops$unfished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )
      plot_data_unfish_df <- data.frame(
        length = plot_data_unfished,
        population = "Unfished"
      )
      plot_data <- rbind(plot_data_fish_df, plot_data_unfish_df)

      p <- ggplot(plot_data, aes(x = length, color = population)) +
        geom_density(alpha = 0.5, size = 1, show.legend = FALSE) +
        scale_color_manual(
          values = c(
            "Fished" = viridis(1, option = "plasma"),
            "Unfished" = "gray49"
          )
        ) +
        labs(
          x = "Length",
          y = "Frequency",
          title = "Total Length Distribution: Fished vs Unfished"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        geom_vline(
          xintercept = c(input$L50, input$Linf),
          linetype = "dashed",
          col = c("darkgreen", "orange")
        )

      ggplotly(p, tooltip = c("x", "y", "colour"))
    })

    # Selected Length structure plot
    output$length_sel_plot <- renderPlotly({
      set.seed(19)
      pops <- populations()

      plot_data_fished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$fished$numbers[x] * pops$fished$selectivity[x], 0),
              pops$fished$length[x],
              pops$fished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )
      plot_data_fish_df <- data.frame(
        length = plot_data_fished,
        population = "Fished"
      )
      plot_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$unfished$numbers[x] * pops$fished$selectivity[x], 0),
              pops$unfished$length[x],
              pops$unfished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )
      plot_data_unfish_df <- data.frame(
        length = plot_data_unfished,
        population = "Unfished"
      )

      plot_data <- rbind(plot_data_fish_df, plot_data_unfish_df)
      max.d <- max(density(plot_data$length)$y)
      #col.line<-viridis(1,option="plasma")

      mean.lt.fish <- mean(plot_data_fish_df$length)
      mean.lt.unfish <- mean(plot_data_unfish_df$length)

      dens_fished <- density(plot_data_fish_df$length, adjust = 1)

      mean_density_fished <- approx(
        dens_fished$x,
        dens_fished$y,
        xout = mean.lt.fish
      )$y

      dens_unfished <- density(
        plot_data_unfish_df$length,
        adjust = 1
      )
      mean_density_unfished <- approx(
        dens_unfished$x,
        dens_unfished$y,
        xout = mean.lt.unfish
      )$y

      max.d <- max(c(dens_fished$y, dens_unfished$y))

      p <- ggplot(plot_data, aes(x = length, color = population)) +
        geom_density(alpha = 0.5, size = 1, show.legend = FALSE) +
        #scale_fill_manual(values = c("Fished" = viridis(1,option="plasma"), "Unfished" = viridis(2,option="plasma"))) +
        scale_color_manual(
          values = c(
            "Fished" = viridis(1, option = "plasma"),
            "Unfished" = "gray49"
          )
        ) +
        labs(
          x = "Length",
          y = "Proportion",
          title = "Selected Length Distribution: Fished vs Unfished"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        geom_vline(
          xintercept = c(input$L50_bc, input$Linf_bc),
          linetype = "dashed",
          col = c("green", "orange")
        ) +
        annotate(
          "text",
          x = input$L50_bc + input$Linf_bc * 0.05,
          y = max.d + max.d * 0.1,
          label = "L50",
          col = "green",
          size = 2.5
        ) +
        annotate(
          "text",
          x = input$Linf_bc + input$Linf_bc * 0.05,
          y = max.d + max.d * 0.1,
          label = "Linf",
          col = "orange",
          size = 2.5
        ) +
        geom_point(
          aes(
            x = mean.lt.fish,
            y = mean_density_fished
          ),
          color = viridis(1, option = "plasma"),
          size = 2,
          shape = 19
        ) +
        geom_point(
          aes(
            x = mean.lt.unfish,
            y = mean_density_unfished
          ),
          color = "gray49",
          size = 2,
          shape = 19
        )

      ggplotly(p, tooltip = c("x", "y", "colour"))
    })

    # Selectivity plot
    output$selectivity_plot_lt <- renderPlot({
      ages <- 1:(5.4 / input$M_bc)
      lengths <- c(0:input$Linf_bc + 0.2 * input$Linf_bc)

      #lengths <- von_bertalanffy(ages, input$Linf_bc, input$K_bc, input$t0_bc)
      selectivity <- calc_selectivity(
        length = lengths,
        L50_asc = input$L50_asc,
        L95_asc = input$L95_asc,
        peak_length = input$peak_length,
        desc_sd = input$desc_sd
      )

      sel_data <- data.frame(Length = lengths, Selectivity = selectivity)
      sel.pts <- data.frame(
        Length = c(input$L50_asc, input$L95_asc),
        Prop = c(0.5, 0.95)
      )
      maturity_lengths <- 1 /
        (1 +
          exp(
            -log(19) * (lengths - input$L50_bc) / (input$L95_bc - input$L50_bc)
          ))
      mat.pts <- data.frame(
        Length = c(input$L50_bc, input$L95_bc),
        Prop = c(0.5, 0.95)
      )

      desc_vals <- desc_params()

      p <- ggplot(sel_data, aes(x = Length, y = Selectivity)) +
        geom_line(aes(color = "black"), size = 1.5) +
        geom_point(
          data = sel.pts,
          aes(x = Length, y = Prop, col = "black"),
          fill = "gray",
          size = 5,
          pch = 21
        ) +
        annotate(
          "text",
          sel.pts$Length - (input$Linf_bc * 0.08),
          sel.pts$Prop,
          label = c("Sel50%", "Sel95%")
        ) +
        #geom_point(aes(x=input$L95_asc,y=0.95,col="black"),fill="gray",size=4,pch=21)+
        geom_line(aes(y = maturity_lengths, col = "pink"), size = 1.5) +
        geom_point(
          data = mat.pts,
          aes(x = Length, y = Prop, col = "pink"),
          fill = "pink4",
          size = 5,
          pch = 21
        ) +
        annotate(
          "text",
          mat.pts$Length + input$Linf_bc * 0.08,
          mat.pts$Prop,
          label = c("Lmat50%", "Lmat95%")
        ) +
        #geom_point(aes(x=input$L95,y=0.95,col="pink"),fill="pink4",size=4,pch=21)+
        #geom_point(aes(x=input$peak_length,y=1,col="purple"),fill="violet",size=4,pch=21)+
        #geom_hline(yintercept = c(0.5, 0.95), linetype = "dashed", alpha = 0.6, color = "blue") +
        #geom_vline(xintercept = c(input$L50_asc, input$L95_asc),
        #           linetype = "dashed", alpha = 0.6, color = "darkgreen") +
        geom_vline(
          xintercept = input$peak_length,
          linetype = "dotted",
          alpha = 0.8,
          color = "purple",
          size = 1
        ) +
        #      if(input$peak_length<input$Linf){geom_vline(xintercept = input$peak_length,
        #                linetype = "solid", alpha = 0.8, color = "purple", size = 1)} +
        geom_vline(
          xintercept = c(desc_vals$L50, desc_vals$L05),
          linetype = "dashed",
          alpha = 0.6,
          color = "orange"
        ) +
        labs(
          title = "Selectivity and Maturity Curves",
          subtitle = paste(
            "Peak selectivity at length",
            input$peak_length,
            "| Descending SD =",
            input$desc_sd
          ),
          x = "Length",
          y = "Selectivity",
          #        caption = "Green lines: Ascending limb | Purple line: Peak | Orange lines: Descending normal curve (50% & 5%)"
          caption = "Dots: 50% and 95% selectivity/maturity | Purple line: Peak | Orange lines: Descending normal curve (50% & 5%)"
        ) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
        xlim(0, (input$Linf + round(input$Linf * 0.1, 0))) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.position = "bottom"
        ) +
        scale_color_manual(
          labels = c("Selectivity", "Maturity"),
          values = c("black", "pink")
        ) +
        labs(color = "")
      p
    })

    #Growth and Mortality plot
    output$growth_M_plot <- renderPlot({
      set.seed(19)

      pops <- populations()
      #Ages
      age_data_fished <- do.call(
        c,
        mapply(
          function(x) {
            rep(
              pops$fished$age[x],
              round(pops$fished$numbers[x] * pops$fished$selectivity[x], 0)
            )
          },
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )
      age_data_fish_df <- data.frame(
        age = age_data_fished,
        population = "Fished"
      )
      age_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rep(pops$unfished$age[x], round(pops$unfished$numbers[x], 0))
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )
      age_data_unfish_df <- data.frame(
        age = age_data_unfished,
        population = "Unfished"
      )

      #Lengths
      lt_data_fished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$fished$numbers[x] * pops$fished$selectivity[x], 0),
              pops$fished$length[x],
              pops$fished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )
      lt_data_fish_df <- data.frame(
        length = lt_data_fished,
        population = "Fished"
      )
      lt_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$unfished$numbers[x], 0),
              pops$unfished$length[x],
              pops$unfished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )
      lt_data_unfish_df <- data.frame(
        length = lt_data_unfished,
        population = "Unfished"
      )

      agelt_fished_df <- cbind(age_data_fish_df$age, lt_data_fish_df)
      agelt_unfished_df <- cbind(age_data_unfish_df$age, lt_data_unfish_df)

      #age_lt_data_df<-cbind(age_data_df$age,lt_data_df)
      colnames(agelt_unfished_df)[1] <- colnames(agelt_fished_df)[1] <- "age"

      #Calculate age at maturity lengths
      age.mat <- input$t0_bc -
        (log(1 - (c(input$L50_bc, input$L95_bc) / input$Linf_bc)) / input$K_bc)
      age.lt.mat <- data.frame(
        Age = age.mat,
        Length = c(input$L50_bc, input$L95_bc)
      )

      data <- populations()$unfished

      # Create the plot with dual y-axes
      # First, we need to scale the population data to fit with length data
      length_range <- range(data$length)
      pop_range <- c(0, 1)

      # Scale population to length scale for plotting
      pop_scaled <- (data$numbers / 1000 - pop_range[1]) /
        (pop_range[2] - pop_range[1]) *
        (length_range[2] - length_range[1]) +
        length_range[1]
      color.line.in <- viridis(3)

      # Create the plot
      p <- ggplot(data, aes(x = age)) +
        geom_line(
          aes(y = pop_scaled, color = "Population"),
          size = 1.2,
          linetype = "dashed"
        ) +
        geom_point(
          aes(y = pop_scaled, color = "Population"),
          size = 2,
          shape = 17
        ) +
        geom_point(
          data = agelt_unfished_df,
          aes(age, length),
          col = "gray49",
          alpha = 0.3
        ) +
        geom_point(
          data = agelt_fished_df,
          aes(age, length),
          col = viridis(1, option = "plasma"),
          alpha = 0.4
        ) +
        geom_line(aes(y = length, color = "Length"), size = 1.2) +
        geom_point(aes(y = length, color = "Length"), size = 2) +
        geom_point(
          data = age.lt.mat,
          aes(x = Age, y = Length),
          size = 5,
          shape = 21,
          col = "black",
          fill = "lightblue"
        ) +
        annotate(
          "text",
          age.lt.mat$Age + (5.4 / input$M_bc) * 0.08,
          age.lt.mat$Length,
          label = c("Lmat50%", "Lmat95%")
        ) +

        # Add second y-axis
        scale_y_continuous(
          name = "Length",
          sec.axis = sec_axis(
            trans = ~ (. - length_range[1]) /
              (length_range[2] - length_range[1]) *
              (pop_range[2] - pop_range[1]) +
              pop_range[1],
            #           trans = ~ (. *length_range[2]*),
            name = "Proportion at Age"
          )
        ) +

        scale_color_manual(
          name = "Measure",
          values = c(
            "Length" = color.line.in[1],
            "Population" = color.line.in[2]
          ),
          labels = c(
            "Length" = "Length (cm)",
            "Population" = "Proportion at Age"
          )
        ) +

        labs(
          title = "Age-Length Relationship and Population Decline by Natural Mortality",
          subtitle = paste0(
            "von Bertalanffy: L∞=",
            input$Linf_bc,
            ", k=",
            input$K,
            ", t₀=",
            input$t0_bc,
            " | Natural Mortality: M=",
            input$M_bc,
            " | Maximum age: M=",
            5.4 / input$M_bc
          ),
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
    output$stock_status_tab <- renderTable(
      {
        status <- stock_status()
        mean_age_lt <- mean_bio_comp()
        sel_mean_age_lt <- sel_mean_bio_comp()

        data.frame(
          Metric = c(
            "Spawning Biomass Ratio (SSB/SSB0)",
            "Total Biomass Ratio (B/B0)",
            "Mean Length Ratio (fished/unfished)",
            "Mean Age Ratio (fished/unfished)",
            "Sampled Mean Length Ratio (fished/unfished)",
            "Sampled Mean Age Ratio (fished/unfished)"
            #"Spawning Biomass (Fished)",
            #"Spawning Biomass (Unfished)",
            #"Total Biomass (Fished)",
            #"Total Biomass (Unfished)"
          ),
          Value = c(
            round(status$SSB_ratio, 3),
            round(status$B_ratio, 3),
            mean_age_lt[2],
            mean_age_lt[1],
            sel_mean_age_lt[2],
            sel_mean_age_lt[1]
            #round(status$SSB_fished, 0),
            #round(status$SSB_unfished, 0),
            #round(status$B_fished, 0),
            #round(status$B_unfished, 0)
          )
        )
      },
      striped = TRUE,
      hover = TRUE
    )

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

      paste(
        interpretation,
        sprintf(
          "The fished population has %.1f%% of the unfished spawning biomass.",
          ssb_ratio * 100
        )
      )
    })

    results_out <- reactiveVal(data.frame(
      SSB_ratio = "",
      TB_ratio = "",
      Lt_ratio = "",
      Age_ratio = "",
      Sel_Lt_ratio = "",
      Sel_Age_ratio = "",
      L25per = "",
      L95per = "",
      M = "",
      Linf = "",
      K = "",
      t0 = "",
      L50 = "",
      L95 = "",
      Sel50 = "",
      Sel95 = ""
    ))

    #Capture index measures for chosen sampling
    observeEvent(input$save_results, {
      status <- stock_status()
      mean_age_lt <- mean_bio_comp()
      sel_mean_age_lt <- sel_mean_bio_comp()
      pops <- populations()

      lt_data_fished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$fished$numbers[x] * pops$fished$selectivity[x], 0),
              pops$fished$length[x],
              pops$fished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$fished),
          SIMPLIFY = TRUE
        )
      )
      lt_data_fish_df <- data.frame(
        length = lt_data_fished,
        population = "Fished"
      )
      lt_data_unfished <- do.call(
        c,
        mapply(
          function(x) {
            rnorm(
              round(pops$unfished$numbers[x] * pops$fished$selectivity[x], 0),
              pops$unfished$length[x],
              pops$unfished$length[x] * 0.1
            )
          },
          x = 1:nrow(pops$unfished),
          SIMPLIFY = TRUE
        )
      )
      lt_data_unfish_df <- data.frame(
        length = lt_data_unfished,
        population = "Unfished"
      )

      lt_data <- rbind(lt_data_fish_df, lt_data_unfish_df)

      results_cap <- rbind(
        results_out(),
        c(
          round(status$SSB_ratio, 2),
          round(status$B_ratio, 2),
          round(mean_age_lt[2], 2),
          round(mean_age_lt[1], 2),
          round(sel_mean_age_lt[2], 2),
          round(sel_mean_age_lt[1], 2),
          round(quantile(lt_data_fish_df$length, 0.25), 2),
          round(quantile(lt_data_fish_df$length, 0.95), 2),
          input$M_bc,
          input$Linf_bc,
          input$K_bc,
          input$t0_bc,
          input$L50_bc,
          input$L95_bc,
          input$L50_asc,
          input$L95_asc,
          input$peak_length,
          input$desc_sd
        )
      )
      #rownames(pop_samples_cap)<-c("Sampled Population","True Population")
      results_out(results_cap)
    })

    output$results_out <- renderTable({
      results_out()
    })

    #Clear the saved samples
    observeEvent(input$clear_results, {
      results_out(data.frame(
        SSB_ratio = "",
        TB_ratio = "",
        Lt_ratio = "",
        Age_ratio = "",
        Sel_Lt_ratio = "",
        Sel_Age_ratio = "",
        L25per = "",
        L95per = "",
        M = "",
        Linf = "",
        K = "",
        t0 = "",
        L50 = "",
        L95 = "",
        Sel50 = "",
        Sel95 = ""
      ))
    })
  })

  ##############
  # Indicators #
  ##############
  nav_hide("navbar", "indicators")

  observeEvent(input$goto_indicators, {
    nav_show("navbar", "indicators")
    nav_select("navbar", "indicators")

    stock.data <- readRDS("stock_data.rds")
    data.colors <- viridis(3)

    data.sub <- reactive({
      data.sub <- subset(stock.data, Stock == input$stock.choice)
    })

    # observeEvent(input$calculate_cr, {
    #   # Code here runs ONLY when 'event_expression' changes or is triggered
    #   print("An event occurred!")
    #   showModal(modalDialog(title = "Triggered", "The event happened!"))
    # })
    #Equation choices

    #CR_calc_Ct<-eventReactive(input$calculate_cr, {
    CR_calc_Ct <- reactive({
      CR_calc_Ct <- NA
      if (input$ct_equation_type == "ct_custom") {
        cr.calc.ct <- input$ct_custom_cr
      } else {
        cr.calc.ct <- switch(
          input$ct_equation_type,
          "ct_ratio" = "I/RP",
          "ct_cubic" = "0.2*((I/RP)-1)^3",
          "ct_cubicpoly" = "0.2*((I/RP)-1)^3+0.05*((I/RP)-1)"
        )
      }
      #      if(!is.null(input$data.id) & any(input$data.id=="Catch"))
      #      {
      I = input$Ct_I_in
      RP = input$Ct_RP_in
      if (I != 0 & RP != 0) {
        CR_calc_Ct <- eval(parse(text = cr.calc.ct))
      }
      #     }
      CR_calc_Ct
    })

    CR_calc_Ind <- reactive({
      CR_calc_Ind <- NA
      if (input$ind_equation_type == "ind_custom") {
        cr.calc.ind <- input$ind_custom_cr
      } else {
        cr.calc.ind <- switch(
          input$ind_equation_type,
          "ind_ratio" = "I/RP",
          "ind_cubic" = "0.2*((I/RP)-1)^3",
          "ind_cubicpoly" = "0.2*((I/RP)-1)^3+0.05*((I/RP)-1)"
        )
      }
      #      if(!is.null(input$data.id) & any(input$data.id=="Catch"))
      #      {
      I = input$I_I_in
      RP = input$I_RP_in
      if (I != 0 & RP != 0) {
        CR_calc_Ind <- eval(parse(text = cr.calc.ind))
      }
      #     }
      CR_calc_Ind
    })

    CR_calc_Lt <- reactive({
      CR_calc_Lt <- NA
      if (input$lt_equation_type == "lt_custom") {
        cr.calc.lt <- input$lt_custom_cr
      } else {
        cr.calc.lt <- switch(
          input$lt_equation_type,
          "lt_ratio" = "I/RP",
          "lt_cubic" = "0.2*((I/RP)-1)^3",
          "lt_cubicpoly" = "0.2*((I/RP)-1)^3+0.05*((I/RP)-1)"
        )
      }
      #      if(!is.null(input$data.id) & any(input$data.id=="Catch"))
      #      {
      I = input$Lt_I_in
      RP = input$Lt_RP_in
      if (I != 0 & RP != 0) {
        CR_calc_Lt <- eval(parse(text = cr.calc.lt))
      }
      #     }
      CR_calc_Lt
    })

    output$data.indicator <- renderUI({
      checkboxGroupButtons(
        inputId = "data.id",
        label = "Which data to plot: ",
        choices = c("Catch", "Index", "Mean Length"),
        size = "sm"
      )
    })

    output$download_indicator_data <- downloadHandler(
      filename = function() {
        paste(
          "stock_",
          input$stock.choice,
          "_indicator_data_",
          Sys.Date(),
          ".csv",
          sep = ""
        )
      },
      content = function(file) {
        req(data.sub())
        write.csv(data.sub(), file, row.names = FALSE)
      }
    )

    #Indicator plots
    output$stock_time_series_Ct_ui <- renderUI({
      if (!is.null(input$Ct_I_in)) {
        I_in <- NA
      }
      if (!is.null(input$Ct_RP_in)) {
        RP_in <- NA
      }
      if (input$Ct_I_in >= 0) {
        I_in <- input$Ct_I_in
      }
      if (input$Ct_RP_in >= 0) {
        RP_in <- input$Ct_RP_in
      }

      ct.label <- data.frame(
        Year = c(76, 76),
        Catch = c(I_in, RP_in),
        label = c("Indicator", "Ref. Pt.")
      )

      if (!is.null(input$data.id) & any(input$data.id == "Catch")) {
        output$stock_time_series_Ct <- renderPlotly({
          data.plot <- data.sub()
          ggplot(data.plot, aes(Year, Catch)) +
            geom_line(col = data.colors[1]) +
            geom_point(col = data.colors[1]) +
            geom_point(
              data = ct.label,
              aes(Year, Catch, fill = label),
              shape = c(22, 23),
              size = 3
            ) +
            scale_fill_manual(values = c("orange", "red")) +
            labs(fill = "") +
            theme_bw() +
            theme(legend.position = "bottom") +
            xlab("Year") +
            ylab("Total Catch")
        })
      }
    })

    output$stock_time_series_Index_ui <- renderUI({
      if (!is.null(input$I_I_in)) {
        I_in <- NA
      }
      if (!is.null(input$I_RP_in)) {
        RP_in <- NA
      }
      if (input$I_I_in >= 0) {
        I_in <- input$I_I_in
      }
      if (input$I_RP_in >= 0) {
        RP_in <- input$I_RP_in
      }

      ind.label <- data.frame(
        Year = c(76, 76),
        Index = c(I_in, RP_in),
        label = c("Indicator", "Ref. Pt.")
      )

      if (!is.null(input$data.id) & any(input$data.id == "Index")) {
        output$stock_time_series_Ct <- renderPlotly({
          data.plot <- data.sub()
          ggplot(data.plot, aes(Year, Index)) +
            geom_line(col = data.colors[2]) +
            geom_point(col = data.colors[2]) +
            geom_point(
              data = ind.label,
              aes(Year, Index, fill = label),
              shape = c(22, 23),
              size = 3
            ) +
            scale_fill_manual(values = c("orange", "red")) +
            labs(fill = "") +
            theme_bw() +
            theme(legend.position = "bottom") +
            xlab("Year") +
            ylab("Index")
        })
      }
    })

    output$stock_time_series_Lt_ui <- renderUI({
      if (!is.null(input$Lt_I_in)) {
        I_in <- NA
      }
      if (!is.null(input$Lt_RP_in)) {
        RP_in <- NA
      }
      if (input$Lt_I_in >= 0) {
        I_in <- input$Lt_I_in
      }
      if (input$Lt_RP_in >= 0) {
        RP_in <- input$Lt_RP_in
      }

      lt.label <- data.frame(
        Year = c(76, 76),
        Mean.Length = c(I_in, RP_in),
        label = c("Indicator", "Ref. Pt.")
      )

      if (!is.null(input$data.id) & any(input$data.id == "Mean Length")) {
        if (input$stock.choice == "A") {
          M.in <- 0.0375
          Linf.in <- 60.1
          k.in <- 0.08
          t0.in <- -0.55
          L50.in <- 46.5
        }

        if (input$stock.choice == "B") {
          M.in <- 0.068
          Linf.in <- 42.8
          k.in <- 0.13
          t0.in <- -0.94
          L50.in <- 29
        }

        if (input$stock.choice == "C") {
          M.in <- 0.145
          Linf.in <- 53
          k.in <- 0.143
          t0.in <- -0.07
          L50.in <- 42
        }

        if (input$stock.choice == "D") {
          M.in <- 0.099
          Linf.in <- 57.38
          k.in <- 0.128
          t0.in <- -2.4
          L50.in <- 39.4
        }

        output$stock_time_series_Ct <- renderPlotly({
          data.plot <- data.sub()
          ggplot(data.plot, aes(Year, Mean.Length)) +
            geom_line(col = data.colors[3]) +
            geom_point(col = data.colors[3]) +
            geom_point(
              data = lt.label,
              aes(Year, Mean.Length, fill = label),
              shape = c(22, 23),
              size = 3
            ) +
            scale_fill_manual(values = c("orange", "red")) +
            labs(fill = "") +
            theme_bw() +
            theme(legend.position = "bottom") +
            xlab("Year") +
            ylab("Mean Length") +
            geom_hline(
              aes(yintercept = Linf.in),
              col = "blue",
              linetype = "longdash"
            ) +
            geom_hline(
              aes(yintercept = L50.in),
              col = "purple",
              linetype = "dash"
            ) +
            #ylim(NA,NA)+
            annotate(
              "text",
              x = 1,
              y = Linf.in + 0.05 * Linf.in,
              label = "Linf",
              col = "blue"
            ) +
            annotate(
              "text",
              x = 1,
              y = L50.in + 0.05 * L50.in,
              label = "Lmat50%",
              col = "purple"
            )
        })
      }
    })

    output$LH_values <- renderTable(
      {
        if (input$stock.choice == "A") {
          M.in <- 0.0375
          Linf.in <- 60.1
          k.in <- 0.08
          t0.in <- -0.55
          L50.in <- 46.5
        }

        if (input$stock.choice == "B") {
          M.in <- 0.068
          Linf.in <- 42.8
          k.in <- 0.13
          t0.in <- -0.94
          L50.in <- 29
        }

        if (input$stock.choice == "C") {
          M.in <- 0.145
          Linf.in <- 53
          k.in <- 0.143
          t0.in <- -0.07
          L50.in <- 42
        }

        if (input$stock.choice == "D") {
          M.in <- 0.099
          Linf.in <- 57.38
          k.in <- 0.128
          t0.in <- -2.4
          L50.in <- 39.4
        }

        data.frame(
          Life_History_Parameter = c("M", "Linf", "k", "t0", "Lmat50%"),
          Value = c(M.in, Linf.in, k.in, t0.in, L50.in)
        )
      },
      striped = TRUE
    )

    summary_stats_tab <- reactive({
      CR.in_Ct <- CR_calc_Ct()
      CR.in_I <- CR_calc_Ind()
      CR.in_Lt <- CR_calc_Lt()
      data.frame(
        Statistic = c("Ct I", "Ct RP", "Control Rule", "CR value"),
        Catch_CR = c(
          input$Ct_I_in,
          input$Ct_RP_in,
          input$ct_equation_type,
          round(CR.in_Ct, 3)
        ),
        Index_CR = c(
          input$I_I_in,
          input$I_RP_in,
          input$ind_equation_type,
          round(CR.in_I, 3)
        ),
        Length_CR = c(
          input$Lt_I_in,
          input$Lt_RP_in,
          input$lt_equation_type,
          round(CR.in_Lt, 3)
        )
      )
    })

    output$summary_stats <- renderTable(
      {
        summary_stats_tab()
        # CR.in_Ct <- CR_calc_Ct()
        # CR.in_I <- CR_calc_Ind()
        # CR.in_Lt <- CR_calc_Lt()
        # data.frame(
        #   Statistic = c("Ct I", "Ct RP", "Control Rule", "CR value"),
        #   Catch_CR = c(
        #     input$Ct_I_in,
        #     input$Ct_RP_in,
        #     input$ct_equation_type,
        #     round(CR.in_Ct, 3)
        #   ),
        #   Index_CR = c(
        #     input$I_I_in,
        #     input$I_RP_in,
        #     input$ind_equation_type,
        #     round(CR.in_I, 3)
        #   ),
        #   Length_CR = c(
        #     input$Lt_I_in,
        #     input$Lt_RP_in,
        #     input$lt_equation_type,
        #     round(CR.in_Lt, 3)
        #   )
        #   # Length_CR = c(
        #   #   paste0("x"),
        #   #   paste0("x"),
        #   #   paste0("x"),
        #   #   paste0("x")
        #   # )
        # )
      },
      striped = TRUE
    )

    observeEvent(input$copy_btn, {
      # Convert dataframe to tab-separated text
      table_text <- paste(
        paste(names(summary_stats_tab()), collapse = "\t"),
        paste(
          apply(summary_stats_tab(), 1, function(row) {
            paste(row, collapse = "\t")
          }),
          collapse = "\n"
        ),
        sep = "\n"
      )

      # Use JavaScript to copy to clipboard
      session$sendCustomMessage("copy_to_clipboard", table_text)
    })
  })

  ######################################
  # Reference Points and Control Rules #
  ######################################
  nav_hide("navbar", "refpts")

  observeEvent(input$goto_refpts, {
    nav_show("navbar", "refpts")
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
      stock_ratio <- round(seq(0, 1, by = 0.01), 2)
      data.frame(
        stock_ratio = stock_ratio,
        catch = stock_ratio * input$E_msy
      )

      # Create linear models

      # Calculate catch based on control rule type
      #   catch_values <- sapply(stock_ratio, function(b) {
      #
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
      data$thresh <- data$constant <- NA
      thresh.coefs <- coef(lm(
        c(0, data[data$stock_ratio == input$b_target, ]$catch) ~ c(
          input$b_nocatch,
          input$b_target
        )
      ))
      data$thresh[
        data$stock_ratio >= input$b_nocatch & data$stock_ratio <= input$b_target
      ] <- thresh.coefs[2] *
        data$stock_ratio[
          data$stock_ratio >= input$b_nocatch &
            data$stock_ratio <= input$b_target
        ] +
        thresh.coefs[1]
      data$constant[data$stock_ratio >= input$b_target] <- thresh.coefs[2] *
        data$stock_ratio[data$stock_ratio == input$b_target] +
        thresh.coefs[1]

      p <- ggplot(data, aes(x = stock_ratio, y = catch)) +
        geom_line(color = "blue", size = 2) +
        geom_line(
          aes(x = stock_ratio, y = catch * input$buffer),
          color = "#390878",
          size = 2,
          linetype = "dotted"
        ) +
        geom_line(
          aes(x = stock_ratio, y = thresh),
          color = "orange",
          size = 2
        ) +
        geom_line(
          aes(x = stock_ratio, y = thresh * input$buffer),
          color = "#390878",
          size = 2,
          linetype = "dotted"
        ) +
        geom_line(
          aes(x = stock_ratio, y = constant),
          color = "#005595",
          size = 2
        ) +
        geom_line(
          aes(x = stock_ratio, y = constant * input$buffer),
          color = "#390878",
          size = 2,
          linetype = "dotted"
        ) +
        geom_point(
          aes(
            data[data$stock_ratio == input$current_stock, 1],
            data[data$stock_ratio == input$current_stock, 2]
          ),
          size = 5,
          color = "black",
          fill = "white"
        ) +
        #geom_abline(intercept = thresh.coefs[1],slope = thresh.coefs[2],
        #           color = "orange", linetype = "dashed", size = 1) +
        geom_vline(
          xintercept = input$b_limit,
          color = "red",
          linetype = "dashed",
          size = 1
        ) +
        geom_vline(
          xintercept = input$b_target,
          color = "#5D9741",
          linetype = "dashed",
          size = 1
        ) +
        #geom_vline(xintercept = input$current_stock,
        #           color = "orange", linetype = "solid", size = 1.5) +
        geom_hline(
          yintercept = 0,
          color = "black",
          linetype = "solid",
          alpha = 0.3
        ) +
        coord_cartesian(
          clip = "off",
          ylim = c(-0.025 * input$E_msy, input$E_msy)
        ) +
        xlim(0, 1) +

        # Add reference point labels
        annotate(
          "text",
          x = input$b_limit,
          y = 0.025 * input$E_msy,
          label = paste("Limit RP =", input$b_limit),
          color = "red",
          hjust = -0.1
        ) +
        annotate(
          "text",
          x = input$b_target,
          y = 0.025 * input$E_msy,
          label = paste("Target RP =", input$b_target),
          color = "#5D9741",
          hjust = -0.1
        ) +
        annotate(
          "text",
          x = data$stock_ratio[93],
          y = input$E_msy,
          label = paste("Constant fishing rate"),
          color = "blue",
          hjust = 0.1
        ) +
        annotate(
          "text",
          x = data$stock_ratio[97],
          y = max(data$constant, na.rm = TRUE),
          label = paste("Constant catch"),
          color = "#005595",
          vjust = -1.5
        ) +
        annotate(
          "text",
          x = input$b_nocatch,
          y = -0.025 * input$E_msy,
          label = paste("No catch =", input$b_nocatch),
          color = "black",
          hjust = -0.1
        ) +
        annotate(
          "text",
          x = input$current_stock,
          y = data[data$stock_ratio == input$current_stock, 2] * 1,
          label = "Current Stock",
          color = "black",
          hjust = 0.5,
          vjust = -2.5
        ) +

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
        annotate(
          "rect",
          xmin = 0,
          xmax = input$b_limit,
          ymin = 0,
          ymax = input$E_msy,
          alpha = 0.1,
          fill = "red"
        ) +
        annotate(
          "rect",
          xmin = input$b_limit,
          xmax = input$b_target,
          ymin = 0,
          ymax = input$E_msy,
          alpha = 0.1,
          fill = "yellow"
        ) +
        annotate(
          "rect",
          xmin = input$b_target,
          xmax = 1,
          ymin = 0,
          ymax = input$E_msy,
          alpha = 0.1,
          fill = "#5D9741"
        )

      print(p)
    })

    # Generate stock status summary
    output$stock_status <- renderText({
      current_catch <- control_rule_data() %>%
        filter(
          abs(stock_ratio - input$current_stock) ==
            min(abs(stock_ratio - input$current_stock))
        ) %>%
        pull(catch) %>%
        first()

      data <- control_rule_data()
      #Add threshhold option
      data$thresh <- data$constant <- NA
      thresh.coefs <- coef(lm(
        c(0, data[data$stock_ratio == input$b_target, ]$catch) ~ c(
          input$b_nocatch,
          input$b_target
        )
      ))
      data$thresh[
        data$stock_ratio >= input$b_nocatch & data$stock_ratio <= input$b_target
      ] <- thresh.coefs[2] *
        data$stock_ratio[
          data$stock_ratio >= input$b_nocatch &
            data$stock_ratio <= input$b_target
        ] +
        thresh.coefs[1]
      data$constant[data$stock_ratio >= input$b_target] <- thresh.coefs[2] *
        data$stock_ratio[data$stock_ratio == input$b_target] +
        thresh.coefs[1]

      curr_stock_catch <- data[data$stock_ratio == input$current_stock, ]

      paste0(
        #"Overfishing limit at current stock size: ", round(current_catch, 3), " (relative units)\n",
        "Overfishing limit (OFL) at current stock size: ",
        round(curr_stock_catch[2], 3),
        " (relative units)\n",
        "Threshhold control rule catch: ",
        round(curr_stock_catch[4], 3),
        " (relative units)\n",
        "Buffered catch (e.g., ABC): ",
        round(curr_stock_catch[4] * input$buffer, 3),
        " (relative units)\n",
        "Constant catch (catch at FMSY proxy at target biomass): ",
        round(curr_stock_catch[3] * input$buffer, 3),
        " (relative units)\n"
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
        "Current Stock Size (SB/SB₀): ",
        round(input$current_stock, 3),
        "\n",
        "Current Stock Status: ",
        status,
        "\n",
        "No Catch Point: ",
        input$b_nocatch,
        "\n",
        "Limit (Overfished) Reference Point: ",
        input$b_limit,
        "\n",
        "Target Reference Point: ",
        input$b_target,
        "\n\n",
        "Management Zones:\n",
        "• RED (0 - ",
        input$b_limit,
        "): Overfished - rebuilding plan.\n",
        "• YELLOW (",
        input$b_limit,
        " - ",
        input$b_target,
        "): Precautionary - fishing mortality < FMSY. Note that fishing mortality is no longer constant.\n",
        "• GREEN (",
        input$b_target,
        "+): Healthy - Full fishing allowed."
      )
    })
  })

  # observeEvent(input$goto_reports, {
  #   nav_select("navbar", "reports4")
  # })

  ###############################
  # Scale, Status, Productivity #
  ###############################
  nav_hide("navbar", "SSP")

  observeEvent(input$goto_ssp, {
    nav_show("navbar", "SSP")
    nav_select("navbar", "SSP")
    #Folder names
    ssp.folder.names <- c(
      "Status20%",
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
      "Status_est_Scale40%_M_hi"
    )
    ssp.folder.names.in.status <- ssp.folder.names[1:2]
    ssp.folder.names.in.scale <- ssp.folder.names[3:4]
    ssp.folder.names.in.prod <- ssp.folder.names[5:8]
    ssp.folder.names.in.status_prod <- ssp.folder.names[13:18]
    ssp.folder.names.in.scale_prod <- ssp.folder.names[9:12]

    #SSP choices for user and to be used in plots. These should read better.
    SSP_choices <- c(
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
      "Estimate status, Steepness high"
    )

    SSP_choices.in.status <- SSP_choices[1:2]
    SSP_choices.in.scale <- SSP_choices[3:4]
    SSP_choices.in.prod <- SSP_choices[5:8]
    SSP_choices.in.status_prod <- SSP_choices[13:18]
    SSP_choices.in.scale_prod <- SSP_choices[9:12]

    output$SSP_model_picks_status <- renderUI({
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

    output$SSP_model_picks_scale <- renderUI({
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

    output$SSP_model_picks_prod <- renderUI({
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

    output$SSP_model_picks_status_prod <- renderUI({
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

    output$SSP_model_picks_scale_prod <- renderUI({
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

    output$SSP_model_picks_grouped <- renderUI({
      pickerInput(
        inputId = "myPicker_SSP_grouped",
        label = "Organized by change in the model",
        choices = list(
          "Status" = SSP_choices.in.status,
          "Scale" = SSP_choices.in.scale,
          "Productivity" = SSP_choices.in.prod,
          "Status and productivity" = SSP_choices.in.status_prod,
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

    output$SSP_model_picks_groupedII <- renderUI({
      selectInput(
        "myPicker_SSP_grouped",
        "Organized by change in the model:",
        list(
          "Status" = SSP_choices.in.status,
          "Scale" = SSP_choices.in.scale,
          "Productivity" = SSP_choices.in.prod,
          "Status and productivity" = SSP_choices.in.status_prod,
          "Scale and productivity" = SSP_choices.in.scale_prod
        ),
        multiple = TRUE
      )
    })

    #load model summaries
    observeEvent(req(input$run_SSP_comps), {
      print(c(
        SSP_choices.in.status,
        SSP_choices.in.scale,
        SSP_choices.in.prod,
        SSP_choices.in.status_prod,
        SSP_choices.in.scale_prod
      ))
      #      load(paste0(getwd(),"/mod_summary.RDS"))
      #      load(paste0(getwd(),"/Catches.RDS"))
      load("mod_summary.rds")
      load("Catches.rds")

      colnames(ssp_summary$SpawnBio)[
        1:(length(colnames(ssp_summary$SpawnBio)) - 2)
      ] <- colnames(ssp_summary$Bratio)[
        1:(length(colnames(ssp_summary$Bratio)) - 2)
      ] <- c("Status 40%", SSP_choices)
      mod_indices <- c(2:ssp_summary$n)[
        SSP_choices %in% input$myPicker_SSP_grouped
      ]
      SpawnOutput <- melt(
        id.vars = c("Yr"),
        ssp_summary$SpawnBio[
          -nrow(ssp_summary$SpawnBio),
          c(1, mod_indices, ncol(ssp_summary$SpawnBio))
        ],
        value.name = "Scale"
      )
      Bratio <- melt(
        id.vars = c("Yr"),
        ssp_summary$Bratio[
          -nrow(ssp_summary$Bratio),
          c(1, mod_indices, ncol(ssp_summary$Bratio))
        ],
        value.name = "Status"
      )
      #Catches<-Catches[Catches$Model%in%c("Status40%",ssp.folder.names[SSP_choices%in%input$myPicker_SSP]),]
      Catches <- as.data.frame(Catches)
      Catches <- Catches[
        Catches$Model %in% c("Status 40%", input$myPicker_SSP_grouped),
      ]
      #Catches$Model<-c("Status 40%",SSP_choices[SSP_choices%in%input$myPicker_SSP_grouped])
      #try(SSplotComparisons(ssp_summary, subplots=c(1,3),legendlabels = c("Status40%",input$myPicker_SSP),endyrvec=2020, ylimAdj = 1.30, new = FALSE,plot=FALSE,print=TRUE, legendloc = 'topleft',uncertainty=TRUE,plotdir=paste0(Dir_SSP,"/Comparisons"),btarg=0.4,minbthresh=0.25))

      #Pull future catch values
      OFL <- ssp_summary$quants[
        ssp_summary$quants$Label == "OFLCatch_2021",
        c(-(ncol(ssp_summary$quants) - 1), -ncol(ssp_summary$quants))
      ]
      colnames(OFL) <- c("Status 40%", SSP_choices)
      OFL <- OFL[c(1, mod_indices)]
      OFL.rel <- (OFL - as.numeric(OFL[1])) / as.numeric(OFL[1])
      Forecatch <- ssp_summary$quants[
        ssp_summary$quants$Label == "ForeCatch_2021",
        c(-(ncol(ssp_summary$quants) - 1), -ncol(ssp_summary$quants))
      ]
      colnames(Forecatch) <- c("Status 40%", SSP_choices)
      Forecatch <- Forecatch[c(1, mod_indices)]
      Forecatch.rel <- (Forecatch - as.numeric(Forecatch[1])) /
        as.numeric(Forecatch[1])
      MSY <- ssp_summary$quants[
        ssp_summary$quants$Label == "Dead_Catch_MSY",
        c(-(ncol(ssp_summary$quants) - 1), -ncol(ssp_summary$quants))
      ]
      colnames(MSY) <- c("Status 40%", SSP_choices)
      MSY <- MSY[c(1, mod_indices)]
      MSY.rel <- (MSY - as.numeric(MSY[1])) / as.numeric(MSY[1])
      Proj.rel <- rbind(MSY.rel, OFL.rel, Forecatch.rel)
      Proj.rel$metric <- c("MSY", "OFL", "ABC")
      Proj.rel <- Proj.rel[, -1]
      Proj.rel.gg <- melt(Proj.rel)
      Proj.rel.gg$metric <- factor(
        Proj.rel.gg$metric,
        levels = c("MSY", "OFL", "ABC")
      )

      #Create plots
      output$Catches <- renderPlotly({
        p_catch <- ggplot(Catches, aes(Yr, dead_bio, color = Model)) +
          geom_line(lwd = 1.1) +
          xlab("Year") +
          ylab("Remvoals (in biomass)") +
          ylim(0, NA) +
          scale_color_viridis_d() +
          theme_bw() +
          labs(color = "Models") +
          ggtitle("Removals (Scale measure)") +
          theme(plot.title = element_text(size = 40, face = "bold"))

        ggplotly(p_catch)
      })

      output$Scale <- renderPlotly({
        p_scale <- ggplot(SpawnOutput, aes(Yr, Scale, color = variable)) +
          geom_line(lwd = 1.1) +
          xlab("Year") +
          ylab("Scale (Spawning Output)") +
          ylim(0, NA) +
          scale_color_viridis_d() +
          theme_bw() +
          labs(color = "Models") +
          ggtitle("Scale") +
          theme(plot.title = element_text(size = 40, face = "bold"))

        ggplotly(p_scale)
      })

      output$Status <- renderPlotly({
        p_status <- ggplot(Bratio, aes(Yr, Status, color = variable)) +
          geom_line(lwd = 1.1) +
          xlab("Year") +
          ylab("Status (Size relative to unfished)") +
          ylim(0, NA) +
          scale_color_viridis_d() +
          theme_bw() +
          labs(color = "Models") +
          ggtitle("Status") +
          theme(plot.title = element_text(size = 40, face = "bold"))

        ggplotly(p_status)
      })

      output$Proj <- renderPlotly({
        p_proj <- ggplot(
          Proj.rel.gg,
          aes(variable, value * 100, color = metric)
        ) +
          geom_point(aes(shape = metric), size = 4) +
          xlab("Model") +
          ylab("% change relative to the 40% stock status model") +
          geom_hline(yintercept = 0) +
          scale_color_viridis_d() +
          theme_bw() +
          labs(color = "Catch metric", shape = "") +
          ggtitle("Projected catch") +
          theme(plot.title = element_text(size = 40, face = "bold")) +
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

  ####################
  # Baseline Shifter #
  ####################
  nav_hide("navbar", "baseline")

  observeEvent(input$goto_baseline, {
    nav_show("navbar", "baseline")
    nav_select("navbar", "baseline")

    #spp.out<-SS_output(paste0(getwd(),"/Spp_Reports/REBS_2025"))
    volumes <- getVolumes()()

    pathReport <- reactive({
      shinyDirChoose(
        input,
        "Report_dir",
        roots = volumes,
        session = session,
        filetypes = c('', 'txt')
      )
      return(parseDirPath(volumes, input$Report_dir))
    })

    model.out <- reactiveValues(report = NULL)
    # if(!is.null(input$Report_dir))
    #  {
    #

    observeEvent(input$Report_dir, {
      output$ReportPath <- renderText({
        paste0("Selected model folder:\n", pathReport())
      })
    })

    #spp.out<-SS_output(paste0(getwd(),"/Spp_Reports/REBS_2025"))

    observeEvent(req(pathReport()), {
      show_modal_spinner(
        spin = "flower",
        color = wes_palettes$AsteroidCity1[1],
        text = "Reading in model output"
      )
      model.out$report <- SS_output(pathReport())
      remove_modal_spinner()
    })

    observeEvent(input$run_baseline_comps, {
      spp.out <- model.out$report
      Spp.dervout <- data.frame(
        Year = spp.out$timeseries$Yr,
        TotalB = spp.out$timeseries$Bio_all,
        SummaryB = spp.out$timeseries$Bio_smry,
        SpawnOut <- spp.out$timeseries$SpawnBio,
        Dep <- spp.out$timeseries$SpawnBio / spp.out$timeseries$SpawnBio[1]
      )
      Spp.dervout.RE <- data.frame(
        Year = spp.out$timeseries$Yr,
        TotalB.RE = (spp.out$timeseries$Bio_all -
          spp.out$timeseries$Bio_all[1]) /
          spp.out$timeseries$Bio_all[1],
        SummaryB = (spp.out$timeseries$Bio_smry -
          spp.out$timeseries$Bio_smry[1]) /
          spp.out$timeseries$Bio_smry[1],
        SpawnOut.RE <- (spp.out$timeseries$SpawnBio -
          spp.out$timeseries$SpawnBio[1]) /
          spp.out$timeseries$SpawnBio[1]
      )

      if (!any(spp.out$timeseries$Yr == input$Year_comp)) {
        Spp.dervout.gg <- rbind(
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = spp.out$timeseries$Bio_all / spp.out$timeseries$Bio_all[1],
            Metric = "Total Biomass"
          ),
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = spp.out$timeseries$Bio_smry /
              spp.out$timeseries$Bio_smry[1],
            Metric = "Summary Biomass"
          ),
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = spp.out$timeseries$SpawnBio /
              spp.out$timeseries$SpawnBio[1],
            Metric = "Spawning Output"
          )
        )

        Spp.dervout.RE.gg <- rbind(
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = (spp.out$timeseries$Bio_all -
              spp.out$timeseries$Bio_all[1]) /
              spp.out$timeseries$Bio_all[1],
            Metric = "Total Biomass"
          ),
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = (spp.out$timeseries$Bio_smry -
              spp.out$timeseries$Bio_smry[1]) /
              spp.out$timeseries$Bio_smry[1],
            Metric = "Summary Biomass"
          ),
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = (spp.out$timeseries$SpawnBio -
              spp.out$timeseries$SpawnBio[1]) /
              spp.out$timeseries$SpawnBio[1],
            Metric = "Spawning Output"
          )
        )
      }

      if (any(spp.out$timeseries$Yr == input$Year_comp)) {
        Spp.dervout.gg <- rbind(
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = spp.out$timeseries$Bio_all /
              spp.out$timeseries$Bio_all[
                spp.out$timeseries$Yr == input$Year_comp
              ],
            Metric = "Total Biomass"
          ),
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = spp.out$timeseries$Bio_smry /
              spp.out$timeseries$Bio_smry[
                spp.out$timeseries$Yr == input$Year_comp
              ],
            Metric = "Summary Biomass"
          ),
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = spp.out$timeseries$SpawnBio /
              spp.out$timeseries$SpawnBio[
                spp.out$timeseries$Yr == input$Year_comp
              ],
            Metric = "Spawning Output"
          )
        )

        Spp.dervout.RE.gg <- rbind(
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = (spp.out$timeseries$Bio_all -
              spp.out$timeseries$Bio_all[
                spp.out$timeseries$Yr == input$Year_comp
              ]) /
              spp.out$timeseries$Bio_all[
                spp.out$timeseries$Yr == input$Year_comp
              ],
            Metric = "Total Biomass"
          ),
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = (spp.out$timeseries$Bio_smry -
              spp.out$timeseries$Bio_smry[
                spp.out$timeseries$Yr == input$Year_comp
              ]) /
              spp.out$timeseries$Bio_smry[
                spp.out$timeseries$Yr == input$Year_comp
              ],
            Metric = "Summary Biomass"
          ),
          data.frame(
            Year = spp.out$timeseries$Yr,
            Value = (spp.out$timeseries$SpawnBio -
              spp.out$timeseries$SpawnBio[
                spp.out$timeseries$Yr == input$Year_comp
              ]) /
              spp.out$timeseries$SpawnBio[
                spp.out$timeseries$Yr == input$Year_comp
              ],
            Metric = "Spawning Output"
          )
        )
      }

      output$CompPlot <- renderPlotly({
        comp1 <- ggplot(Spp.dervout.gg, aes(Year, Value, col = Metric)) +
          geom_line(lwd = 1.25) +
          ylab("Value relative to chosen year") +
          ylim(0, NA) +
          geom_hline(yintercept = 1, col = "orange", linetype = "dashed") +
          geom_vline(
            xintercept = input$Year_comp,
            col = "orange",
            linetype = "dashed"
          ) +
          theme_bw() #+
        #annotate("rect",xmin=-Inf, xmax=input$Year_comp, ymin=-Inf, ymax=Inf, alpha=0.75, fill='darkgray')

        comp1 <- ggplotly(comp1)

        comp1[['x']][['layout']][['shapes']] <- c()
        comp1 <- layout(
          comp1,
          shapes = list(
            list(
              type = "rect",
              fillcolor = "gray",
              line = list(color = "gray"),
              opacity = 0.25,
              x0 = 0,
              x1 = input$Year_comp,
              y0 = 0,
              y1 = 1000
            )
          )
        )
        comp1
      })

      output$CompPlotRE <- renderPlotly({
        comp2 <- ggplot(
          Spp.dervout.RE.gg,
          aes(Year, Value * 100, col = Metric)
        ) +
          geom_segment(aes(x = Year, xend = Year, y = 0, yend = Value)) +
          geom_point(size = 4) +
          ylab("Percent difference from chosen year") +
          geom_hline(yintercept = 0, col = "orange", linetype = "dashed") +
          geom_vline(
            xintercept = input$Year_comp,
            col = "orange",
            linetype = "dashed"
          ) +
          theme_bw() #+
        #annotate("rect",xmin=-Inf, xmax=input$Year_comp, ymin=-Inf, ymax=Inf, alpha=0.5, fill='gray')

        comp2 <- ggplotly(comp2)

        comp2[['x']][['layout']][['shapes']] <- c()
        comp2 <- layout(
          comp2,
          shapes = list(
            list(
              type = "rect",
              fillcolor = "gray",
              line = list(color = "gray"),
              opacity = 0.25,
              x0 = 0,
              x1 = input$Year_comp,
              y0 = -1000,
              y1 = 1000
            )
          )
        )
        comp2
      })

      output$DepPlot <- renderPlotly({
        p1 <- ggplot(Spp.dervout, aes(Year, Dep)) +
          geom_line(lwd = 1.25) +
          ylab("Relative Stock Status") +
          ylim(0, NA) +
          theme_bw()
        ggplotly(p1)
      })

      output$SpawnOutPlot <- renderPlotly({
        p2 <- ggplot(Spp.dervout, aes(Year, SpawnOut)) +
          geom_line(lwd = 1.25) +
          ylab("Spawning Output") +
          ylim(0, NA) +
          theme_bw()
        ggplotly(p2)
      })

      output$SummaryBPlot <- renderPlotly({
        p3 <- ggplot(Spp.dervout, aes(Year, SummaryB)) +
          geom_line(lwd = 1.25) +
          ylab("Summary Biomass") +
          ylim(0, NA) +
          theme_bw()
        ggplotly(p3)
      })

      output$TotalBPlot <- renderPlotly({
        p4 <- ggplot(Spp.dervout, aes(Year, TotalB)) +
          geom_line(lwd = 1.25) +
          ylab("Total Biomass") +
          ylim(0, NA) +
          theme_bw()
        ggplotly(p4)
      })
    })
  })

  #outputOptions(output, "CompPlot", suspendWhenHidden = FALSE)

  #####################
  # SAC decision tree #
  #####################
  nav_hide("navbar", "sac")

  observeEvent(input$goto_SAC, {
    nav_show("navbar", "sac")
    nav_select("navbar", "sac")

    decision_tree <- list(
      "root" = list(
        question = "Do you have a removal time series AND/OR an measure of absolute abundance?",
        choices = list(
          "YES" = "scale_node",
          "NO " = "status_node"
        )
      ),

      ##############
      #Scale models#
      ##############
      "scale_node" = list(
        question = "Do you have biological (lengths or ages) compositions?",
        choices = list(
          "YES: biological data are available" = "scale_bio_node",
          "NO: biological data are NOT available" = "scale_nobio_node"
        ),
        path = c("Catch: YES")
      ),

      "scale_bio_node" = list(
        question = "Do you have a relative index of abundance?",
        choices = list(
          "YES: at least one abundance index is available" = "scale_bio_index_node",
          "NO: no abudance indices are available" = "scale_bio_noindex_node"
        ),
        path = c("Catch: YES", "Bio comps: YES")
      ),

      "scale_bio_index_node" = list(
        outcome = "You have the all the data types to do a traditional FULLY INTEGRATED stock assessment model.",
        path = c("Catch: YES", "Bio comps: YES", "Index of abundance: YES")
      ),

      "scale_bio_noindex_node" = list(
        outcome = "You have the the data types to do a CATCH + LENGTH and/or AGE model.",
        path = c("Catch: YES", "Bio comps: YES", "Index of abundance: NO")
      ),

      "scale_nobio_node" = list(
        question = "Do you have a relative index of abundance?",
        choices = list(
          "YES: at least one abundance index is available" = "scale_nobio_index_node",
          "NO: no abudance indices are available" = "scale_nobio_noindex_node"
        ),
        path = c("Catch: YES", "Bio comps: NO")
      ),

      "scale_nobio_index_node" = list(
        outcome = "You have the the data types to do a SURPLUS PRODUCTION model. This can be age-based if using an age-structured model.",
        path = c("Catch: YES", "Bio comps: NO", "Index of abundance: YES")
      ),

      "scale_nobio_noindex_node" = list(
        outcome = "You have the data to do a CATCH ONLY model.",
        path = c("Catch: YES", "Bio comps: NO", "Index of abundance: NO")
      ),

      ###############
      #Status models#
      ###############
      "status_node" = list(
        question = "Do you have biological (lengths or ages) compositions?",
        choices = list(
          "YES: biological data are available" = "status_bio_node",
          "NO: biological data are NOT available" = "status_nobio_node"
        ),
        path = c("Catch: NO")
      ),

      "status_bio_node" = list(
        question = "Do you have a relative index of abundance?",
        choices = list(
          "YES: at least one abundance index is available" = "status_bio_index_node",
          "NO: no abudance indices are available" = "status_bio_noindex_node"
        ),
        path = c("Catch: NO", "Bio comps: YES")
      ),

      "status_bio_index_node" = list(
        outcome = "You have the data types to do a MULTI-INDICATOR approach.",
        path = c("Catch: NO", "Bio comps: YES", "Index of abundance: YES")
      ),

      "status_bio_noindex_node" = list(
        outcome = "You have the the data types to do a LENGTH and/or AGE ONLY model.",
        path = c("Catch: NO", "Bio comps: YES", "Index of abundance: NO")
      ),

      "status_nobio_node" = list(
        question = "Do you have a relative index of abundance?",
        choices = list(
          "YES: at least one abundance index is available" = "status_nobio_index_node",
          "NO: at least one abundance index is available" = "status_nobio_noindex_node"
        ),
        path = c("Catch: NO", "Bio comps: NO")
      ),

      "status_nobio_index_node" = list(
        outcome = "You have the the data types to do an ABUNDANCE INDICATOR approach.",
        path = c("Catch: NO", "Bio comps: NO", "Index of abundance: YES")
      ),

      "status_nobio_noindex_node" = list(
        outcome = "Lacking the big three data types, a RISK ANALYSIS seems a good option.",
        path = c("Catch: NO", "Bio comps: NO", "Index of abundance: NO")
      )
    )

    current_node <- reactiveVal("welcome")
    node_history <- reactiveVal(character(0))

    # Initialize useShinyjs
    useShinyjs()

    output$SACImage_init <- renderImage(
      {
        list(
          src = file.path(getwd(), "SAC_pics/SAC.jpg"),
          alt = "SAC decision tree",
          width = "50%",
          height = "100%",
          style = "display: block; margin-left: auto; margin-right: auto;"
        )
      },
      deleteFile = FALSE
    )

    output$SACImage <- renderImage(
      {
        list.out <- list(
          src = file.path(getwd(), "SAC_pics/SAC.jpg"),
          alt = "SAC decision tree",
          width = "50%",
          height = "100%",
          style = "display: block; margin-left: auto; margin-right: auto;"
        )

        if (current_node() == "root") {
          list.out$src = src = file.path(getwd(), "SAC_pics/SAC.jpg")
        }
        if (current_node() == "scale_node") {
          list.out$src = file.path(getwd(), "SAC_pics/Scale_bio.jpg")
        }
        if (current_node() == "scale_bio_node") {
          list.out$src = file.path(getwd(), "SAC_pics/Scale_bioYES_Index.jpg")
        }
        if (current_node() == "scale_nobio_node") {
          list.out$src = file.path(getwd(), "SAC_pics/Scale_bioNO_Index.jpg")
        }
        if (current_node() == "status_node") {
          list.out$src = file.path(getwd(), "SAC_pics/Status_bio.jpg")
        }
        if (current_node() == "status_bio_node") {
          list.out$src = file.path(getwd(), "SAC_pics/Status_bioYES_Index.jpg")
        }
        if (current_node() == "status_nobio_node") {
          list.out$src = file.path(getwd(), "SAC_pics/Status_bioNO_Index.jpg")
        }

        list.out
      },
      deleteFile = FALSE
    )

    output$SACImage_out <- renderImage(
      {
        list.end <- list(
          src = file.path(getwd(), "SAC_pics/SAC.jpg"),
          alt = "SAC decision tree out",
          width = "50%",
          height = "100%",
          style = "display: block; margin-left: auto; margin-right: auto;"
        )

        if (current_node() == "scale_bio_index_node") {
          list.end$src = file.path(
            getwd(),
            "SAC_pics/Scale_bioYES_IndexYES_IA.jpg"
          )
        }
        if (current_node() == "scale_bio_noindex_node") {
          list.end$src = file.path(
            getwd(),
            "SAC_pics/Scale_bioYES_IndexNO_CL.jpg"
          )
        }
        if (current_node() == "scale_nobio_index_node") {
          list.end$src = file.path(
            getwd(),
            "SAC_pics/Scale_bioNO_IndexYES_SP.jpg"
          )
        }
        if (current_node() == "scale_nobio_noindex_node") {
          list.end$src = file.path(
            getwd(),
            "SAC_pics/Scale_bioNO_IndexNO_CtO.jpg"
          )
        }
        if (current_node() == "status_bio_index_node") {
          list.end$src = file.path(
            getwd(),
            "SAC_pics/Status_bioYES_IndexYES_MInd.jpg"
          )
        }
        if (current_node() == "status_bio_noindex_node") {
          list.end$src = file.path(
            getwd(),
            "SAC_pics/Status_bioYES_IndexNO_LAO.jpg"
          )
        }
        if (current_node() == "status_nobio_index_node") {
          list.end$src = file.path(
            getwd(),
            "SAC_pics/Status_bioNO_IndexYES_Indicator.jpg"
          )
        }
        if (current_node() == "status_nobio_noindex_node") {
          list.end$src = file.path(
            getwd(),
            "SAC_pics/Status_bioNO_IndexNO_RA.jpg"
          )
        }

        list.end
      },
      deleteFile = FALSE
    )

    # Track if we're at root
    output$at_root <- reactive({
      current_node() == "welcome"
    })
    outputOptions(output, "at_root", suspendWhenHidden = FALSE)

    # Track if we're at an outcome
    output$at_outcome <- reactive({
      node <- current_node()
      node != "welcome" && !is.null(decision_tree[[node]]$outcome)
    })
    outputOptions(output, "at_outcome", suspendWhenHidden = FALSE)

    # Show path when not at welcome
    output$show_path <- reactive({
      current_node() != "welcome"
    })
    outputOptions(output, "show_path", suspendWhenHidden = FALSE)

    # Display current question
    output$question_text <- renderText({
      node <- current_node()
      if (node != "welcome" && !is.null(decision_tree[[node]]$question)) {
        decision_tree[[node]]$question
      } else {
        ""
      }
    })

    # Display outcome text
    output$outcome_text <- renderText({
      node <- current_node()
      if (!is.null(decision_tree[[node]]$outcome)) {
        decision_tree[[node]]$outcome
      } else {
        ""
      }
    })

    # Generate choice buttons
    output$choices_ui <- renderUI({
      node <- current_node()
      if (node != "welcome" && !is.null(decision_tree[[node]]$choices)) {
        choices <- decision_tree[[node]]$choices
        choice_buttons <- lapply(names(choices), function(choice_name) {
          actionButton(
            inputId = paste0("choice_", gsub("[^A-Za-z0-9]", "_", choice_name)),
            label = choice_name,
            width = "25%",
            class = "text-center",
            class = "btn-outline-info choice-button",
            onclick = paste0(
              "Shiny.setInputValue('selected_choice', '",
              choice_name,
              "');"
            )
          )
        })
        do.call(div, choice_buttons)
      }
    })

    # Display path taken
    output$path_display <- renderText({
      node <- current_node()
      if (node != "welcome" && !is.null(decision_tree[[node]]$path)) {
        path_items <- decision_tree[[node]]$path
        path_html <- paste(
          sapply(path_items, function(item) {
            paste0('<span class="path-item">', item, '</span>')
          }),
          collapse = ' → '
        )
        path_html
      } else {
        ""
      }
    })

    # Start the decision tree
    observeEvent(input$start, {
      current_node("root")
      node_history(character(0))
    })

    # Handle choice selection
    observeEvent(input$selected_choice, {
      current <- current_node()
      if (!is.null(decision_tree[[current]]$choices)) {
        choice_name <- input$selected_choice
        next_node <- decision_tree[[current]]$choices[[choice_name]]

        # Update history and move to next node
        node_history(c(node_history(), current))
        current_node(next_node)
        # print(current_node)
        # print(node_history)
      }
    })

    # Go back functionality
    observeEvent(input$go_back, {
      history <- node_history()
      if (length(history) > 0) {
        # Go back to previous node
        current_node(history[length(history)])
        node_history(history[-length(history)])
      } else {
        # Go back to welcome if no history
        current_node("welcome")
      }
    })

    # Go back from outcome
    observeEvent(input$go_back_outcome, {
      history <- node_history()
      if (length(history) > 0) {
        current_node(history[length(history)])
        node_history(history[-length(history)])
      } else {
        current_node("welcome")
      }
    })

    # Reset to beginning
    observeEvent(c(input$reset, input$start_over), {
      current_node("welcome")
      node_history(character(0))
    })
  })
  ################

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
