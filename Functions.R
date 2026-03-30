library(twosamples)
library(shiny)

# Define the von Bertalanffy growth function
von_bertalanffy <- function(age, Linf, K, t0 = 0) {
  Linf * (1 - exp(-K * (age - t0)))
}

# Define the selectivity calculation function
calc_selectivity <- function(lengths, L50_asc, L95_asc, peak_length, desc_sd) {
  selectivity <- numeric(length(lengths))

  for (i in 1:length(lengths)) {
    L <- lengths[i]

    # Ascending limb (logistic)
    sel_asc <- 1 / (1 + exp(-log(19) * ((L - L50_asc) / (L95_asc - L50_asc))))

    # Descending limb (normal/Gaussian curve)
    sel_desc <- exp(-0.5 * ((L - peak_length) / desc_sd)^2)

    # Combined selectivity
    if (L <= peak_length) {
      selectivity[i] <- sel_asc
    } else {
      selectivity[i] <- sel_desc
    }
  }

  return(selectivity)
}

# Define the population calculation function
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

  Pop_structure <- data.frame(
    Age = ages,
    Length = lengths,
    Numbers = numbers,
    Selectivity = selectivity,
    Mortality = Z
  )

  return(Pop_structure)
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
