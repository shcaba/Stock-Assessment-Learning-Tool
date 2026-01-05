generate_stochastic_recruitment <- function(years, mean_rec, cv_rec, autocorr = 0.3) {
  n_years <- length(years)
  rec_devs <- numeric(n_years)
  rec_devs[1] <- rnorm(1, 0, sqrt(log(1 + cv_rec^2)))
  
  for (i in 2:n_years) {
    rec_devs[i] <- autocorr * rec_devs[i-1] + 
      sqrt(1 - autocorr^2) * rnorm(1, 0, sqrt(log(1 + cv_rec^2)))
  }
  
  recruitment <- mean_rec * exp(rec_devs - 0.5 * log(1 + cv_rec^2))
  return(recruitment)
}

age_structured_model <- function(years, ages, recruitment, M, F_mult, 
                                 selectivity, maturity, weight_at_age) {
  n_years <- length(years)
  n_ages <- length(ages)
  
  # Initialize population matrix (years x ages)
  N <- matrix(0, nrow = n_years, ncol = n_ages)
  
  # Initialize first year with equilibrium age structure
  N[1, 1] <- recruitment[1]
  for (a in 2:n_ages) {
    if (a < n_ages) {
      N[1, a] <- N[1, a-1] * exp(-M)
    } else {
      # Plus group
      N[1, a] <- N[1, a-1] * exp(-M) / (1 - exp(-M))
    }
  }
  
  # Project population forward
  for (y in 2:n_years) {
    # Recruitment
    N[y, 1] <- recruitment[y]
    
    # Aging and mortality
    for (a in 2:n_ages) {
      if (a < n_ages) {
        total_mort <- M + F_mult * selectivity[a-1]
        N[y, a] <- N[y-1, a-1] * exp(-total_mort)
      } else {
        # Plus group
        total_mort_prev <- M + F_mult * selectivity[a-1]
        total_mort_curr <- M + F_mult * selectivity[a]
        N[y, a] <- (N[y-1, a-1] * exp(-total_mort_prev) + 
                      N[y-1, a] * exp(-total_mort_curr))
      }
    }
  }
  