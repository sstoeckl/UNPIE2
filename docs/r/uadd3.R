# add3: Maximum periodic spending possible to exceed a specified
#       minimum years to ruin with probability p (real terms)
#
# wealth0    : real wealth at retirement (t=0)
# min_years  : minimum years to ruin that we want to EXCEED/REACH
# prob_ok    : target probability P(time_to_ruin >= min_years)
# mu_real    : expected REAL return per year (e.g. 0.03)
# vol_real   : volatility of REAL return per year (e.g. 0.08)
# nper       : total years to simulate (default 25, like the app)
# n_scen     : number of Monte Carlo scenarios
# n_show     : number of sample paths to return
# seed       : RNG seed (NULL => random)
#
# Model (end-of-period spending, real):
#   gross_t = exp( m + vol_real * Z_t ), with m = log(1 + mu_real) - 0.5*vol_real^2
#   W_t     = pmax(0, W_{t-1} * gross_t - spend), W_0 = wealth0
#
# For a candidate spend, success = 1{ time_to_ruin >= min_years OR no ruin in [1..nper] }.

uadd3 <- function(wealth0, min_years, prob_ok, mu_real, vol_real,
                  nper = 25L, n_scen = 10000L, n_show = 10L, seed = NULL) {
  stopifnot(is.numeric(wealth0), is.numeric(min_years), is.numeric(prob_ok),
            is.numeric(mu_real), is.numeric(vol_real))
  min_years <- as.integer(min_years)
  if (!is.null(seed)) set.seed(as.integer(seed))
  
  # Return a function that simulates paths and returns:
  #   list(prob=..., paths=matrix, ruin_years=integer vector)
  simulate_with_spend <- function(spend, need_paths = FALSE) {
    m <- log1p(mu_real) - 0.5 * vol_real^2
    Z <- matrix(rnorm(n_scen * nper), nrow = n_scen, ncol = nper)
    gross <- exp(m + vol_real * Z)
    
    W <- matrix(0.0, nrow = n_scen, ncol = nper)
    # year 1
    W[, 1] <- pmax(0, wealth0 * gross[, 1] - spend)
    # years 2..n
    if (nper >= 2L) {
      for (t in 2:nper) {
        W[, t] <- pmax(0, W[, t - 1] * gross[, t] - spend)
      }
    }
    
    ruin_year <- apply(W, 1L, function(x) {
      k <- which(x <= 0)
      if (length(k)) k[1] else NA_integer_
    })
    
    ok <- is.na(ruin_year) | ruin_year >= min_years
    pr <- mean(ok)
    
    if (!need_paths) {
      return(list(prob = pr))
    } else {
      show_idx <- seq_len(min(n_show, n_scen))
      W_show <- W[show_idx, , drop = FALSE]
      ruin_show <- ruin_year[show_idx]
      years <- sort(unique(ruin_show[!is.na(ruin_show)]))
      counts <- if (length(years)) sapply(years, function(y) sum(ruin_show <= y, na.rm = TRUE)) else integer(0)
      return(list(prob = pr, paths = W_show, ruin_show = ruin_show,
                  table_years = years, table_cum = counts))
    }
  }
  
  # Find maximal spend with P(time_to_ruin >= min_years) >= prob_ok
  # Bracket via doubling until prob drops below target.
  low <- 0
  high <- max(100, wealth0)  # initial guess
  # ensure high is "too high" (prob < prob_ok)
  repeat {
    pr <- simulate_with_spend(high)$prob
    if (pr < prob_ok || high > 1e7) break
    high <- high * 2
  }
  
  # If even zero spending cannot reach prob_ok (shouldn't happen), fallback.
  if (simulate_with_spend(low)$prob < prob_ok && simulate_with_spend(high)$prob < prob_ok) {
    spend_star <- 0
  } else {
    # Binary search
    for (iter in 1:40) {
      mid <- 0.5 * (low + high)
      pr  <- simulate_with_spend(mid)$prob
      if (pr >= prob_ok) {
        low <- mid  # feasible
      } else {
        high <- mid # infeasible
      }
    }
    spend_star <- low
  }
  
  # Final simulation with optimal spend to collect paths & KPIs
  sim <- simulate_with_spend(spend_star, need_paths = TRUE)
  
  list(
    ok = TRUE,
    inputs = list(
      wealth0 = wealth0, min_years = min_years, prob_ok = prob_ok,
      mu_real = mu_real, vol_real = vol_real, nper = nper,
      n_scen = n_scen, n_show = n_show
    ),
    results = list(
      t = 1:nper,
      spend_opt = spend_star,
      paths = sim$paths,                       # matrix n_show × nper
      ruin_years_show = sim$ruin_show,         # length n_show
      table_years = sim$table_years,           # KPI header row
      table_cum_ruins = sim$table_cum          # KPI second row
    )
  )
}
