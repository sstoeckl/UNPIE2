# Case 15n (original + median lifetime):
# Returns the exponential mortality rate matched to the GM median,
# the probability of retirement ruin (gamma approximation),
# and the median expected lifetime (years).
#
# Inputs (real terms):
#   x, lambda, m, b ... Gompertz–Makeham parameters
#   r                ... expected real return (annual compounding)
#   sigma            ... st.dev. of annualized real log returns
#   inflation        ... ignored here (r already real)
#   B                ... constant continuous real spending per year
#   K                ... wealth at retirement (real, t=0 purchasing power)

ucase15n <- function(x = 65, lambda = 0, m = 82.3, b = 11.4,
                     r = 0.03, sigma = 0.08, inflation = 0.01,
                     B = 100, K = 1000,
                     chart_horizon_years = 50) {
  
  # Gompertz–Makeham survival S(t|x)
  S_gm <- function(t) {
    cum_haz <- lambda * t + exp((x - m) / b) * (exp(t / b) - 1)
    exp(-cum_haz)
  }
  
  # Median remaining lifetime t_med: S(t_med) = 0.5
  f_med <- function(t) S_gm(t) - 0.5
  t_med <- uniroot(f_med, lower = 1e-8, upper = 200, tol = 1e-10)$root
  
  # Exponential mortality rate matched to the GM median
  mort_rate <- round(log(2) / t_med, 4)  # λ^exp
  
  # Gamma-approx ruin probability (same form as in the original wrapper)
  shape <- (2 * log1p(r) + 4 * mort_rate) / (sigma^2 + mort_rate) - 1
  scale <- (sigma^2 + mort_rate) / 2
  prob_ruin <- pgamma(q = B / K, shape = shape, scale = scale)
  
  # Data for the plot: GM vs matched exponential survival
  t <- seq(0, chart_horizon_years, by = 1)
  S_exp <- exp(-mort_rate * t)
  S_gm_vec <- S_gm(t)
  
  list(
    results = list(
      mortality_rate = as.numeric(mort_rate),
      probability_of_retirement_ruin_when_starting_at_wealth_w = as.numeric(prob_ruin),
      median_expected_lifetime = as.numeric(t_med),        # NEW (years)
      # for plotting:
      t_years = t,
      survival_gm = S_gm_vec,
      survival_exp = S_exp
    )
  )
}
