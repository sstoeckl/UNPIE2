# Case 15n (original): Mortality rate (exp. hazard) and probability of retirement ruin
# Inputs:
#   x, lambda, m, b  ... Gompertz–Makeham mortality params
#   r                ... expected REAL return (annual compounding)
#   sigma            ... st.dev. of annualized REAL log returns
#   inflation        ... (not used here; r is real already)
#   B                ... constant continuous REAL spending per year
#   K                ... wealth at retirement (REAL, t=0 purchasing power)
#
# Outputs (exactly like wrapper.case15n):
#   results$mortality_rate
#   results$probability_of_retirement_ruin_when_starting_at_wealth_w
#
# Formulas:
#   Survival S(t|x) = exp( -[ lambda*t + exp((x-m)/b) * (exp(t/b)-1) ] )
#   median_remaining_lifetime: solve S(t_med)=0.5
#   mortality_rate λ^exp = log(2) / t_med
#   ruin prob ≈ pgamma(B/K, shape = (2*log(1+r)+4*λ^exp)/(sigma^2+λ^exp) - 1,
#                               scale = (sigma^2+λ^exp)/2)

ucase15n <- function(x = 65, lambda = 0, m = 82.3, b = 11.4,
                     r = 0.03, sigma = 0.08, inflation = 0.01,
                     B = 100, K = 1000) {
  
  # Survival under Gompertz–Makeham
  S <- function(t) {
    cum_haz <- lambda * t + exp((x - m)/b) * (exp(t / b) - 1)
    exp(-cum_haz)
  }
  
  # Median remaining lifetime t_med with S(t_med) = 0.5
  f_med <- function(t) S(t) - 0.5
  # robust bracketing
  lower <- 1e-8
  upper <- 200
  # ensure sign change (S(0)=1, hence f_med(0)=+0.5; at large t, S -> 0, hence negative)
  t_med <- uniroot(f_med, lower = lower, upper = upper, tol = 1e-10)$root
  
  # Exponential mortality rate matching the median
  mortality_rate <- log(2) / t_med
  
  # Gamma approximation of ruin probability (inputs are REAL)
  shape <- (2 * log1p(r) + 4 * mortality_rate) / (sigma^2 + mortality_rate) - 1
  scale <- (sigma^2 + mortality_rate) / 2
  prob_ruin <- pgamma(q = B / K, shape = shape, scale = scale)
  
  list(
    results = list(
      mortality_rate = as.numeric(mortality_rate),
      probability_of_retirement_ruin_when_starting_at_wealth_w = as.numeric(prob_ruin)
    )
  )
}
