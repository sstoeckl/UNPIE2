# Case 14n: Fair conversion rate (wealth -> lifelong pension)
# Implements a Gompertz–Makeham survival with parameters:
#   lambda = accidental death rate (Makeham term)
#   m      = modal natural death age
#   b      = dispersion
# Discounts with the *real* rate: (1+r)/(1+inflation) - 1
# Outputs: survival curve, annuity factor, fair conversion rate, expected remaining lifetime.

ucase14n <- function(x = 65, lambda = 0, m = 82.3, b = 11.4, r = 0.03, inflation = 0.01) {
  # real discount rate and factor
  real_r <- (1 + r) / (1 + inflation) - 1
  v <- 1 / (1 + real_r)
  
  # horizon up to max_age
  max_age <- 120
  T <- max(0, floor(max_age - x))
  t <- seq(0, T, by = 1)
  
  # Gompertz–Makeham survival from age x to x+t:
  # hazard(u) = lambda + exp((x+u - m)/b)
  # cumulative hazard over [0,t]:
  #   Lambda(t) = lambda * t + exp((x - m)/b) * b * (exp(t/b) - 1)
  # survival S(t) = exp(-Lambda(t))
  surv <- exp(- (lambda * t + exp((x - m)/b) * b * (exp(t / b) - 1)))
  
  # Discrete immediate annuity: payments at t=1,2,... with probability of survival to t years
  if (T >= 1) {
    annuity_factor <- sum(v^(1:T) * surv[2:(T + 1)])  # surv at 1..T
  } else {
    annuity_factor <- 0
  }
  
  fair_conversion_rate <- ifelse(annuity_factor > 0, 1 / annuity_factor, NA_real_)
  
  # Expected remaining lifetime (approx, in years): sum of S(t) over t=0..T-1 with step 1
  # (discrete approximation of integral of survival)
  if (T >= 1) {
    erl <- sum(surv[1:T])
  } else {
    erl <- 0
  }
  
  ages <- x + t
  
  list(
    results = list(
      ages = ages,
      survival = surv,
      annuity_factor = annuity_factor,
      fair_conversion_rate = fair_conversion_rate,
      expected_remaining_lifetime = erl
    )
  )
}
