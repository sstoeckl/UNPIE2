# Case 8m: Member’s view — affordable constant pension payments given wealth K
# Inputs (annual):
#   K, r, inflation, beta, x, lambda, m, b
# Outputs:
#   results$spend_nominal_yearly
#   results$spend_real_yearly
#   results$spend_via_beta
#   results$expected_remaining_lifetime
#   results$fair_conversion_rate

ucase8m <- function(K = 200000,
                    r = 0.03, inflation = 0.01, beta = 0.05,
                    x = 65, lambda = 0, m = 82.3, b = 11.4) {
  
  # GM survival (matches your gompertzMakehamMortality)
  S <- function(t) exp(-(lambda * t + exp((x - m)/b) * (exp(t / b) - 1)))
  
  # continuous discount rates
  delta_nom  <- log1p(r)
  delta_real <- log1p(r) - log1p(inflation)
  
  annuity_cont <- function(delta) {
    f <- function(t) exp(-delta * t) * S(t)
    stats::integrate(f, lower = 0, upper = 200, rel.tol = 1e-10, abs.tol = 0)$value
  }
  
  a_nom  <- annuity_cont(delta_nom)
  a_real <- annuity_cont(delta_real)
  beta_fair <- if (a_real > 0) 1 / a_real else NA_real_
  
  # Expected remaining lifetime
  ETx <- stats::integrate(S, lower = 0, upper = 200, rel.tol = 1e-10, abs.tol = 0)$value
  
  # Affordable spend levels
  spend_nominal_yearly <- if (a_nom  > 0) K / a_nom  else NA_real_
  spend_real_yearly    <- if (a_real > 0) K / a_real else NA_real_
  spend_via_beta       <- beta * K
  
  list(
    results = list(
      spend_nominal_yearly = spend_nominal_yearly,
      spend_real_yearly    = spend_real_yearly,
      spend_via_beta       = spend_via_beta,
      expected_remaining_lifetime = ETx,
      fair_conversion_rate = beta_fair
    )
  )
}
