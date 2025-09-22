# Case 7m: Pension fund's view — required wealth for constant pension payments
# Inputs (all annual): spending (nominal or real), r, inflation, beta(conv. rate),
# Gompertz–Makeham params (x, lambda, m, b)

ucase7m <- function(spending = 12000,
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
  
  # Expected remaining lifetime E[T_x] = ∫ S(t) dt
  ETx <- stats::integrate(S, lower = 0, upper = 200, rel.tol = 1e-10, abs.tol = 0)$value
  
  # Required wealth bars
  wealth_nominal_yearly <- spending * a_nom                  # orange in screenshot
  wealth_real_yearly    <- spending * a_real                 # grey
  wealth_via_beta       <- if (beta > 0) spending / beta else NA_real_  # green
  
  list(
    results = list(
      wealth_nominal_yearly = wealth_nominal_yearly,
      wealth_real_yearly    = wealth_real_yearly,
      wealth_via_beta       = wealth_via_beta,
      expected_remaining_lifetime = ETx,
      fair_conversion_rate  = beta_fair
    )
  )
}
