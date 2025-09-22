# Case 16n: Moments & conditional survival (Gompertz–Makeham) — TWO scenarios
# Matches the original 'gompertzMakehamMortality' parameterization.

ucase16n <- function(
    x = 65,
    # scenario 1 (orange)
    y1 = 5, lambda1 = 0.000, m1 = 82.3, b1 = 14.5,
    # scenario 2 (green)
    y2 = 5, lambda2 = 0.003, m2 = 80.9, b2 = 11.4,
    chart_horizon_years = 50
) {
  # Survival S_x(t) with Lambda_x(t) = lambda*t + exp((x-m)/b)*(exp(t/b)-1)
  S_fun <- function(t, x, lambda, m, b) {
    exp(-(lambda * t + exp((x - m)/b) * (exp(t / b) - 1)))
  }
  
  # Hazard at age x+t: h_x(t) = lambda + (1/b) * exp((x+t-m)/b)
  h_fun <- function(t, x, lambda, m, b) {
    lambda + (1 / b) * exp((x + t - m) / b)
  }
  
  # PDF of remaining lifetime: f_x(t) = h_x(t) * S_x(t)
  f_fun <- function(t, x, lambda, m, b) {
    h_fun(t, x, lambda, m, b) * S_fun(t, x, lambda, m, b)
  }
  
  # Expected remaining lifetime E[T_x] = ∫ S_x(t) dt  (high-precision numeric)
  expected_life <- function(x, lambda, m, b) {
    stats::integrate(function(tt) S_fun(tt, x, lambda, m, b),
                     lower = 0, upper = 200, rel.tol = 1e-10, abs.tol = 0)$value
  }
  
  # Median remaining lifetime: S_x(t_med) = 0.5
  median_life <- function(x, lambda, m, b) {
    f <- function(tt) S_fun(tt, x, lambda, m, b) - 0.5
    stats::uniroot(f, lower = 1e-8, upper = 200, tol = 1e-10)$root
  }
  
  # Scenario 1
  E1   <- expected_life(x, lambda1, m1, b1)
  med1 <- median_life( x, lambda1, m1, b1)
  cp1  <- S_fun(y1, x, lambda1, m1, b1)
  
  # Scenario 2
  E2   <- expected_life(x, lambda2, m2, b2)
  med2 <- median_life( x, lambda2, m2, b2)
  cp2  <- S_fun(y2, x, lambda2, m2, b2)
  
  # Densities over age for the chart
  t_grid <- seq(0, chart_horizon_years, by = 1)
  ages   <- x + t_grid
  den1   <- f_fun(t_grid, x, lambda1, m1, b1)
  den2   <- f_fun(t_grid, x, lambda2, m2, b2)
  
  list(
    results = list(
      scenario1 = list(
        expected_remaining_lifetime = as.numeric(E1),
        median_remaining_lifetime   = as.numeric(med1),
        conditional_probability_survive_y = as.numeric(cp1),
        y = as.numeric(y1)
      ),
      scenario2 = list(
        expected_remaining_lifetime = as.numeric(E2),
        median_remaining_lifetime   = as.numeric(med2),
        conditional_probability_survive_y = as.numeric(cp2),
        y = as.numeric(y2)
      ),
      ages = ages,
      density1 = den1,
      density2 = den2
    )
  )
}
