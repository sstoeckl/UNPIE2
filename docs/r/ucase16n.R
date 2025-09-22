# Case 16n: Moments and conditional survival probabilities for Gompertz–Makeham
# Two scenarios for comparison (Scenario 1: orange, Scenario 2: green).
#
# Parameterization matches your earlier apps (cases 14n/15n):
# Survival from age x for t >= 0:
#   S_x(t) = exp( -[ lambda * t + exp((x - m)/b) * b * (exp(t/b) - 1) ] )
# Hazard at age x+t:
#   h_x(t) = lambda + exp((x + t - m)/b)
# PDF of remaining life T_x:
#   f_x(t) = h_x(t) * S_x(t)
#
# Outputs:
#   results$scenario1/2:
#     - expected_remaining_lifetime
#     - median_remaining_lifetime
#     - conditional_probability_survive_y   (P{T_x >= y})
#     - y                                  (years used for the conditional prob)
#   results$ages        (vector of ages = x + t_grid)
#   results$density1/2  (pdf values at ages for both scenarios)

ucase16n <- function(
    # shared
  x = 65,
  # scenario 1 (orange)
  y1 = 5, lambda1 = 0.000, m1 = 82.3, b1 = 14.5,
  # scenario 2 (green)
  y2 = 5, lambda2 = 0.003, m2 = 80.9, b2 = 11.4,
  # plot horizon (years after x)
  chart_horizon_years = 50
) {
  # Survival S_x(t)
  S_fun <- function(t, x, lambda, m, b) {
    exp( - (lambda * t + exp((x - m)/b) * b * (exp(t / b) - 1)) )
  }
  # Hazard h_x(t)
  h_fun <- function(t, x, lambda, m, b) {
    lambda + exp((x + t - m)/b)
  }
  # PDF f_x(t) = h_x(t) * S_x(t)
  f_fun <- function(t, x, lambda, m, b) {
    h_fun(t, x, lambda, m, b) * S_fun(t, x, lambda, m, b)
  }
  
  # Expected remaining lifetime E[T_x] = ∫_0^∞ S_x(t) dt
  expected_life <- function(x, lambda, m, b) {
    stats::integrate(function(tt) S_fun(tt, x, lambda, m, b),
                     lower = 0, upper = 200, rel.tol = 1e-10, abs.tol = 0)$value
  }
  
  # Median remaining lifetime: solve S_x(t_med) = 0.5
  median_life <- function(x, lambda, m, b) {
    f <- function(tt) S_fun(tt, x, lambda, m, b) - 0.5
    stats::uniroot(f, lower = 1e-8, upper = 200, tol = 1e-10)$root
  }
  
  # Scenario 1 stats
  E1   <- expected_life(x, lambda1, m1, b1)
  med1 <- median_life( x, lambda1, m1, b1)
  cp1  <- S_fun(y1, x, lambda1, m1, b1)
  
  # Scenario 2 stats
  E2   <- expected_life(x, lambda2, m2, b2)
  med2 <- median_life( x, lambda2, m2, b2)
  cp2  <- S_fun(y2, x, lambda2, m2, b2)
  
  # Curves for the plot (densities over age)
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
