# Case 14n (original behavior): "Fair" conversion rate with constant expected returns
# Real discounting only (single output rate), survival returned for plotting.
#
# Survival (Gompertz–Makeham, modal m, dispersion b):
#   S(t | x) = exp( - [ λ t + exp((x - m)/b) * (exp(t/b) - 1) ] )
# Continuous real discount rate:
#   δ_real = ln(1 + r) - ln(1 + inflation)
# Continuous-life annuity (rate 1 per year):
#   ā_x(δ) = ∫_0^∞ e^{-δ t} S(t|x) dt
# Fair conversion rate:
#   β_fair = 1 / ā_x(δ_real)

ucase14n <- function(x = 65, lambda = 0, m = 82.3, b = 11.4,
                     r = 0.03, inflation = 0.01,
                     chart_max_age = 120) {
  
  # Real continuous discount rate
  delta_real <- log1p(r) - log1p(inflation)  # ln(1+r) - ln(1+i)
  
  # Survival function S(t | x) under Gompertz–Makeham
  S <- function(t) {
    # cumulative hazard over [0, t]:
    #   Λ(t) = λ t + exp((x - m)/b) * (exp(t/b) - 1)
    cum_haz <- lambda * t + exp((x - m)/b) * (exp(t / b) - 1)
    exp(-cum_haz)
  }
  
  # Continuous-life annuity at real rate via numeric integration
  annuity_real <- (function(delta) {
    f <- function(t) exp(-delta * t) * S(t)
    # Upper bound large enough for numerical convergence
    stats::integrate(f, lower = 0, upper = 200, rel.tol = 1e-10, abs.tol = 0)$value
  })(delta_real)
  
  fair_rate <- if (is.finite(annuity_real) && annuity_real > 0) 1 / annuity_real else NA_real_
  
  # Data for the plot (survival vs. age)
  max_age <- max(x, chart_max_age)
  T <- max(0, floor(max_age - x))
  t_seq <- seq(0, T, by = 1)
  ages  <- x + t_seq
  surv  <- S(t_seq)
  
  list(
    results = list(
      annuity_real = annuity_real,
      fair_conversion_rate = fair_rate,  # single (real) rate, as in original wrapper
      ages = ages,                       # for plotting
      survival = surv                    # for plotting
    )
  )
}
