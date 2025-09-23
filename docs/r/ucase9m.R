# Case 9m (webR): Required real yearly savings to finance given continuous real pension
# Inputs (all real are in t=0 purchasing power):
#   spend_real_target  B^{real} : continuous real pension per year (life-long)
#   nper_save          T        : years of saving (from t=1 ... T)
#   rate               r        : nominal interest (annual comp.)
#   inflation          i        : inflation rate (annual comp.)
#   x, lambda, m, b             : Gompertz–Makeham mortality parameters at retirement
#
# Outputs:
#   required_savings_real_yearly  A^{real}                      (per year, real)
#   fv_real_at_retirement         K_T^{real} = B^{real} * a_x^{real}
#   annuity_factor_real           a_x^{real} = ∫ e^{-r_c^{real} t} S_x(t) dt
#   fair_conversion_rate          beta* = 1 / a_x^{real}
#   expected_remaining_lifetime   E[T_x] = ∫ S_x(t) dt

ucas9m <- function(spend_real_target,
                   nper_save,
                   rate,
                   inflation,
                   x = 65, lambda = 0, m = 82.3, b = 11.4) {
  
  stopifnot(is.numeric(spend_real_target), is.numeric(nper_save),
            is.numeric(rate), is.numeric(inflation),
            is.numeric(x), is.numeric(lambda), is.numeric(m), is.numeric(b))
  
  # --- Payout leg (continuous real discount) ---
  rc_real <- log1p(rate) - log1p(inflation)
  S <- function(t) exp(-(lambda * t + exp((x - m)/b) * (exp(t / b) - 1)))
  
  a_real <- stats::integrate(function(t) exp(-rc_real * t) * S(t),
                             lower = 0, upper = 200,
                             rel.tol = 1e-10, abs.tol = 0)$value
  beta_fair <- if (a_real > 0) 1 / a_real else NA_real_
  ETx <- stats::integrate(S, 0, 200, rel.tol = 1e-10, abs.tol = 0)$value
  
  # Wealth needed at retirement (real)
  fv_real <- spend_real_target * a_real
  
  # --- Saving leg (discrete real rate) ---
  r_real <- (1 + rate) / (1 + inflation) - 1
  req_savings <- if (abs(r_real) < 1e-12) {
    # A^{real} = K_T^{real} / T    (limit r_real -> 0)
    fv_real / nper_save
  } else {
    # A^{real} = K_T^{real} * r_real / ((1 + r_real)^T - 1)
    fv_real * r_real / ((1 + r_real)^nper_save - 1)
  }
  
  list(
    ok = TRUE,
    inputs = list(
      spend_real_target = spend_real_target,
      nper_save = nper_save,
      rate = rate, inflation = inflation,
      x = x, lambda = lambda, m = m, b = b
    ),
    results = list(
      required_savings_real_yearly = req_savings, # A^{real}
      fv_real_at_retirement = fv_real,            # K_T^{real}
      annuity_factor_real = a_real,               # a_x^{real}
      fair_conversion_rate = beta_fair,           # 1 / a_x^{real}
      expected_remaining_lifetime = ETx           # E[T_x]
    )
  )
}
