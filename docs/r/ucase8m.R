# Case 8m (final): Real pension payments from given (real) yearly savings
# Inputs (all "real" in t=0 purchasing power):
#   pmt_real_save = A^{real}  (yearly savings during working phase)
#   nper_save     = T         (years of saving)
#   rate = r (nominal, annual comp.)
#   inflation = i (annual comp.)
#   x, lambda, m, b = Gompertz–Makeham parameters (at retirement)
#
# Outputs:
#   spend_real_during_retirement = B^{real} (constant continuous real spending rate)
#   fv_real_at_retirement        = K_T^{real}
#   annuity_factor_real          = a_x^{real}
#   fair_conversion_rate         = 1 / a_x^{real}
#   expected_remaining_lifetime  = E[T_x]

ucase8m <- function(pmt_real_save,
                    nper_save,
                    rate,
                    inflation,
                    x = 65, lambda = 0, m = 82.3, b = 11.4) {
  
  stopifnot(is.numeric(pmt_real_save), is.numeric(nper_save),
            is.numeric(rate), is.numeric(inflation),
            is.numeric(x), is.numeric(lambda), is.numeric(m), is.numeric(b))
  
  # --- real discrete rate for accumulation ---
  r_real  <- (1 + rate) / (1 + inflation) - 1
  
  # K_T^{real}
  fv_real <- if (abs(r_real) < 1e-12) {
    pmt_real_save * nper_save
  } else {
    pmt_real_save * ((1 + r_real)^nper_save - 1) / r_real
  }
  
  # --- continuous real rate for payout ---
  rc_real <- log1p(rate) - log1p(inflation)
  
  # Survival S_x(t) under Gompertz–Makeham
  S <- function(t) exp(-(lambda * t + exp((x - m)/b) * (exp(t / b) - 1)))
  
  # a_x^{real}(rc_real) = int_0^200 e^{-rc_real t} S_x(t) dt
  a_real <- stats::integrate(function(t) exp(-rc_real * t) * S(t),
                             lower = 0, upper = 200,
                             rel.tol = 1e-10, abs.tol = 0)$value
  
  beta_fair <- if (a_real > 0) 1 / a_real else NA_real_
  
  # E[T_x] = int_0^200 S_x(t) dt
  ETx <- stats::integrate(S, 0, 200, rel.tol = 1e-10, abs.tol = 0)$value
  
  # B^{real}
  spend_real <- if (a_real > 0) fv_real / a_real else NA_real_
  
  list(
    ok = TRUE,
    inputs = list(
      pmt_real_save = pmt_real_save,
      nper_save     = nper_save,
      rate = rate, inflation = inflation,
      x = x, lambda = lambda, m = m, b = b
    ),
    results = list(
      fv_real_at_retirement       = fv_real,         # K_T^{real}
      spend_real_during_retirement= spend_real,      # B^{real}
      annuity_factor_real         = a_real,          # a_x^{real}
      fair_conversion_rate        = beta_fair,       # 1 / a_x^{real}
      expected_remaining_lifetime = ETx              # E[T_x]
    )
  )
}
