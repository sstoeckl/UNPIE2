# Case 8m (webR): Real pension payments financed by given (real) savings
# Inputs (real = t=0 purchasing power):
#   pmt_real_save  A^{real} : yearly savings during working phase
#   nper_save      T        : years of saving
#   rate           r        : nominal interest (annual comp.)
#   inflation      i        : inflation (annual comp.)
#   x, lambda, m, b         : Gompertz–Makeham parameters at retirement
#
# Outputs:
#   fv_real_at_retirement        K_T^{real}
#   spend_real_during_retirement B^{real} = K_T^{real} / a_x^{real}
#   annuity_factor_real          a_x^{real} = ∫ e^{-r_c^{real} t} S_x(t) dt
#   fair_conversion_rate         beta* = 1 / a_x^{real}
#   expected_remaining_lifetime  E[T_x] = ∫ S_x(t) dt

ucas8m <- function(pmt_real_save,
                   nper_save,
                   rate,
                   inflation,
                   x = 65, lambda = 0, m = 82.3, b = 11.4) {
  
  stopifnot(is.numeric(pmt_real_save), is.numeric(nper_save),
            is.numeric(rate), is.numeric(inflation),
            is.numeric(x), is.numeric(lambda), is.numeric(m), is.numeric(b))
  
  # (1) Accumulation in REAL terms (discrete real rate)
  r_real <- (1 + rate) / (1 + inflation) - 1
  fv_real <- if (abs(r_real) < 1e-12) {
    pmt_real_save * nper_save
  } else {
    pmt_real_save * ((1 + r_real)^nper_save - 1) / r_real
  }
  
  # (2) Life annuity in REAL terms with continuous discount + survival
  rc_real <- log1p(rate) - log1p(inflation)
  S <- function(t) exp(-(lambda * t + exp((x - m)/b) * (exp(t / b) - 1)))
  
  a_real <- stats::integrate(function(t) exp(-rc_real * t) * S(t),
                             lower = 0, upper = 200,
                             rel.tol = 1e-10, abs.tol = 0)$value
  
  beta_fair <- if (a_real > 0) 1 / a_real else NA_real_
  ETx <- stats::integrate(S, 0, 200, rel.tol = 1e-10, abs.tol = 0)$value
  spend_real <- if (a_real > 0) fv_real / a_real else NA_real_
  
  list(
    ok = TRUE,
    inputs = list(
      pmt_real_save = pmt_real_save,
      nper_save = nper_save,
      rate = rate, inflation = inflation,
      x = x, lambda = lambda, m = m, b = b
    ),
    results = list(
      fv_real_at_retirement = fv_real,
      spend_real_during_retirement = spend_real,
      annuity_factor_real = a_real,
      fair_conversion_rate = beta_fair,
      expected_remaining_lifetime = ETx
    )
  )
}
