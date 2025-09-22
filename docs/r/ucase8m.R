# Case 8m: Real yearly spending (actuarial) from given (real) yearly savings
# Inputs:
#   pmt_real_save : yearly savings (real, today's CHF) during working phase
#   nper_save     : years of saving
#   rate          : nominal interest rate r
#   inflation     : inflation rate i
#   x, lambda, m, b: Gompertz–Makeham mortality parameters at retirement
#
# Logic:
#   1) Accumulate real wealth at retirement from constant real savings:
#        rr = (1+r)/(1+i) - 1
#        FV_real = pmt * [ ((1+rr)^n - 1) / rr ]  (or pmt * n if rr ~ 0)
#   2) Convert FV_real into a constant real life annuity using survival-weighted
#      continuous discounting:
#        delta_real = log1p(r) - log1p(inflation)
#        S(t) = exp( - (lambda*t + exp((x - m)/b) * (exp(t/b) - 1)) )
#        a_real = ∫_0^{200} exp(-delta_real * t) * S(t) dt
#        spend_real = FV_real / a_real
#      fair_conversion_rate (real) = 1 / a_real
#
ucase8m <- function(pmt_real_save,
                    nper_save,
                    rate,
                    inflation,
                    x = 65, lambda = 0, m = 82.3, b = 11.4) {
  
  stopifnot(is.numeric(pmt_real_save), is.numeric(nper_save),
            is.numeric(rate), is.numeric(inflation),
            is.numeric(x), is.numeric(lambda), is.numeric(m), is.numeric(b))
  
  # --- (1) Accumulation in REAL terms (discrete real rate)
  rr <- (1 + rate) / (1 + inflation) - 1
  fv_real <- if (abs(rr) < 1e-12) {
    pmt_real_save * nper_save
  } else {
    pmt_real_save * ((1 + rr)^nper_save - 1) / rr
  }
  
  # --- (2) Life annuity in REAL terms with continuous discount & survival
  S <- function(t) exp(-(lambda * t + exp((x - m)/b) * (exp(t / b) - 1)))
  delta_real <- log1p(rate) - log1p(inflation)
  
  annuity_cont <- function(delta) {
    f <- function(t) exp(-delta * t) * S(t)
    stats::integrate(f, lower = 0, upper = 200, rel.tol = 1e-10, abs.tol = 0)$value
  }
  
  a_real <- annuity_cont(delta_real)
  beta_fair <- if (a_real > 0) 1 / a_real else NA_real_
  
  # Expected remaining lifetime E[T_x] = ∫ S(t) dt
  ETx <- stats::integrate(S, lower = 0, upper = 200, rel.tol = 1e-10, abs.tol = 0)$value
  
  spend_real <- if (a_real > 0) fv_real / a_real else NA_real_
  
  list(
    ok = TRUE,
    inputs = list(
      pmt_real_save = pmt_real_save,
      nper_save = nper_save,
      rate = rate,
      inflation = inflation,
      x = x, lambda = lambda, m = m, b = b
    ),
    results = list(
      fv_real_at_retirement = fv_real,
      spend_real_during_retirement = spend_real,
      expected_remaining_lifetime = ETx,
      fair_conversion_rate = beta_fair,
      annuity_factor_real = a_real
    )
  )
}
