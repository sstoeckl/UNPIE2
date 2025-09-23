# Case 8m: Real yearly spending (actuarial) from given (real) yearly savings
ucase8m <- function(pmt_real_save,
                    nper_save,
                    nper_spend,      # kept for UI parity; not used in actuarial payout
                    rate,
                    inflation,
                    x = 65, lambda = 0, m = 82.3, b = 11.4) {
  
  stopifnot(is.numeric(pmt_real_save), is.numeric(nper_save), is.numeric(nper_spend),
            is.numeric(rate), is.numeric(inflation),
            is.numeric(x), is.numeric(lambda), is.numeric(m), is.numeric(b))
  
  # (1) accumulate in REAL CHF from constant REAL savings
  rr <- (1 + rate) / (1 + inflation) - 1
  fv_real <- if (abs(rr) < 1e-12) {
    pmt_real_save * nper_save
  } else {
    pmt_real_save * ((1 + rr)^nper_save - 1) / rr
  }
  
  # (2) convert to constant REAL life annuity with GM survival + continuous discount
  S <- function(t) exp(-(lambda * t + exp((x - m)/b) * (exp(t / b) - 1)))
  delta_real <- log1p(rate) - log1p(inflation)
  a_real <- stats::integrate(function(t) exp(-delta_real * t) * S(t),
                             0, 200, rel.tol = 1e-10, abs.tol = 0)$value
  beta_fair <- if (a_real > 0) 1 / a_real else NA_real_
  ETx <- stats::integrate(S, 0, 200, rel.tol = 1e-10, abs.tol = 0)$value
  
  spend_real <- if (a_real > 0) fv_real / a_real else NA_real_
  
  list(
    ok = TRUE,
    inputs = list(
      pmt_real_save = pmt_real_save, nper_save = nper_save, nper_spend = nper_spend,
      rate = rate, inflation = inflation, x = x, lambda = lambda, m = m, b = b
    ),
    results = list(
      fv_real_at_retirement = fv_real,
      spend_real_during_retirement = spend_real,   # shown as grey bar/badge
      expected_remaining_lifetime = ETx,
      fair_conversion_rate = beta_fair,
      annuity_factor_real = a_real
    )
  )
}
