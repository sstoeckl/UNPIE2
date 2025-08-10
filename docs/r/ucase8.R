# Case 8: Real yearly spending financed by given (real) yearly savings
# pmt_real_save : yearly savings (real, today's CHF) during working phase
# nper_save     : years of saving
# nper_spend    : years of spending (retirement)
# rate          : nominal interest rate
# inflation     : inflation rate

ucase8 <- function(pmt_real_save, nper_save, nper_spend, rate, inflation) {
  stopifnot(is.numeric(pmt_real_save), is.numeric(nper_save),
            is.numeric(nper_spend), is.numeric(rate), is.numeric(inflation))
  
  # real rate
  rr <- (1 + rate) / (1 + inflation) - 1
  
  # Future wealth at retirement in REAL CHF from constant REAL savings
  fv_real <- if (abs(rr) < 1e-12) pmt_real_save * nper_save
  else pmt_real_save * ((1 + rr)^nper_save - 1) / rr
  
  # Real yearly spending during retirement such that wealth is exactly amortized
  spend_real <- if (abs(rr) < 1e-12) fv_real / nper_spend
  else fv_real * rr / (1 - (1 + rr)^(-nper_spend))
  
  list(
    ok = TRUE,
    inputs = list(
      pmt_real_save = pmt_real_save,
      nper_save = nper_save,
      nper_spend = nper_spend,
      rate = rate,
      inflation = inflation
    ),
    results = list(
      fv_real_at_retirement = fv_real,
      spend_real_during_retirement = spend_real
    )
  )
}
