# Future value of constant periodic payments (ordinary annuity, end-of-period)
# pmt: payment per period
# nper: number of periods (years)
# rate: interest rate per period (e.g., 0.02 for 2%)
ucase3 <- function(pmt, nper, rate) {
  stopifnot(is.numeric(pmt), is.numeric(nper), is.numeric(rate))
  t <- 1:nper
  sum_contrib <- pmt * nper
  if (rate == 0) {
    fv <- sum_contrib
  } else {
    fv <- pmt * ((1 + rate)^nper - 1) / rate
  }
  list(
    ok = TRUE,
    inputs  = list(pmt = pmt, nper = nper, rate = rate),
    results = list(
      sum_contrib = sum_contrib,
      fv = fv,
      t = t,
      contrib_path = rep(pmt, length(t))  # for plotting orange bars
    )
  )
}
