# Required starting capital to finance nominal constant periodic expenses
# pmt : periodic outflow per year (nominal)
# nper: number of years
# rate: interest rate per year
ucase6 <- function(pmt, nper, rate) {
  stopifnot(is.numeric(pmt), is.numeric(nper), is.numeric(rate))
  t <- 1:nper
  sum_expenses <- pmt * nper
  pv_required <- if (rate == 0) sum_expenses else pmt * (1 - (1 + rate)^(-nper)) / rate
  list(
    ok = TRUE,
    inputs = list(pmt = pmt, nper = nper, rate = rate),
    results = list(
      sum_expenses = sum_expenses,
      pv_required = pv_required,
      t = t,
      expense_path = rep(pmt, nper)
    )
  )
}
