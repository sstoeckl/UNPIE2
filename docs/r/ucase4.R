# Future value (nominal & real) of nominally constant periodic payments
# pmt: nominal payment per period (end-of-period, ordinary annuity)
# nper: number of periods
# rate: nominal interest rate per period
# inflation: inflation rate per period
ucase4 <- function(pmt, nper, rate, inflation) {
  stopifnot(is.numeric(pmt), is.numeric(nper), is.numeric(rate), is.numeric(inflation))
  
  t <- 1:nper
  
  # Nominal sum of contributions (just pmt * nper)
  sum_nominal <- pmt * nper
  
  # Nominal FV of annuity (ordinary, end-of-period)
  if (rate == 0) {
    fv_nominal <- sum_nominal
  } else {
    fv_nominal <- pmt * ((1 + rate)^nper - 1) / rate
  }
  
  # Real FV (deflate nominal FV by cumulated inflation), equivalent to using real rate
  fv_real <- fv_nominal / (1 + inflation)^nper
  
  # Contribution bars (orange) for years 1..n
  contrib_path <- rep(pmt, length(t))
  
  list(
    ok = TRUE,
    inputs  = list(pmt = pmt, nper = nper, rate = rate, inflation = inflation),
    results = list(
      sum_nominal = sum_nominal,
      fv_nominal  = fv_nominal,
      fv_real     = fv_real,
      t = t,
      contrib_path = contrib_path
    )
  )
}
