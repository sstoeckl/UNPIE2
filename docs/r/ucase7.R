# Required starting wealth to finance yearly spending (nominal vs. real)
# pmt       : yearly spending amount stated in "today's CHF" (real base)
# nper      : number of years
# rate      : nominal interest rate (per year)
# inflation : inflation rate (per year)

ucase7 <- function(pmt, nper, rate, inflation) {
  stopifnot(is.numeric(pmt), is.numeric(nper), is.numeric(rate), is.numeric(inflation))
  t <- 1:nper
  
  # A) Constant *nominal* spending: pmt every year (flat nominal)
  pv_required_nominal <- if (rate == 0) pmt * nper else pmt * (1 - (1 + rate)^(-nper)) / rate
  expense_nominal_path <- rep(pmt, nper)
  
  # B) Constant *real* spending: pmt real each year -> nominal grows with inflation
  expense_real_nominal_path <- pmt * (1 + inflation)^(t - 1)
  
  # PV to finance constant real spending (growing annuity with growth = inflation)
  if (abs(rate - inflation) < 1e-12) {
    # Limit case r == g: PV = pmt * n / (1+r)
    pv_required_real <- pmt * nper / (1 + rate)
  } else {
    pv_required_real <- pmt * (1 - ((1 + inflation) / (1 + rate))^nper) / (rate - inflation)
  }
  
  list(
    ok = TRUE,
    inputs = list(pmt = pmt, nper = nper, rate = rate, inflation = inflation),
    results = list(
      pv_required_nominal = pv_required_nominal,
      pv_required_real = pv_required_real,
      t = t,
      expense_nominal_path = expense_nominal_path,           # constant nominal path (orange)
      expense_real_nominal_path = expense_real_nominal_path  # indexed path (grey)
    )
  )
}
