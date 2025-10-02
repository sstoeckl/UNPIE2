# Required starting wealth to finance yearly spending (nominal vs. real)
# pmt       : yearly spending in today's CHF (real base)
# nper      : number of years
# rate      : nominal interest rate (per year)
# inflation : inflation rate (per year)

ucase7 <- function(pmt, nper, rate, inflation) {
  stopifnot(is.numeric(pmt), is.numeric(nper), is.numeric(rate), is.numeric(inflation))
  t <- 1:nper
  
  # A) Constant nominal spending: flat nominal pmt each year
  pv_required_nominal <- if (rate == 0) pmt * nper else pmt * (1 - (1 + rate)^(-nper)) / rate
  expense_nominal_path <- rep(pmt, nper)
  
  # B) Constant real spending: first nominal payment occurs at t=1 and is pmt*(1+inflation)
  # -> index with exponent t (not t-1), so inflation already hits the first payment
  expense_real_nominal_path <- pmt * (1 + inflation)^(t)
  
  # real rate
  rr <- round((1 + rate) / (1 + inflation) - 1,4)
  
  # PV of a growing annuity with growth g = inflation and first payment A1 = pmt*(1+inflation)
  if (abs(rate - inflation) < 1e-12) {
    # limit r == g: PV = A1 * n / (1 + r)
    pv_required_real <- pmt / (1 + rr) * nper
  } else {
    pv_required_real <- pmt * ((1 + rr)^nper - 1)/(rr * (1 + rr)^nper)
  }
  
  list(
    ok = TRUE,
    inputs = list(pmt = pmt, nper = nper, rate = rate, inflation = inflation),
    results = list(
      rate_real = rr,
      pv_required_nominal = pv_required_nominal,
      pv_required_real = pv_required_real,
      t = t,
      expense_nominal_path = expense_nominal_path,           # constant nominal (orange)
      expense_real_nominal_path = expense_real_nominal_path  # indexed (grey), starts at t=1 with (1+inflation)
    )
  )
}
