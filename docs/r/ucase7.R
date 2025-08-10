# Required starting wealth to finance yearly spending (nominal vs. real)
# pmt       : yearly spending amount stated in "today's CHF" (real base)
# nper      : number of years
# rate      : nominal interest rate (per year)
# inflation : inflation rate (per year)
#
# Outputs include:
# - pv_required_nominal: PV to finance constant *nominal* spending of size pmt (flat each year)
# - pv_required_real   : PV to finance constant *real* spending of size pmt (nominal grows with inflation)
# - sum_nominal        : Sum of nominal spending if keeping *real* spending pmt (i.e., index by inflation)
# - paths for charting
ucase7 <- function(pmt, nper, rate, inflation) {
  stopifnot(is.numeric(pmt), is.numeric(nper), is.numeric(rate), is.numeric(inflation))
  t <- 1:nper
  
  # ----- Case A: constant *nominal* spending = pmt every year (in CHF nominal)
  pv_required_nominal <- if (rate == 0) pmt * nper else pmt * (1 - (1 + rate)^(-nper)) / rate
  expense_nominal_path <- rep(pmt, nper)  # flat nominal line
  
  # ----- Case B: constant *real* spending = pmt real each year
  # nominal outflow in year t is indexed by inflation: pmt * (1+infl)^(t-1)
  expense_real_nominal_path <- pmt * (1 + inflation)^(t - 1)
  
  # Sum of nominal spending in Case B (pure indexation, no discounting)
  sum_nominal <- if (inflation == 0) pmt * nper else pmt * ((1 + inflation)^nper - 1) / inflation
  
  # PV to finance constant real spending (growing annuity with growth g = inflation)
  if (abs(rate - inflation) < 1e-12) {
    pv_required_real <- pmt * nper / (1 + rate)  # limit case r == g
  } else {
    pv_required_real <- pmt * (1 - ((1 + inflation) / (1 + rate))^nper) / (rate - inflation)
  }
  
  list(
    ok = TRUE,
    inputs = list(pmt = pmt, nper = nper, rate = rate, inflation = inflation),
    results = list(
      pv_required_nominal = pv_required_nominal,
      pv_required_real = pv_required_real,
      sum_nominal = sum_nominal,
      t = t,
      expense_nominal_path = expense_nominal_path,           # constant nominal (Case A)
      expense_real_nominal_path = expense_real_nominal_path  # indexed path (Case B)
    )
  )
}
