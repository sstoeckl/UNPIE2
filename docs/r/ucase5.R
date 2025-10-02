# Future value (nominal & real) of constant *real* yearly payments
# pmt_real : payment in real terms (today's CHF) each year (end-of-period)
# rate     : nominal interest rate
# inflation: inflation rate
# nper     : number of years
ucase5 <- function(pmt_real, nper, rate, inflation) {
  stopifnot(is.numeric(pmt_real), is.numeric(nper), is.numeric(rate), is.numeric(inflation))
  t <- 1:nper
  
  # nominal path of each year's payment (indexing previous real pmt by inflation)
  pmt_nominal_t <- pmt_real * (1 + inflation)^(t)
  

  
  # FV in real CHF (ordinary annuity end-of-period at real rate)
  fv_real <- if (rr == 0) pmt_real * nper else pmt_real * ((1 + rr)^nper - 1) / rr
  
  # FV in nominal CHF at time T
  fv_nominal <- fv_real * (1 + inflation)^nper
  
  # sum of nominal payments (just index the stream, no interest)
  sum_nominal <- if (inflation == 0) {pmt_real * nper 
    } else {pmt_real * ((1 + inflation)^(nper+1) - (1 + inflation)) / inflation}
  
  list(
    ok = TRUE,
    inputs = list(pmt_real = pmt_real, nper = nper, rate = rate, inflation = inflation),
    results = list(
      rate_real = rr,
      fv_nominal = fv_nominal,
      fv_real = fv_real,
      sum_nominal = sum_nominal,
      t = t,
      contrib_real_path = rep(pmt_real, nper),
      contrib_nominal_path = pmt_nominal_t
    )
  )
}
