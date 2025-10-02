# Case 9: Yearly savings required to finance given (real) yearly spending
# spend_real   : yearly (real) spending during retirement
# nper_save    : years of saving (working phase)
# nper_spend   : years of spending (retirement)
# rate         : nominal interest rate
# inflation    : inflation rate

ucase9 <- function(spend_real, nper_save, nper_spend, rate, inflation) {
  stopifnot(is.numeric(spend_real), is.numeric(nper_save),
            is.numeric(nper_spend), is.numeric(rate), is.numeric(inflation))
  
  # real rate
  rr <- round((1 + rate) / (1 + inflation) - 1, 4)
  
  # Required real wealth at retirement to fund real spending (PV of real annuity)
  wealth_req <- if (abs(rr) < 1e-12) spend_real * nper_spend
  else spend_real * (1 - (1 + rr)^(-nper_spend)) / rr
  
  # Required constant real savings during working to accumulate that wealth (solve FV annuity)
  # FV_real_from_saving = p * ((1+rr)^n - 1) / rr  =>  p = FV * rr / ((1+rr)^n - 1)
  save_real <- if (abs(rr) < 1e-12) wealth_req / nper_save
  else wealth_req * rr / ((1 + rr)^nper_save - 1)
  
  list(
    ok = TRUE,
    inputs = list(
      spend_real = spend_real,
      nper_save = nper_save,
      nper_spend = nper_spend,
      rate = rate,
      inflation = inflation
    ),
    results = list(
      rate_real = rr,
      required_wealth_at_retirement_real = wealth_req,
      required_savings_real = save_real
    )
  )
}
