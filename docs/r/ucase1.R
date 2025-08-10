# docs/r/ucase1.R
ucase1 <- function(rate, nper, pv) {
  stopifnot(is.numeric(rate), is.numeric(nper), is.numeric(pv))
  # Sign convention: in your old UI pv was passed as -pv; here we'll keep it explicit.
  fv <- (-pv) * (1 + rate)^nper
  list(
    ok = TRUE,
    inputs  = list(rate = rate, nper = nper, pv = pv),
    results = list(fv = fv)
  )
}
