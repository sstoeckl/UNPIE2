# docs/r/ucase1.R
ucase1 <- function(rate, nper, pv) {
  stopifnot(is.numeric(rate), is.numeric(nper), is.numeric(pv))
  t <- 0:nper
  fv_path <- -pv * (1 + rate)^t
  list(
    ok = TRUE,
    inputs  = list(rate = rate, nper = nper, pv = pv),
    results = list(
      fv = tail(fv_path, 1),
      t  = t,
      fv_path = fv_path
    )
  )
}
