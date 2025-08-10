# Future value (nominal & real) of a one-time contribution
# Returns final values and the full paths (optional for future line charts)
ucase2 <- function(rate, inflation, nper, pv) {
  stopifnot(is.numeric(rate), is.numeric(inflation), is.numeric(nper), is.numeric(pv))
  t <- 0:nper
  fv_nom_path  <- pv * (1 + rate)^t
  defl_factor  <- (1 + inflation)^t
  fv_real_path <- fv_nom_path / defl_factor
  list(
    ok = TRUE,
    inputs  = list(rate = rate, inflation = inflation, nper = nper, pv = pv),
    results = list(
      fv_nom  = tail(fv_nom_path, 1),
      fv_real = tail(fv_real_path, 1),
      t = t,
      fv_nom_path = fv_nom_path,
      fv_real_path = fv_real_path
    )
  )
}
