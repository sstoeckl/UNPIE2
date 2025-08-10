# ucase10: Real yearly pension resulting from constant (real) yearly savings
# pmt_real : yearly savings in today's (real) CHF, paid at END of each year
# nper     : years to retirement
# mu_real  : expected REAL return per year (e.g. 0.03 for 3%)
# vol_real : volatility of REAL return per year (e.g. 0.08 for 8%)
# conv     : conversion rate to compute lifelong REAL pension at retirement (e.g. 0.05)
# n_scen   : number of Monte Carlo scenarios (for quantiles)
# n_show   : number of sample paths to return for charting
#
# Model: gross return_t = exp( m + vol_real * Z_t ),  Z ~ N(0,1)
# with m chosen so that E[gross] = 1 + mu_real  => m = log(1 + mu_real) - 0.5*vol_real^2
# Wealth recursion (end-of-period contributions):
#   W_t = W_{t-1} * gross_t + pmt_real ,  W_0 = 0
ucase10 <- function(pmt_real, nper, mu_real, vol_real, conv,
                  n_scen = 10000L, n_show = 10L, seed = NULL) {
  stopifnot(is.numeric(pmt_real), is.numeric(nper),
            is.numeric(mu_real), is.numeric(vol_real), is.numeric(conv))
  if (!is.null(seed)) set.seed(seed)
  
  # drift that matches E[gross] = 1 + mu_real for lognormal
  m <- log1p(mu_real) - 0.5 * vol_real^2
  Z <- matrix(rnorm(n_scen * nper), nrow = n_scen, ncol = nper)
  gross <- exp(m + vol_real * Z)   # n_scen × nper
  
  # propagate wealth paths
  W <- matrix(0.0, nrow = n_scen, ncol = nper)
  for (t in 1:nper) {
    if (t == 1) {
      W[, t] <- 0 * gross[, t] + pmt_real
    } else {
      W[, t] <- W[, t - 1] * gross[, t] + pmt_real
    }
  }
  
  # terminal distribution & quantiles
  W_T <- W[, nper]
  probs <- seq(0.1, 0.9, by = 0.1)              # 10th … 90th
  qW <- as.numeric(quantile(W_T, probs = probs, names = FALSE))
  qP <- conv * qW                                # pension = conv-rate × wealth
  
  # sample a few scenarios for plotting (fixed, reproducible order)
  show_idx <- seq_len(min(n_show, n_scen))
  W_show <- W[show_idx, , drop = FALSE]
  
  list(
    ok = TRUE,
    inputs = list(
      pmt_real = pmt_real, nper = nper,
      mu_real = mu_real, vol_real = vol_real, conv = conv,
      n_scen = n_scen, n_show = n_show
    ),
    results = list(
      t = 1:nper,
      paths = W_show,                     # matrix: n_show × nper
      quantiles_wealth = qW,              # length 9 (10%..90%)
      quantiles_pension = qP,             # same length
      probs = probs
    )
  )
}
