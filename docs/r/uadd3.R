# add3: Years to ruin when spending yearly from given retirement savings
# spend_real : yearly (real) spending, paid at END of each year
# wealth0    : real wealth at retirement (t = 0)
# mu_real    : expected REAL return per year (e.g., 0.03)
# vol_real   : volatility of REAL return per year (e.g., 0.08)
# nper       : years to simulate (default 25, as in original app)
# n_scen     : number of Monte Carlo scenarios
# n_show     : number of sample paths to return for the chart
# seed       : numeric seed or NULL for random
#
# Model (real, end-of-period spending):
#   gross_t  = exp( m + vol_real * Z_t ),  Z ~ N(0,1),
#   m        = log(1 + mu_real) - 0.5*vol_real^2  (so E[gross]=1+mu_real)
#   W_t      = pmax(0, W_{t-1} * gross_t - spend_real),   W_0 = wealth0

uadd3 <- function(spend_real, wealth0, mu_real, vol_real,
                  nper = 25L, n_scen = 10000L, n_show = 10L, seed = NULL) {
  stopifnot(is.numeric(spend_real), is.numeric(wealth0),
            is.numeric(mu_real), is.numeric(vol_real))
  if (!is.null(seed)) set.seed(as.integer(seed))
  
  # drift for lognormal so that E[gross] = 1 + mu_real
  m <- log1p(mu_real) - 0.5 * vol_real^2
  
  Z <- matrix(rnorm(n_scen * nper), nrow = n_scen, ncol = nper)
  gross <- exp(m + vol_real * Z)
  
  W <- matrix(0.0, nrow = n_scen, ncol = nper)
  # year 1
  W[, 1] <- pmax(0, wealth0 * gross[, 1] - spend_real)
  # years 2..n
  if (nper >= 2L) {
    for (t in 2:nper) {
      W[, t] <- pmax(0, W[, t - 1] * gross[, t] - spend_real)
    }
  }
  
  # time to ruin (first year wealth hits 0)
  ruin_year <- apply(W, 1L, function(x) {
    k <- which(x <= 0)
    if (length(k)) k[1] else NA_integer_
  })
  
  # choose sample paths for display
  show_idx <- seq_len(min(n_show, n_scen))
  W_show <- W[show_idx, , drop = FALSE]
  ruin_show <- ruin_year[show_idx]
  
  # KPI table: unique sorted ruin years among shown paths + cumulative counts
  years <- sort(unique(ruin_show[!is.na(ruin_show)]))
  counts <- if (length(years)) sapply(years, function(y) sum(ruin_show <= y, na.rm = TRUE)) else integer(0)
  
  list(
    ok = TRUE,
    inputs = list(
      spend_real = spend_real, wealth0 = wealth0,
      mu_real = mu_real, vol_real = vol_real,
      nper = nper, n_scen = n_scen, n_show = n_show
    ),
    results = list(
      t = 1:nper,
      paths = W_show,                 # matrix n_show × nper
      ruin_years_show = ruin_show,    # length n_show
      table_years = years,            # KPI header row
      table_cum_ruins = counts        # KPI second row
    )
  )
}
