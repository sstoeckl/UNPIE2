# add4 (fast, corrected tail): max real yearly spending s* s.t.
# P(time_to_ruin >= N_min) >= p. Uses 1-p quantile of path thresholds.

uadd4 <- function(wealth0, min_years, prob_ok, mu_real, vol_real,
                  nper = 25L, n_scen = 10000L, n_show = 10L, seed = NULL,
                  spend_timing = c("begin","end"),
                  ruin_rule    = c("inclusive","strict")) {
  
  spend_timing <- match.arg(spend_timing)
  ruin_rule    <- match.arg(ruin_rule)
  
  stopifnot(is.numeric(wealth0), is.numeric(min_years), is.numeric(prob_ok),
            is.numeric(mu_real), is.numeric(vol_real))
  if (!is.null(seed)) set.seed(as.integer(seed))
  
  nper      <- as.integer(nper)
  min_years <- max(1L, min(as.integer(min_years), nper))
  prob_ok   <- min(max(as.numeric(prob_ok), 0), 1)
  
  # Lognormal gross returns with E[G] = 1 + mu_real
  m <- log1p(mu_real) - 0.5 * vol_real^2
  Z <- matrix(rnorm(n_scen * nper), nrow = n_scen, ncol = nper)
  G <- exp(m + vol_real * Z)
  
  # Cumulative products C_t and S_t = sum_{k=1..t} 1/C_k
  C <- matrix(0.0, nrow = n_scen, ncol = nper)
  S <- matrix(0.0, nrow = n_scen, ncol = nper)
  C[,1] <- G[,1]
  S[,1] <- 1 / C[,1]
  if (nper >= 2L) {
    for (t in 2:nper) {
      C[,t] <- C[,t-1] * G[,t]
      S[,t] <- S[,t-1] + 1 / C[,t]
    }
  }
  
  # For begin-of-period spending: T_t = 1 + S_{t-1} with S_0 := 0
  T <- cbind(1, 1 + S[, -ncol(S), drop = FALSE])  # n_scen x nper
  
  # index for binding constraint
  idx_inclusive <- function(N) max(1L, N - 1L)
  t_star <-
    if (ruin_rule == "inclusive") idx_inclusive(min_years) else min_years
  
  # pathwise thresholds (s must be below this to satisfy the rule)
  s_max_path <-
    if (spend_timing == "end")  wealth0 / S[, t_star]
  else                        wealth0 / T[, t_star]
  
  # *** Correct tail: need F^{-1}(1 - prob_ok) ***
  spend_opt <- as.numeric(
    stats::quantile(s_max_path, probs = 1 - prob_ok, names = FALSE, type = 7, na.rm = TRUE)
  )
  if (!is.finite(spend_opt)) spend_opt <- 0
  
  # Example paths & KPIs at s*
  show_idx <- seq_len(min(n_show, n_scen))
  C_show <- C[show_idx,, drop = FALSE]
  S_show <- S[show_idx,, drop = FALSE]
  T_show <- T[show_idx,, drop = FALSE]
  
  W_show <-
    if (spend_timing == "end")  C_show * (wealth0 - spend_opt * S_show)
  else                        C_show * (wealth0 - spend_opt * T_show)
  W_show[W_show < 0] <- 0  # keep matrix dims
  
  ruin_show <- apply(W_show, 1L, function(x) { k <- which(x <= 0); if (length(k)) k[1] else NA_integer_ })
  years  <- sort(unique(ruin_show[!is.na(ruin_show)]))
  counts <- if (length(years)) sapply(years, function(y) sum(ruin_show <= y, na.rm = TRUE)) else integer(0)
  
  list(
    ok = TRUE,
    inputs = list(
      wealth0=wealth0, min_years=min_years, prob_ok=prob_ok, mu_real=mu_real, vol_real=vol_real,
      nper=nper, n_scen=n_scen, n_show=n_show, spend_timing=spend_timing, ruin_rule=ruin_rule
    ),
    results = list(
      t = 1:nper,
      spend_opt = spend_opt,
      paths = W_show,
      ruin_years_show = ruin_show,
      table_years = years,
      table_cum_ruins = counts
    )
  )
}
