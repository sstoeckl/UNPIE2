# add2: Real yearly savings required to reach a minimum desired (real)
#       yearly pension with probability p
# Modell (wie uadd1/uadd4):
#   End-of-year Beiträge (real): W_t = W_{t-1} * G_t + pmt_real, W_0=0
#   G_t = exp(m + vol_real * Z_t), Z_t ~ N(0,1), m = log(1+mu_real) - 0.5*vol_real^2
#   => W_T = pmt_real * H,  H = C_T * S_T,
#      C_t = prod_{k=1..t} G_k,  S_t = sum_{k=1..t} 1/C_k
# Zielbedingung: P(conv * W_T >= pension_min) >= prob_ok
#   <=> pmt_real >= (pension_min/conv) / Q_{1-prob_ok}(H)

uadd2 <- function(pension_min, nper, mu_real, vol_real, conv, prob_ok,
                  n_scen = 10000L, n_show = 10L, seed = NULL) {
  stopifnot(is.numeric(pension_min), is.numeric(nper),
            is.numeric(mu_real), is.numeric(vol_real),
            is.numeric(conv), is.numeric(prob_ok))
  if (!is.null(seed)) set.seed(as.integer(seed))
  
  nper    <- as.integer(nper)
  prob_ok <- min(max(as.numeric(prob_ok), 0), 1)
  
  # lognormal mit E[G]=1+mu_real
  m <- log1p(mu_real) - 0.5 * vol_real^2
  Z <- matrix(rnorm(n_scen * nper), nrow = n_scen, ncol = nper)
  G <- exp(m + vol_real * Z)
  
  # kumulative Produkte/Summen
  C <- matrix(0.0, nrow = n_scen, ncol = nper)
  S <- matrix(0.0, nrow = n_scen, ncol = nper)
  C[, 1] <- G[, 1]
  S[, 1] <- 1 / C[, 1]
  if (nper >= 2L) {
    for (t in 2:nper) {
      C[, t] <- C[, t - 1] * G[, t]
      S[, t] <- S[, t - 1] + 1 / C[, t]
    }
  }
  
  H <- C[, nper] * S[, nper]
  W_star <- pension_min / conv
  
  qH <- as.numeric(stats::quantile(H, probs = 1 - prob_ok, names = FALSE, type = 7, na.rm = TRUE))
  pmt_req <- if (is.finite(qH) && qH > 0) W_star / qH else Inf
  
  # Beispielpfade (Wealth) bei pmt_req
  show_idx <- seq_len(min(n_show, n_scen))
  C_show <- C[show_idx, , drop = FALSE]
  S_show <- S[show_idx, , drop = FALSE]
  W_show <- pmt_req * (C_show * S_show)
  
  probs <- seq(0.1, 0.9, by = 0.1)
  qH_vec <- as.numeric(stats::quantile(H, probs = probs, names = FALSE, type = 7, na.rm = TRUE))
  
  list(
    ok = TRUE,
    inputs = list(
      pension_min = pension_min, nper = nper,
      mu_real = mu_real, vol_real = vol_real, conv = conv, prob_ok = prob_ok,
      n_scen = n_scen, n_show = n_show
    ),
    results = list(
      t = 1:nper,
      pmt_required = pmt_req,
      paths = W_show,         # n_show × nper (Wealth-Pfade)
      H_quantiles = qH_vec,   # 10%..90% (Diagnostik)
      probs = probs
    )
  )
}
