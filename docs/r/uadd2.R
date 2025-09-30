# uadd2_analytic: required yearly savings consistent with original package
# - Begin-of-year deposits (annuity-due)
# - Log-returns eps ~ N(mu_log, sigma_log), G = exp(eps)
# - A = W*/Q_{1-p}(H) with H = sum_{j=0}^{T-1} C_j

uadd2 <- function(pension_min, nper, mu_log, sigma_log, conv, prob_ok,
                  n_scen = 1000L, n_show = 10L, seed = NULL) {
  stopifnot(is.numeric(pension_min), is.numeric(nper),
            is.numeric(mu_log), is.numeric(sigma_log),
            is.numeric(conv), is.numeric(prob_ok))
  if (!is.null(seed)) set.seed(as.integer(seed))
  nper    <- as.integer(nper)
  n_scen  <- as.integer(n_scen)
  prob_ok <- max(0, min(1, prob_ok))
  
  # eps ~ N(mu_log, sigma_log)  -> cumulative products C_j = exp(sum eps_1..eps_j)
  if (nper == 1L) {
    H_unit <- rep(1, n_scen)                 # sum_{j=0}^{0} C_j = 1
    M_show <- matrix(1, nrow = min(n_show, n_scen), ncol = 1)
  } else {
    eps   <- matrix(rnorm(n_scen * nper, mean = mu_log, sd = sigma_log), nrow = n_scen)
    Ccum  <- t(apply(eps[, 1:(nper - 1), drop = FALSE], 1L, cumsum))  # size: n_scen x (T-1)
    C     <- exp(Ccum)                                                # C_1..C_{T-1}
    S     <- t(apply(C, 1L, cumsum))                                  # S_{t-1} = Σ C_k
    M     <- cbind(1, 1 + S)                                          # wealth factors per unit A
    H_unit <- M[, nper]                                               # = sum_{j=0}^{T-1} C_j
    M_show <- M[seq_len(min(n_show, n_scen)), , drop = FALSE]
  }
  
  W_star <- pension_min / conv
  
  # diskretes (1-p)-Quantil (Orderstatistik, konsistent zum Zählkriterium im Paket)
  k  <- max(1L, min(n_scen, ceiling((1 - prob_ok) * n_scen)))
  Hq <- sort(H_unit, partial = k)[k]
  
  A_req <- if (is.finite(Hq) && Hq > 0) W_star / Hq else Inf
  
  list(
    ok = TRUE,
    inputs = list(
      pension_min = pension_min, nper = nper,
      mu_log = mu_log, sigma_log = sigma_log, conv = conv, prob_ok = prob_ok,
      n_scen = n_scen, n_show = n_show, seed = seed
    ),
    results = list(
      t = 1:nper,
      pmt_required = A_req,
      paths = M_show * A_req,                  # wealth paths per sample
      terminal_wealth  = H_unit * A_req,       # for the table
      terminal_pension = H_unit * A_req * conv
    )
  )
}
