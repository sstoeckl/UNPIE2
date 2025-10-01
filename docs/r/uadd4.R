# uadd4: smooth to deterministic boundary (EOY spending, inclusive rule)
uadd4 <- function(wealth0, min_years, prob_ok, mu_real, vol_real,
                  nper = 25L, n_scen = 10000L, n_show = 10L, seed = NULL,
                  ruin_rule = c("inclusive","strict")) {
  
  ruin_rule <- match.arg(ruin_rule)
  stopifnot(is.numeric(wealth0), is.numeric(min_years), is.numeric(prob_ok),
            is.numeric(mu_real), is.numeric(vol_real))
  if (!is.null(seed)) set.seed(as.integer(seed))
  
  nper      <- as.integer(nper)
  min_years <- max(1L, min(as.integer(min_years), nper))
  prob_ok   <- min(max(as.numeric(prob_ok), 0), 1)
  t_star    <- if (ruin_rule == "inclusive") max(1L, min_years - 1L) else min_years
  
  # --- deterministischer Grenzfall: mu = 0, sigma = 0 ------------------------
  eps <- 1e-12
  if (abs(mu_real) <= eps && vol_real <= eps) {
    s <- wealth0 / if (ruin_rule == "inclusive") (min_years - 1L) else min_years
    
    # EOY-Wealth nach j Jahren: W_eoy[j] = W0 - s*j  (j = 1..nper)
    W_eoy <- matrix(wealth0 - s * (1:nper), nrow = 1)
    W_eoy[W_eoy < 0] <- 0
    
    # Anzeige: erster Punkt ist Startvermögen (shift um 1 nach rechts)
    if (nper > 1L) {
      W_disp <- cbind(wealth0, W_eoy[, 1:(nper-1), drop = FALSE])
    } else {
      W_disp <- matrix(wealth0, nrow = 1, ncol = 1)
    }
    
    ruin_show <- which(W_eoy[1, ] <= 0)
    ruin_show <- if (length(ruin_show)) ruin_show[1] else NA_integer_
    years  <- if (is.na(ruin_show)) integer(0) else ruin_show
    counts <- if (length(years)) 1L else integer(0)
    
    return(list(
      ok = TRUE,
      inputs = list(wealth0=wealth0, min_years=min_years, prob_ok=prob_ok,
                    mu_real=mu_real, vol_real=vol_real,
                    nper=nper, n_scen=1L, n_show=1L,
                    model="EOY-inclusive (deterministic)"),
      results = list(
        t = 1:nper,
        spend_opt = s,
        paths = W_disp,
        ruin_years_show = ruin_show,
        table_years = years,
        table_cum_ruins = counts
      )
    ))
  }
  
  # --- stochastischer Fall: lognormal mit E[G] = 1 + mu_real -----------------
  m <- log1p(mu_real) - 0.5 * vol_real^2
  Z <- matrix(rnorm(n_scen * nper), nrow = n_scen, ncol = nper)
  G <- exp(m + vol_real * Z)
  
  # kumulative Produkte C_t und S_t = sum_{k=1..t} 1/C_k (EOY-Spending)
  C <- matrix(0.0, nrow = n_scen, ncol = nper)
  S <- matrix(0.0, nrow = n_scen, ncol = nper)
  C[, 1] <- G[, 1]
  S[, 1] <- 1 / C[, 1]
  if (nper >= 2L) {
    for (t in 2:nper) {
      C[, t] <- C[, t-1] * G[, t]
      S[, t] <- S[, t-1] + 1 / C[, t]
    }
  }
  
  # Pfadweise Schwelle (EOY): W_t = C_t * (W0 - s * S_t)  =>  s <= W0 / S_t
  s_max_path <- wealth0 / S[, t_star]
  
  # diskrete Wahrscheinlichkeit wie im Paket „fixLimit“
  prob_adj <- floor(prob_ok * n_scen) / n_scen
  
  spend_opt <- as.numeric(
    stats::quantile(s_max_path, probs = 1 - prob_adj, names = FALSE,
                    type = 7, na.rm = TRUE)
  )
  if (!is.finite(spend_opt)) spend_opt <- 0
  
  # Beispielpfade bei s*: EOY-Werte …
  show_idx <- seq_len(min(n_show, n_scen))
  C_show <- C[show_idx,, drop = FALSE]
  S_show <- S[show_idx,, drop = FALSE]
  W_eoy_show <- C_show * (wealth0 - spend_opt * S_show)
  W_eoy_show[W_eoy_show < 0] <- 0
  
  # … und Anzeige geankert auf Startvermögen (shift um 1)
  if (nper > 1L) {
    W_disp <- cbind(wealth0, W_eoy_show[, 1:(nper-1), drop = FALSE])
  } else {
    W_disp <- matrix(wealth0, nrow = nrow(W_eoy_show), ncol = 1)
  }
  
  # Ruinzeit aus den EOY-Werten (erste Stelle <= 0)
  ruin_show <- apply(W_eoy_show, 1L, function(x) {
    k <- which(x <= 0); if (length(k)) k[1] else NA_integer_
  })
  years  <- sort(unique(ruin_show[!is.na(ruin_show)]))
  counts <- if (length(years)) sapply(years, function(y) sum(ruin_show <= y, na.rm = TRUE)) else integer(0)
  
  list(
    ok = TRUE,
    inputs = list(
      wealth0=wealth0, min_years=min_years, prob_ok=prob_ok,
      mu_real=mu_real, vol_real=vol_real,
      nper=nper, n_scen=n_scen, n_show=n_show,
      model="EOY-inclusive (lognormal)"
    ),
    results = list(
      t = 1:nper,
      spend_opt = spend_opt,
      paths = W_disp,
      ruin_years_show = ruin_show,
      table_years = years,
      table_cum_ruins = counts
    )
  )
}
