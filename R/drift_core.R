# Internal engine for decision-drift auditing (aigov_drift / aigov_monitor)
#
# Notation used throughout:
#   D  decision (binary or categorical), X recorded case characteristics,
#   G  group, J  decision unit (recruiter, office, judge, court), t period.
#   m_t(x, g)    = P_t(D = k | X = x, G = g)          (system-level rule)
#   m_t(x, g, j) = P_t(D = k | X = x, G = g, J = j)   (unit-level rule)
#
# Single-level decomposition (within a group subset S_g):
#   raw = mean_{S1} m1 - mean_{S0} m0
#       = mean_{S1}[m1 - m0]                (rule change at current case mix)
#       + mean_{S1} m0 - mean_{S0} m0       (case mix)
#
# Multilevel decomposition (unit-level models mu_t, system model ms_0):
#   raw = mean_{S1}[mu1 - mu0]              (within-unit rule change)
#       + mean_{S1} mu0 - mean_{S1} ms0     (allocation of cases to units)
#       + mean_{S1} ms0 - mean_{S0} ms0     (case mix)
#
# Because every model contains an intercept and group main effects (and unit
# main effects for mu_t), maximum-likelihood score equations give
# mean_{S_tg} fitted = observed rate, so the components add to the raw change
# exactly in the analysis sample.

# --------------------------------------------------------------------------- #
# column-name helper                                                          #
# --------------------------------------------------------------------------- #

.as_colname <- function(q, arg) {
  if (rlang::quo_is_null(q)) return(NULL)
  ex <- rlang::quo_get_expr(q)
  if (is.character(ex)) {
    if (length(ex) != 1L) cli::cli_abort("{.arg {arg}} must be a single column name.")
    return(ex)
  }
  if (is.symbol(ex)) return(rlang::as_string(ex))
  cli::cli_abort("{.arg {arg}} must be an unquoted column name or a string.")
}

# --------------------------------------------------------------------------- #
# model fitting                                                               #
# --------------------------------------------------------------------------- #

.bq <- function(x) paste0("`", x, "`")

.rhs_formula <- function(covariates, group_col, unit_col = NULL, extra = NULL) {
  terms <- c(.bq(covariates), extra, .bq(group_col))
  if (!is.null(unit_col)) terms <- c(terms, .bq(unit_col))
  stats::reformulate(terms, response = ".y")
}

# Fit P(D = k | rhs) for all categories. Returns a predictor that gives an
# n x K probability matrix with columns in `levels` order.
.fit_decision <- function(df, form, levels) {
  K <- length(levels)
  df <- droplevels(df)
  present <- unique(as.character(df$.y))
  if (!all(levels %in% present)) {
    stop("decision category absent in a period: ",
         paste(setdiff(levels, present), collapse = ", "))
  }
  n_sep <- 0L
  muffle_sep <- function(w) {
    if (grepl("fitted probabilities numerically 0 or 1|did not converge",
              conditionMessage(w))) {
      n_sep <<- n_sep + 1L
      invokeRestart("muffleWarning")
    }
  }
  if (K == 2L) {
    df$.y <- as.integer(df$.y == levels[2])
    fit <- withCallingHandlers(
      stats::glm(form, data = df, family = stats::binomial()),
      warning = muffle_sep
    )
    pred <- function(newdata) {
      p <- stats::predict(fit, newdata = newdata, type = "response")
      m <- cbind(1 - p, p)
      colnames(m) <- levels
      m
    }
  } else {
    df$.y <- factor(df$.y, levels = levels)
    fit <- nnet::multinom(form, data = df, trace = FALSE,
                          MaxNWts = 100000L, maxit = 1000L, reltol = 1e-12)
    pred <- function(newdata) {
      m <- stats::predict(fit, newdata = newdata, type = "probs")
      if (is.null(dim(m))) m <- matrix(m, nrow = 1L)
      colnames(m) <- levels
      m
    }
  }
  list(fit = fit, predict = pred, separation = n_sep > 0L)
}

# --------------------------------------------------------------------------- #
# overlap                                                                     #
# --------------------------------------------------------------------------- #

.overlap_keep <- function(df, covariates, group_col, trim, extra = NULL) {
  if (trim <= 0) return(rep(TRUE, nrow(df)))
  df$.per1 <- as.integer(df$.period == "current")
  form <- stats::reformulate(c(.bq(covariates), extra, .bq(group_col)), response = ".per1")
  fit <- suppressWarnings(stats::glm(form, data = df, family = stats::binomial()))
  e <- stats::fitted(fit)
  e >= trim & e <= 1 - trim
}

# --------------------------------------------------------------------------- #
# one evaluation of all group-level statistics                                #
# --------------------------------------------------------------------------- #

# df: analysis sample with columns .y (character), .period ("baseline"/
#     "current"), covariates, group, optional unit.
# Returns a named numeric vector of statistics, plus attributes describing them.
.drift_stats <- function(df, covariates, group_col, unit_col, levels, focal,
                         reference, group_levels, extra = NULL) {
  s0 <- df$.period == "baseline"
  s1 <- !s0
  d0 <- df[s0, , drop = FALSE]
  d1 <- df[s1, , drop = FALSE]

  f_sys <- .rhs_formula(covariates, group_col, extra = extra)
  ms0 <- .fit_decision(d0, f_sys, levels)
  ms1 <- .fit_decision(d1, f_sys, levels)

  multilevel <- !is.null(unit_col)
  if (multilevel) {
    f_unit <- .rhs_formula(covariates, group_col, unit_col, extra = extra)
    mu0 <- .fit_decision(d0, f_unit, levels)
    mu1 <- .fit_decision(d1, f_unit, levels)
  }

  # predictions on each period's own rows and on the pooled rows
  P_ms0_all <- ms0$predict(df)
  P_ms1_all <- ms1$predict(df)
  if (multilevel) {
    P_mu0_d1 <- mu0$predict(d1)
    P_mu1_d1 <- mu1$predict(d1)
  }

  Y <- vapply(levels, function(k) as.numeric(df$.y == k), numeric(nrow(df)))
  if (is.null(dim(Y))) Y <- matrix(Y, nrow = 1L, dimnames = list(NULL, levels))

  subsets <- c("Overall", group_levels)
  out <- list()
  for (g in subsets) {
    in_g <- if (g == "Overall") rep(TRUE, nrow(df)) else as.character(df[[group_col]]) == g
    g0 <- in_g & s0
    g1 <- in_g & s1
    if (sum(g0) == 0L || sum(g1) == 0L) next
    g1_in_d1 <- in_g[s1]

    rate0 <- colMeans(Y[g0, , drop = FALSE])
    rate1 <- colMeans(Y[g1, , drop = FALSE])
    raw   <- rate1 - rate0

    m0_g0 <- colMeans(P_ms0_all[g0, , drop = FALSE])
    m0_g1 <- colMeans(P_ms0_all[g1, , drop = FALSE])
    case_mix <- m0_g1 - m0_g0

    if (multilevel) {
      mu0_g1 <- colMeans(P_mu0_d1[g1_in_d1, , drop = FALSE])
      mu1_g1 <- colMeans(P_mu1_d1[g1_in_d1, , drop = FALSE])
      allocation <- mu0_g1 - m0_g1
      rule <- mu1_g1 - mu0_g1
    } else {
      allocation <- rep(NA_real_, length(levels))
      rule <- colMeans(P_ms1_all[g1, , drop = FALSE]) - m0_g1
    }

    q_rows <- switch(reference,
      pooled   = in_g,
      baseline = g0,
      current  = g1
    )
    diffQ <- P_ms1_all[q_rows, , drop = FALSE] - P_ms0_all[q_rows, , drop = FALSE]
    net   <- colMeans(diffQ)
    absd  <- mean(0.5 * rowSums(abs(diffQ)))

    for (k in levels) {
      key <- paste(g, k, sep = "||")
      out[[paste0("raw||", key)]]        <- raw[[k]]
      out[[paste0("case_mix||", key)]]   <- case_mix[[k]]
      out[[paste0("allocation||", key)]] <- allocation[[match(k, levels)]]
      out[[paste0("rule||", key)]]       <- rule[[k]]
      out[[paste0("net||", key)]]        <- net[[k]]
      out[[paste0("rate0||", key)]]      <- rate0[[k]]
      out[[paste0("rate1||", key)]]      <- rate1[[k]]
    }
    out[[paste0("abs||", g, "||ALL")]] <- absd
  }
  v <- unlist(out)
  attr(v, "separation") <- ms0$separation || ms1$separation ||
    (multilevel && (mu0$separation || mu1$separation))
  v
}

# --------------------------------------------------------------------------- #
# bootstrap                                                                   #
# --------------------------------------------------------------------------- #

.resample <- function(df, cluster_col) {
  if (!is.null(cluster_col)) {
    cl <- unique(df[[cluster_col]])
    pick <- sample(cl, length(cl), replace = TRUE)
    idx <- split(seq_len(nrow(df)), df[[cluster_col]])
    rows <- unlist(idx[as.character(pick)], use.names = FALSE)
  } else {
    rows <- unlist(lapply(split(seq_len(nrow(df)), df$.period),
                          function(r) r[sample.int(length(r), length(r), replace = TRUE)]),
                   use.names = FALSE)
  }
  df[rows, , drop = FALSE]
}

.bootstrap_stats <- function(df, B, cluster_col, stat_fun, names_ref) {
  if (B <= 0L) return(NULL)
  mat <- matrix(NA_real_, nrow = B, ncol = length(names_ref),
                dimnames = list(NULL, names_ref))
  for (b in seq_len(B)) {
    db <- .resample(df, cluster_col)
    v <- tryCatch(stat_fun(db), error = function(e) NULL)
    if (!is.null(v)) {
      common <- intersect(names(v), names_ref)
      mat[b, common] <- v[common]
    }
  }
  mat
}

# --------------------------------------------------------------------------- #
# unit-level drift (delta method, noise-corrected heterogeneity, EB)          #
# --------------------------------------------------------------------------- #

.unit_drift <- function(df, covariates, group_col, unit_col, focal,
                        max_ref, tolerance, alpha, extra = NULL) {
  df$.yb <- as.integer(df$.y == focal)
  form <- stats::reformulate(c(.bq(covariates), extra, .bq(group_col), .bq(unit_col)),
                             response = ".yb")
  fits <- lapply(c("baseline", "current"), function(p) {
    suppressWarnings(stats::glm(form, data = df[df$.period == p, , drop = FALSE],
                                family = stats::binomial()))
  })
  names(fits) <- c("baseline", "current")

  # reference case mix: pooled analysis sample (subsampled for speed)
  Q <- df
  if (nrow(Q) > max_ref) Q <- Q[sample.int(nrow(Q), max_ref), , drop = FALSE]
  units <- levels(df[[unit_col]])
  Q[[unit_col]] <- factor(units[1], levels = units)  # reference level: all dummies 0

  parts <- lapply(fits, function(fit) {
    tt <- stats::delete.response(stats::terms(fit))
    X  <- stats::model.matrix(tt, Q, xlev = fit$xlevels)
    b  <- stats::coef(fit)
    keep <- !is.na(b)
    asg <- attr(X, "assign")
    unit_term <- match(.bq(unit_col), attr(tt, "term.labels"))
    if (is.na(unit_term)) unit_term <- match(unit_col, attr(tt, "term.labels"))
    unit_cols <- which(asg == unit_term)
    list(X = X, b = b, keep = keep, unit_cols = unit_cols,
         V = stats::vcov(fit, complete = FALSE))
  })

  res <- lapply(units, function(j) {
    lev_idx <- match(j, units)
    vals <- lapply(parts, function(pt) {
      X <- pt$X
      if (lev_idx > 1L) X[, pt$unit_cols[lev_idx - 1L]] <- 1
      Xk <- X[, pt$keep, drop = FALSE]
      eta <- drop(Xk %*% pt$b[pt$keep])
      p <- stats::plogis(eta)
      grad <- colMeans(Xk * (p * (1 - p)))
      list(mean = mean(p), var = drop(t(grad) %*% pt$V %*% grad))
    })
    c(drift = vals$current$mean - vals$baseline$mean,
      se = sqrt(vals$current$var + vals$baseline$var))
  })
  res <- do.call(rbind, res)

  n0 <- as.integer(table(factor(df[[unit_col]][df$.period == "baseline"], levels = units)))
  n1 <- as.integer(table(factor(df[[unit_col]][df$.period == "current"],  levels = units)))

  est <- res[, "drift"]
  se  <- res[, "se"]
  z   <- est / se
  p   <- 2 * stats::pnorm(-abs(z))
  q   <- stats::p.adjust(p, method = "BH")

  tau2 <- max(0, stats::var(est) - mean(se^2))
  mu   <- mean(est)
  shrink <- if (tau2 > 0) tau2 / (tau2 + se^2) else rep(0, length(se))
  eb   <- mu + shrink * (est - mu)

  tab <- tibble::tibble(
    unit = units, n_baseline = n0, n_current = n1,
    drift = est, se = se, z = z, p_value = p, q_value = q,
    drift_shrunk = eb,
    flag = q < alpha & abs(eb) > tolerance
  )
  summ <- list(
    n_units = length(units),
    mean_drift = mu,
    sd_drift_raw = stats::sd(est),
    tau2 = tau2,
    tau = sqrt(tau2),
    reliability = if (tau2 > 0) tau2 / (tau2 + mean(se^2)) else 0,
    mean_abs_drift_shrunk = mean(abs(eb)),
    share_beyond_tolerance = mean(abs(eb) > tolerance),
    n_flagged = sum(tab$flag)
  )
  list(table = tab, summary = summ)
}

# --------------------------------------------------------------------------- #
# full engine                                                                 #
# --------------------------------------------------------------------------- #

.drift_engine <- function(data, decision_col, group_col, ref_group, period_col,
                          baseline, current, covariates, unit_col, cluster_col,
                          reference, overlap_trim, tolerance, focal, B,
                          conf_level, min_unit_n, max_ref, extra_terms = NULL) {

  df <- data[as.character(data[[period_col]]) %in% c(baseline, current), , drop = FALSE]
  needed <- c(decision_col, group_col, period_col, covariates, unit_col, cluster_col)
  cc <- stats::complete.cases(df[, unique(needed), drop = FALSE])
  n_missing <- sum(!cc)
  df <- df[cc, , drop = FALSE]

  df$.y <- as.character(df[[decision_col]])
  df$.period <- ifelse(as.character(df[[period_col]]) == baseline, "baseline", "current")

  levels <- if (is.factor(data[[decision_col]])) {
    intersect(levels(data[[decision_col]]), unique(df$.y))
  } else {
    sort(unique(df$.y))
  }
  if (length(levels) < 2L) cli::cli_abort("The decision has fewer than two observed categories.")
  if (is.null(focal)) focal <- levels[length(levels)]
  focal <- as.character(focal)
  if (!focal %in% levels) cli::cli_abort("{.arg focal} = {.val {focal}} is not a decision category.")

  group_levels <- unique(c(ref_group, sort(unique(as.character(df[[group_col]])))))
  df[[group_col]] <- factor(as.character(df[[group_col]]), levels = group_levels)

  # units: keep units observed in both periods with enough cases
  units_dropped <- character(0)
  if (!is.null(unit_col)) {
    u <- as.character(df[[unit_col]])
    tab <- table(u, df$.period)
    if (!all(c("baseline", "current") %in% colnames(tab))) {
      cli::cli_abort("Both periods must contain decisions.")
    }
    ok <- rownames(tab)[tab[, "baseline"] >= min_unit_n & tab[, "current"] >= min_unit_n]
    units_dropped <- setdiff(rownames(tab), ok)
    df <- df[u %in% ok, , drop = FALSE]
    if (length(ok) < 2L) {
      cli::cli_abort("Fewer than two units have at least {min_unit_n} decisions in both periods.")
    }
    df[[unit_col]] <- factor(as.character(df[[unit_col]]), levels = sort(ok))
  }

  # overlap
  keep <- .overlap_keep(df, covariates, group_col, overlap_trim, extra_terms)
  overlap <- tibble::tibble(
    group = c("Overall", group_levels),
    n_total = c(nrow(df), as.integer(table(df[[group_col]]))),
    n_kept  = c(sum(keep), as.integer(table(df[[group_col]][keep]))),
  )
  overlap$share_trimmed <- 1 - overlap$n_kept / overlap$n_total
  df <- df[keep, , drop = FALSE]

  if (is.null(cluster_col) && !is.null(unit_col)) cluster_col <- unit_col

  stat_fun <- function(d) .drift_stats(d, covariates, group_col, unit_col,
                                       levels, focal, reference, group_levels,
                                       extra_terms)
  est <- stat_fun(df)
  separation <- isTRUE(attr(est, "separation"))
  boot <- .bootstrap_stats(df, B, cluster_col, stat_fun, names(est))

  alpha <- 1 - conf_level
  get_ci <- function(nm) {
    if (is.null(boot)) return(c(NA_real_, NA_real_, NA_real_, NA_real_))
    x <- boot[, nm]
    x <- x[is.finite(x)]
    if (length(x) < 10L) return(c(NA_real_, NA_real_, NA_real_, NA_real_))
    c(stats::sd(x), stats::quantile(x, c(alpha / 2, 1 - alpha / 2), names = FALSE), mean(x))
  }
  n_boot_ok <- if (is.null(boot)) 0L else sum(stats::complete.cases(boot))

  # ---- decomposition table --------------------------------------------------
  parse <- do.call(rbind, strsplit(names(est), "||", fixed = TRUE))
  colnames(parse) <- c("stat", "group", "category")
  keys <- unique(parse[parse[, "stat"] == "raw", c("group", "category"), drop = FALSE])
  decomp <- lapply(seq_len(nrow(keys)), function(i) {
    g <- keys[i, "group"]; k <- keys[i, "category"]
    row <- list(group = g, category = k)
    for (s in c("rate0", "rate1", "raw", "case_mix", "allocation", "rule", "net")) {
      nm <- paste(s, g, k, sep = "||")
      row[[s]] <- unname(est[nm])
      if (s %in% c("raw", "case_mix", "allocation", "rule", "net")) {
        ci <- if (is.na(est[nm])) rep(NA_real_, 4) else get_ci(nm)
        row[[paste0(s, "_lo")]] <- ci[2]
        row[[paste0(s, "_hi")]] <- ci[3]
      }
    }
    tibble::as_tibble(row)
  })
  decomp <- do.call(rbind, decomp)
  names(decomp)[names(decomp) == "rate0"] <- "rate_baseline"
  names(decomp)[names(decomp) == "rate1"] <- "rate_current"
  names(decomp)[names(decomp) == "raw"] <- "raw_change"
  names(decomp)[names(decomp) == "rule"] <- "rule_change"
  names(decomp)[names(decomp) == "net"] <- "net_drift"
  if (is.null(unit_col)) {
    decomp <- decomp[, !grepl("^allocation", names(decomp))]
  }

  # ---- drift summary table (focal category + absolute drift) ---------------
  grp_out <- intersect(c("Overall", group_levels), keys[, "group"])
  drift_tab <- lapply(grp_out, function(g) {
    nm_net <- paste("net", g, focal, sep = "||")
    nm_abs <- paste("abs", g, "ALL", sep = "||")
    ci_net <- get_ci(nm_net)
    ci_abs <- get_ci(nm_abs)
    a_hat <- unname(est[nm_abs])
    a_bc  <- if (is.na(ci_abs[4])) NA_real_ else max(0, 2 * a_hat - ci_abs[4])
    z <- unname(est[nm_net]) / ci_net[1]
    tibble::tibble(
      group = g,
      net_drift = unname(est[nm_net]),
      net_se = ci_net[1], net_lo = ci_net[2], net_hi = ci_net[3],
      net_p = if (is.finite(z)) 2 * stats::pnorm(-abs(z)) else NA_real_,
      net_p_tol = if (is.finite(ci_net[1]) && ci_net[1] > 0) {
        stats::pnorm((tolerance - abs(unname(est[nm_net]))) / ci_net[1])
      } else NA_real_,
      abs_drift = a_hat, abs_drift_bc = a_bc,
      abs_lo = ci_abs[2], abs_hi = ci_abs[3]
    )
  })
  drift_tab <- do.call(rbind, drift_tab)

  drift_tab$alert <- vapply(seq_len(nrow(drift_tab)), function(i) {
    r <- drift_tab[i, ]
    # interval-null rule: the whole confidence interval must lie beyond the
    # practical tolerance, so changes within tolerance never raise an alert
    net_beyond <- isTRUE(r$net_lo > tolerance || r$net_hi < -tolerance)
    if (net_beyond) return("net drift")
    if (isTRUE(r$abs_lo > tolerance) && abs(r$net_drift) <= tolerance) return("masked drift")
    if (isTRUE(r$abs_lo > tolerance)) return("conditional drift")
    "none"
  }, character(1))

  # ---- units ------------------------------------------------------------------
  units <- NULL
  if (!is.null(unit_col)) {
    units <- .unit_drift(df, covariates, group_col, unit_col, focal,
                         max_ref, tolerance, alpha, extra_terms)
  }

  list(
    decomposition = decomp,
    drift = drift_tab,
    overlap = overlap,
    units = units,
    settings = list(
      decision = decision_col, levels = levels, focal = focal,
      period = period_col, baseline = baseline, current = current,
      covariates = covariates, extra_terms = extra_terms,
      unit = unit_col, cluster = cluster_col,
      reference = reference, overlap_trim = overlap_trim,
      tolerance = tolerance, B = B, n_boot_ok = n_boot_ok,
      conf_level = conf_level, min_unit_n = min_unit_n,
      units_dropped = units_dropped, n_missing = n_missing,
      n_analysis = nrow(df), separation = separation
    )
  )
}
