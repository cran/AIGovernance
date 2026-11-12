#' Simulate Decision Data With Known Drift Mechanisms
#'
#' @description
#' Generates multi-period decision records from a known data-generating
#' process so that the targets of \code{\link{aigov_drift}} are known exactly.
#' Used for testing, teaching and simulation studies of audit procedures.
#'
#' Cases have a continuous \code{qualification} score, a binary
#' \code{degree}, a group (\code{"A"}, \code{"B"}, \code{"C"}) and an
#' unrecorded binary characteristic \eqn{z}. Each case is routed to one of
#' \code{n_units} decision units (\code{recruiter}). The decision follows a
#' logistic (binary) or cumulative-logistic (three-category) model:
#' \deqn{\eta = -0.2 + qualification + 0.6\,degree - z + \gamma_g + \alpha_j
#'   + v_j s_t + \delta_t(x, g),}
#' where \eqn{s_t} rises from 0 in the first period to 1 in the last.
#'
#' Mechanisms (\code{scenario}; several may be combined):
#' \describe{
#'   \item{\code{"stable"}}{Nothing changes.}
#'   \item{\code{"case_mix"}}{Mean qualification falls by 0.5 SD.}
#'   \item{\code{"rule"}}{All groups face a stricter rule (\eqn{\delta = -}\code{effect}).}
#'   \item{\code{"offsetting_groups"}}{Group B gains and group A loses by \code{effect}.}
#'   \item{\code{"offsetting_cases"}}{Within every group, cases with a degree
#'     gain and cases without lose by \code{effect}.}
#'   \item{\code{"unobserved"}}{The share of cases with the unrecorded \eqn{z}
#'     rises from 0.3 to 0.6; the full-information rule does not change.}
#'   \item{\code{"reallocation"}}{High-qualification cases are routed to more
#'     lenient units; no unit changes its rule.}
#'   \item{\code{"unit_offsetting"}}{Half of the units become more lenient and
#'     half stricter by \code{effect}; the average unit does not change.}
#'   \item{\code{"category_shift"}}{(Requires \code{categorical = TRUE}.) The
#'     threshold between "review" and "advance" rises by \code{effect} while the
#'     share passing the first screen is unchanged.}
#' }
#'
#' @param n Number of cases per period (default \code{2000}).
#' @param n_periods Number of periods (default \code{2}).
#' @param n_units Number of decision units (default \code{30}).
#' @param scenario Character vector of mechanisms (see Description).
#' @param effect Size of rule changes on the logit scale (default \code{0.8}).
#' @param categorical If \code{TRUE}, adds a three-category \code{decision}
#'   (reject / review / advance) and defines \code{selected} as passing the
#'   first screen (review or advance).
#' @param first_period Label of the first period (default \code{2024}).
#' @param seed Optional random seed.
#'
#' @return A data frame with columns \code{case_id}, \code{period},
#'   \code{group}, \code{qualification}, \code{degree}, \code{recruiter},
#'   \code{selected} and, if \code{categorical = TRUE}, \code{decision}. The
#'   attribute \code{"truth"} is a list with \code{drift} (true net and
#'   absolute drift of \code{selected}, by group, between the first and last
#'   period, over the pooled cases conditional on the recorded
#'   characteristics; with \code{categorical = TRUE} also the true net drift
#'   of "advance" and the true total-variation drift of \code{decision}), \code{decomposition} (true case-mix, allocation and
#'   within-unit rule-change components of the change in \code{selected},
#'   evaluated on the realized cases) and \code{units} (true unit drift over
#'   the pooled cases).
#'
#' @examples
#' sim <- aigov_simulate_drift(n = 500, scenario = "offsetting_cases", seed = 1)
#' attr(sim, "truth")$drift
#'
#' @export
aigov_simulate_drift <- function(n = 2000L,
                                 n_periods = 2L,
                                 n_units = 30L,
                                 scenario = "stable",
                                 effect = 0.8,
                                 categorical = FALSE,
                                 first_period = 2024L,
                                 seed = NULL) {
  valid <- c("stable", "case_mix", "rule", "offsetting_groups",
             "offsetting_cases", "unobserved", "reallocation",
             "unit_offsetting", "category_shift")
  bad <- setdiff(scenario, valid)
  if (length(bad)) cli::cli_abort("Unknown scenario(s): {.val {bad}}. Choose from {.val {valid}}.")
  if ("category_shift" %in% scenario && !categorical) {
    cli::cli_abort("{.val category_shift} requires {.code categorical = TRUE}.")
  }
  if (n_periods < 2L) cli::cli_abort("{.arg n_periods} must be at least 2.")
  if (n_units < 2L) cli::cli_abort("{.arg n_units} must be at least 2.")
  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(seed)
  }
  has <- function(s) s %in% scenario

  groups  <- c("A", "B", "C")
  g_prob  <- c(0.5, 0.3, 0.2)
  gamma   <- c(A = 0, B = -0.4, C = -0.2)
  alpha   <- stats::rnorm(n_units, 0, 0.6)
  alpha_s <- as.numeric(scale(alpha))
  v_sign  <- sample(rep(c(1, -1), length.out = n_units))
  v       <- if (has("unit_offsetting")) effect * v_sign else rep(0, n_units)
  c1 <- 1.0; c2 <- 1.0

  par <- function(t) {
    s <- (t - 1) / (n_periods - 1)
    list(
      s     = s,
      mu_q  = if (has("case_mix")) -0.5 * s else 0,
      pz    = if (has("unobserved")) 0.3 + 0.3 * s else 0.3,
      rho   = if (has("reallocation")) 1.5 * s else 0,
      shift = if (has("category_shift")) effect * s else 0
    )
  }

  delta <- function(s, g, degree) {
    d <- 0
    if (has("rule")) d <- d - effect * s
    if (has("offsetting_groups")) d <- d + effect * s * ((g == "B") - (g == "A"))
    if (has("offsetting_cases")) d <- d + effect * s * (2 * degree - 1)
    d
  }

  # routing probabilities n x J
  route <- function(q, rho) {
    lp <- outer(q, alpha_s) * rho
    lp <- lp - apply(lp, 1, max)
    w <- exp(lp)
    w / rowSums(w)
  }

  # P(selected) and category probabilities given everything
  probs <- function(eta, shift) {
    if (!categorical) {
      p <- stats::plogis(eta)
      return(list(pass = p, adv = p))
    }
    list(pass = stats::plogis(eta + c1), adv = stats::plogis(eta - c2 - shift))
  }

  # linear predictor without z
  eta0 <- function(q, deg, g, j, s) {
    -0.2 + q + 0.6 * deg + gamma[g] + alpha[j] + v[j] * s + delta(s, g, deg)
  }

  rows <- vector("list", n_periods)
  for (t in seq_len(n_periods)) {
    pr  <- par(t)
    g   <- sample(groups, n, replace = TRUE, prob = g_prob)
    q   <- stats::rnorm(n, pr$mu_q, 1)
    deg <- stats::rbinom(n, 1, 0.5)
    z   <- stats::rbinom(n, 1, pr$pz)
    R   <- route(q, pr$rho)
    u   <- stats::runif(n)
    j   <- pmin(rowSums(u > t(apply(R, 1, cumsum))) + 1L, n_units)
    eta <- eta0(q, deg, g, j, pr$s) - z
    if (categorical) {
      pp <- probs(eta, pr$shift)
      uu  <- stats::runif(n)
      dec <- ifelse(uu > pp$pass, "reject", ifelse(uu > pp$adv, "review", "advance"))
      sel <- as.integer(dec != "reject")
    } else {
      sel <- stats::rbinom(n, 1, stats::plogis(eta))
      dec <- NULL
    }
    df <- data.frame(
      period = first_period + t - 1L,
      group = g,
      qualification = round(q, 3),
      degree = deg,
      recruiter = sprintf("R%02d", j),
      selected = sel,
      stringsAsFactors = FALSE
    )
    if (categorical) {
      df$decision <- factor(dec, levels = c("reject", "review", "advance"))
    }
    rows[[t]] <- df
  }
  out <- do.call(rbind, rows)
  out <- cbind(case_id = seq_len(nrow(out)), out)

  # ---- truth: first vs last period over pooled recorded cases ----------------
  first <- out$period == first_period
  last  <- out$period == first_period + n_periods - 1L
  Q <- out[first | last, , drop = FALSE]
  sys_prob <- function(t) {
    pr <- par(t)
    R  <- route(Q$qualification, pr$rho)
    res <- matrix(0, nrow(Q), 3)  # reject, review, advance
    for (zz in 0:1) {
      wz <- if (zz == 1) pr$pz else 1 - pr$pz
      for (jj in seq_len(n_units)) {
        eta <- eta0(Q$qualification, Q$degree, Q$group, rep(jj, nrow(Q)), pr$s) - zz
        pp <- probs(eta, pr$shift)
        pa <- pp$adv; ps <- pp$pass
        res <- res + wz * R[, jj] * cbind(1 - ps, ps - pa, pa)
      }
    }
    res
  }
  P0 <- sys_prob(1)
  P1 <- sys_prob(n_periods)
  sel0 <- P0[, 2] + P0[, 3]
  sel1 <- P1[, 2] + P1[, 3]
  absd <- abs(sel1 - sel0)
  tv   <- 0.5 * rowSums(abs(P1 - P0))
  truth_drift <- do.call(rbind, lapply(c("Overall", groups), function(gg) {
    w <- if (gg == "Overall") rep(TRUE, nrow(Q)) else Q$group == gg
    data.frame(group = gg,
               true_net_drift = mean(sel1[w] - sel0[w]),
               true_abs_drift = mean(absd[w]),
               stringsAsFactors = FALSE)
  }))
  if (categorical) {
    by_g <- function(v) vapply(c("Overall", groups), function(gg) {
      w <- if (gg == "Overall") rep(TRUE, nrow(Q)) else Q$group == gg
      mean(v[w])
    }, numeric(1))
    truth_drift$true_net_drift_advance <- by_g(P1[, 3] - P0[, 3])
    truth_drift$true_abs_drift_decision <- by_g(tv)
  }

  unit_truth <- vapply(seq_len(n_units), function(jj) {
    m <- function(t) {
      pr <- par(t)
      acc <- 0
      for (zz in 0:1) {
        wz <- if (zz == 1) pr$pz else 1 - pr$pz
        eta <- eta0(Q$qualification, Q$degree, Q$group, rep(jj, nrow(Q)), pr$s) - zz
        pp <- probs(eta, pr$shift)
        acc <- acc + wz * pp$pass
      }
      mean(acc)
    }
    m(n_periods) - m(1)
  }, numeric(1))

  # ---- truth: decomposition of the change in P(selected) --------------------
  # unit-level rule m_t(x, g, j) and system rule mbar_t(x, g), z integrated
  m_unit <- function(d, t) {
    pr <- par(t)
    jj <- as.integer(sub("^R", "", d$recruiter))
    acc <- 0
    for (zz in 0:1) {
      wz <- if (zz == 1) pr$pz else 1 - pr$pz
      acc <- acc + wz * probs(eta0(d$qualification, d$degree, d$group, jj, pr$s) - zz,
                              pr$shift)$pass
    }
    acc
  }
  m_sys <- function(d, t) {
    pr <- par(t)
    R  <- route(d$qualification, pr$rho)
    acc <- 0
    for (zz in 0:1) {
      wz <- if (zz == 1) pr$pz else 1 - pr$pz
      for (jj in seq_len(n_units)) {
        acc <- acc + wz * R[, jj] *
          probs(eta0(d$qualification, d$degree, d$group, rep(jj, nrow(d)), pr$s) - zz,
                pr$shift)$pass
      }
    }
    acc
  }
  d0 <- out[first, , drop = FALSE]
  d1 <- out[last, , drop = FALSE]
  mu1_1 <- m_unit(d1, n_periods); mu0_1 <- m_unit(d1, 1)
  ms0_1 <- m_sys(d1, 1);          ms0_0 <- m_sys(d0, 1)
  truth_decomp <- do.call(rbind, lapply(c("Overall", groups), function(gg) {
    w1 <- if (gg == "Overall") rep(TRUE, nrow(d1)) else d1$group == gg
    w0 <- if (gg == "Overall") rep(TRUE, nrow(d0)) else d0$group == gg
    data.frame(group = gg,
               true_case_mix   = mean(ms0_1[w1]) - mean(ms0_0[w0]),
               true_allocation = mean(mu0_1[w1]) - mean(ms0_1[w1]),
               true_rule_change = mean(mu1_1[w1] - mu0_1[w1]),
               stringsAsFactors = FALSE)
  }))

  attr(out, "truth") <- list(
    scenario = scenario,
    effect = effect,
    periods = c(first_period, first_period + n_periods - 1L),
    drift = tibble::as_tibble(truth_drift),
    decomposition = tibble::as_tibble(truth_decomp),
    units = tibble::tibble(unit = sprintf("R%02d", seq_len(n_units)),
                           true_unit_drift = unit_truth)
  )
  out
}
