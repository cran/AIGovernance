#' Monitor Decision Drift Across Several Periods
#'
#' @description
#' Runs \code{\link{aigov_drift}} for a sequence of prespecified comparisons
#' (each period against the baseline, or each period against the previous
#' one) and adjusts the net-drift p-values for the number of comparisons. A
#' net-drift alert is kept only if the adjusted one-sided p-value for
#' \eqn{|\Delta| \le} \code{tolerance} is below \eqn{(1 -}
#' \code{conf_level}\eqn{)/2}.
#' This supports the periodic monitoring of deployed decision systems.
#'
#' \strong{Note:} the adjustment covers the comparisons made in one call. It
#' does not make repeated, data-dependent looks at accumulating data valid;
#' that requires sequential (anytime-valid) methods.
#'
#' @inheritParams aigov_drift
#' @param compare \code{"baseline"} (default) compares every later period with
#'   the first; \code{"previous"} compares consecutive periods.
#' @param adjust Multiplicity adjustment passed to
#'   \code{\link[stats]{p.adjust}} (default \code{"holm"}).
#' @param ... Further arguments passed to \code{\link{aigov_drift}}, such as
#'   \code{reference}, \code{overlap_trim}, \code{focal}, \code{cluster} or
#'   \code{min_unit_n}.
#'
#' @return The input \code{gov} object with \code{gov$results$monitor}, a list
#'   with \code{table} (one row per comparison and group, with adjusted
#'   p-values and alerts), \code{fits} (the individual drift results) and
#'   \code{verdict}.
#'
#' @examples
#' \donttest{
#' data(hiring_panel)
#' gov <- aigov_build(hiring_panel, selected, group, ref_group = "A")
#' gov <- aigov_monitor(gov, period = period,
#'                      covariates = c("qualification", "degree"),
#'                      B = 50, seed = 1)
#' gov$results$monitor$table
#' }
#'
#' @export
aigov_monitor <- function(gov,
                          period,
                          covariates,
                          unit      = NULL,
                          decision  = NULL,
                          compare   = c("baseline", "previous"),
                          adjust    = "holm",
                          tolerance = 0.05,
                          B         = 200L,
                          seed      = NULL,
                          verbose   = TRUE,
                          ...) {
  .check_aigov(gov)
  compare <- match.arg(compare)
  period_col   <- .as_colname(rlang::enquo(period), "period")
  unit_col     <- .as_colname(rlang::enquo(unit), "unit")
  decision_col <- .as_colname(rlang::enquo(decision), "decision")
  if (is.null(period_col)) cli::cli_abort("{.arg period} is required.")
  if (!period_col %in% names(gov$data)) cli::cli_abort("Column {.val {period_col}} not found.")

  per <- sort(unique(gov$data[[period_col]]))
  if (length(per) < 2L) cli::cli_abort("{.arg period} must have at least two values.")
  pairs <- if (compare == "baseline") {
    lapply(per[-1], function(p) c(per[1], p))
  } else {
    lapply(seq_len(length(per) - 1L), function(i) c(per[i], per[i + 1L]))
  }

  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(seed)
  }

  fits <- lapply(pairs, function(pp) {
    g <- aigov_drift(gov, period = !!period_col, covariates = covariates,
                     unit = !!unit_col, decision = !!decision_col,
                     baseline = pp[1], current = pp[2],
                     tolerance = tolerance, B = B, verbose = FALSE, ...)
    g$results$drift
  })
  names(fits) <- vapply(pairs, function(pp) paste(pp, collapse = " -> "), character(1))

  tab <- do.call(rbind, lapply(seq_along(fits), function(i) {
    d <- fits[[i]]$drift
    u <- fits[[i]]$units
    d$comparison <- names(fits)[i]
    d$units_flagged <- if (is.null(u)) NA_integer_ else u$summary$n_flagged
    d
  }))
  tab$net_p_adj <- stats::p.adjust(tab$net_p, method = adjust)
  tab$net_p_tol_adj <- stats::p.adjust(tab$net_p_tol, method = adjust)
  alpha_side <- (1 - fits[[1]]$settings$conf_level) / 2
  tol <- fits[[1]]$settings$tolerance
  demoted <- tab$alert == "net drift" &
    !(is.na(tab$net_p_tol_adj) | tab$net_p_tol_adj < alpha_side)
  fallback <- ifelse(!is.na(tab$abs_lo) & tab$abs_lo > tol,
                     ifelse(abs(tab$net_drift) <= tol, "masked drift", "conditional drift"),
                     "none")
  tab$alert_adj <- ifelse(demoted, fallback, tab$alert)
  tab <- tab[, c("comparison", setdiff(names(tab), "comparison"))]

  verdict <- if (any(tab$alert_adj != "none") ||
                 any(tab$units_flagged > 0, na.rm = TRUE)) "REVIEW" else "PASS"
  gov$results$monitor <- list(table = tab, fits = fits, compare = compare,
                              adjust = adjust, verdict = verdict)

  if (verbose) {
    cli::cli_h2("Decision Drift Monitoring")
    cli::cli_text("{length(fits)} comparison(s), {compare} design; net-drift p-values adjusted by {adjust}.")
    show <- tab[, c("comparison", "group", "net_drift", "net_p_tol_adj",
                    "abs_drift_bc", "abs_lo", "alert_adj", "units_flagged")]
    print(as.data.frame(lapply(show, function(v) if (is.numeric(v)) round(v, 4) else v)),
          row.names = FALSE)
    if (verdict == "PASS") {
      cli::cli_alert_success("Result: PASS \u2014 no drift beyond tolerance in any comparison.")
    } else {
      cli::cli_alert_danger("Result: REVIEW \u2014 at least one comparison shows drift beyond tolerance.")
    }
  }
  gov
}
