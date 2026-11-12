#' Audit Decision Drift Between Two Periods
#'
#' @description
#' Separates a change in decision rates between two periods into changes in
#' the cases being decided and changes in how comparable cases are decided.
#' Single-period audits such as the four-fifths rule or the NYC Local Law 144
#' impact ratio cannot make this distinction.
#'
#' Let \eqn{D} be the decision, \eqn{X} the recorded case characteristics
#' (\code{covariates}), \eqn{G} the group and \eqn{t} the period. The function
#' estimates the system-level decision rule
#' \eqn{m_t(x, g) = P_t(D = k \mid X = x, G = g)} in each period and reports:
#' \describe{
#'   \item{Decomposition}{The raw change in the rate of each decision category
#'     is split into a \emph{case-mix} component and a \emph{rule-change}
#'     component evaluated at the current case mix. When \code{unit} is given
#'     (for example recruiter, office, judge or court), the change is split
#'     three ways: case mix, \emph{allocation} of cases to units, and
#'     \emph{within-unit} rule change. The components add to the raw change
#'     exactly in the analysis sample.}
#'   \item{Net drift}{\eqn{\Delta_g = E_{Q_g}[m_1(X, g) - m_0(X, g)]} for the
#'     focal category, over a fixed reference case mix \eqn{Q_g}.}
#'   \item{Absolute drift}{\eqn{A_g = E_{Q_g}[\frac{1}{2}\sum_k |m_1(X,g)_k -
#'     m_0(X,g)_k|]}, the mean total-variation change in the decision rule.
#'     For a binary decision this is \eqn{E_{Q_g}|m_1 - m_0|}. Increases and
#'     decreases for different cases do not cancel, so \eqn{A_g} can be large
#'     when \eqn{\Delta_g} is near zero ("masked drift").}
#'   \item{Unit drift}{With \code{unit}, the drift of each unit over a common
#'     reference case mix, with delta-method standard errors, a
#'     noise-corrected between-unit variance \eqn{\tau^2}, empirical-Bayes
#'     shrunken estimates and Benjamini-Hochberg q-values.}
#' }
#'
#' Cases outside the region where both periods have comparable cases are
#' removed before estimation (\code{overlap_trim}). Uncertainty is estimated
#' with a bootstrap that resamples units (or \code{cluster}) when given.
#'
#' \strong{Interpretation.} Drift is a change in decisions \emph{conditional
#' on the recorded characteristics}. It does not identify the causal effect of
#' an AI system or of human discretion, and a change in an unrecorded
#' decision-relevant characteristic appears as drift. The absolute-drift
#' estimate is biased upward by estimation noise; \code{abs_drift_bc} reports
#' a bootstrap bias-corrected value, and alerts for absolute drift compare the
#' lower confidence bound with \code{tolerance} rather than with zero.
#' The decomposition and drift estimates are model-based standardizations:
#' they are accurate when the decision models are adequately specified. When
#' the true rule is nonlinear in the covariates (for example because cases are
#' routed to units according to their characteristics), add flexible terms
#' with \code{extra_terms} and compare. These procedures are approximate and
#' should be reviewed with the overlap and sample-size diagnostics. This function does not provide legal
#' advice.
#'
#' @param gov An \code{aigov} object from \code{\link{aigov_build}}.
#' @param period Column identifying the period (unquoted name or string).
#' @param covariates Character vector of recorded case characteristics
#'   available before the decision.
#' @param extra_terms Optional character vector of additional model terms
#'   built from \code{covariates}, for example
#'   \code{"I(qualification^2)"}, \code{"qualification:degree"} or
#'   \code{"splines::ns(qualification, 3)"}. Used in the decision and overlap
#'   models. Comparing results with and without such terms is a check of
#'   functional-form sensitivity.
#' @param unit Optional column identifying the decision unit. Enables the
#'   three-way decomposition and unit-level drift.
#' @param decision Optional column with a categorical decision (for example
#'   reject / review / advance). Defaults to the binary outcome given to
#'   \code{aigov_build()}.
#' @param baseline,current Values of \code{period} to compare. Default: the
#'   first and last sorted values.
#' @param reference Reference case mix for net and absolute drift:
#'   \code{"pooled"} (default; both periods), \code{"baseline"} or
#'   \code{"current"}.
#' @param overlap_trim Cases with an estimated period-membership probability
#'   outside \code{[overlap_trim, 1 - overlap_trim]} are removed. Use \code{0}
#'   to keep all cases.
#' @param tolerance Practical tolerance on the probability scale (default
#'   \code{0.05}). An alert requires the confidence interval to lie beyond
#'   this value, so changes that are plausibly within tolerance never
#'   trigger an alert.
#' @param focal Decision category for net and unit drift. Default: the last
#'   category (\code{1} for a binary outcome).
#' @param cluster Optional column for cluster bootstrap resampling. Defaults
#'   to \code{unit} when given.
#' @param B Number of bootstrap replicates (default \code{200}). Use
#'   \code{0} to skip uncertainty estimation.
#' @param conf_level Confidence level for intervals (default \code{0.95}).
#' @param min_unit_n Minimum decisions per unit in each period for the unit
#'   to enter the multilevel analysis (default \code{20}).
#' @param max_ref Maximum number of reference cases used for unit-level drift
#'   (default \code{2000}).
#' @param seed Optional random seed for the bootstrap.
#' @param verbose Print a summary (default \code{TRUE}).
#'
#' @return The input \code{gov} object with \code{gov$results$drift}, a list
#'   containing:
#' \describe{
#'   \item{\code{decomposition}}{Tibble by group and decision category: rates,
#'     raw change, case-mix, allocation (multilevel only), rule-change and net
#'     drift, with bootstrap percentile intervals.}
#'   \item{\code{drift}}{Tibble by group: net drift for the focal category,
#'     absolute drift, bias-corrected absolute drift, intervals, p-values and
#'     alert. \code{net_p} tests \eqn{\Delta_g = 0}; \code{net_p_tol} is a
#'     one-sided test of \eqn{|\Delta_g| \le} \code{tolerance} (normal
#'     approximation). Alerts use interval nulls: \code{"net drift"} when the
#'     whole interval for \eqn{\Delta_g} lies beyond \eqn{\pm}
#'     \code{tolerance}; \code{"masked drift"} when the lower bound for
#'     \eqn{A_g} exceeds \code{tolerance} while \eqn{|\hat\Delta_g| \le}
#'     \code{tolerance}; \code{"conditional drift"} when the lower bound for
#'     \eqn{A_g} exceeds \code{tolerance} otherwise; \code{"none"}
#'     otherwise.}
#'   \item{\code{overlap}}{Cases kept and trimmed by group.}
#'   \item{\code{units}}{Unit-level drift table and summary (multilevel only).}
#'   \item{\code{settings}}{Analysis settings.}
#'   \item{\code{verdict}}{\code{"PASS"} if no alert and no flagged unit,
#'     otherwise \code{"REVIEW"}.}
#' }
#'
#' @references
#' Kitagawa, E. M. (1955). Components of a difference between two rates.
#' \emph{Journal of the American Statistical Association}, 50(272),
#' 1168--1194. \doi{10.1080/01621459.1955.10501299}
#'
#' Cai, T. T., Namkoong, H., & Yadlowsky, S. (2023). Diagnosing model
#' performance under distribution shift. \emph{arXiv}.
#' \doi{10.48550/arXiv.2303.02011}
#'
#' @seealso \code{\link{aigov_monitor}}, \code{\link{aigov_simulate_drift}}
#'
#' @examples
#' data(hiring_panel)
#' gov <- aigov_build(hiring_panel, selected, group, ref_group = "A")
#' gov <- aigov_drift(gov, period = period,
#'                    covariates = c("qualification", "degree"),
#'                    baseline = 2024, current = 2026, B = 30, seed = 1)
#' gov$results$drift$drift
#'
#' \donttest{
#' # three-way decomposition with recruiters as decision units
#' gov <- aigov_drift(gov, period = period,
#'                    covariates = c("qualification", "degree"),
#'                    unit = recruiter, baseline = 2024, current = 2026,
#'                    B = 100, seed = 1)
#' gov$results$drift$units$summary
#' }
#'
#' @export
aigov_drift <- function(gov,
                        period,
                        covariates,
                        extra_terms  = NULL,
                        unit         = NULL,
                        decision     = NULL,
                        baseline     = NULL,
                        current      = NULL,
                        reference    = c("pooled", "baseline", "current"),
                        overlap_trim = 0.02,
                        tolerance    = 0.05,
                        focal        = NULL,
                        cluster      = NULL,
                        B            = 200L,
                        conf_level   = 0.95,
                        min_unit_n   = 20L,
                        max_ref      = 2000L,
                        seed         = NULL,
                        verbose      = TRUE) {

  .check_aigov(gov)
  reference <- match.arg(reference)

  period_col   <- .as_colname(rlang::enquo(period), "period")
  unit_col     <- .as_colname(rlang::enquo(unit), "unit")
  decision_col <- .as_colname(rlang::enquo(decision), "decision")
  cluster_col  <- .as_colname(rlang::enquo(cluster), "cluster")
  if (is.null(period_col)) cli::cli_abort("{.arg period} is required.")
  if (is.null(decision_col)) decision_col <- gov$outcome

  data <- gov$data
  cols <- c(period_col, unit_col, decision_col, cluster_col, covariates)
  miss <- setdiff(cols, names(data))
  if (length(miss) > 0) cli::cli_abort("Column(s) not found in data: {.val {miss}}.")
  if (!is.character(covariates) || length(covariates) == 0) {
    cli::cli_abort("{.arg covariates} must be a non-empty character vector.")
  }
  if (any(c(gov$group, period_col, unit_col, decision_col) %in% covariates)) {
    cli::cli_abort("{.arg covariates} must not include the group, period, unit or decision column.")
  }
  if (!is.null(extra_terms) && !is.character(extra_terms)) {
    cli::cli_abort("{.arg extra_terms} must be a character vector of model terms.")
  }
  if (!is.numeric(tolerance) || tolerance < 0) cli::cli_abort("{.arg tolerance} must be non-negative.")
  if (!is.numeric(overlap_trim) || overlap_trim < 0 || overlap_trim >= 0.5) {
    cli::cli_abort("{.arg overlap_trim} must be in [0, 0.5).")
  }

  per_vals <- sort(unique(data[[period_col]]))
  if (length(per_vals) < 2L) cli::cli_abort("{.arg period} must have at least two values.")
  baseline <- as.character(if (is.null(baseline)) per_vals[1] else baseline)
  current  <- as.character(if (is.null(current)) per_vals[length(per_vals)] else current)
  if (!all(c(baseline, current) %in% as.character(per_vals))) {
    cli::cli_abort("{.arg baseline} and {.arg current} must be values of {.field {period_col}}.")
  }
  if (baseline == current) cli::cli_abort("{.arg baseline} and {.arg current} must differ.")

  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(seed)
  }

  res <- .drift_engine(
    data = data, decision_col = decision_col, group_col = gov$group,
    ref_group = gov$ref_group, period_col = period_col,
    baseline = baseline, current = current, covariates = covariates,
    unit_col = unit_col, cluster_col = cluster_col, reference = reference,
    overlap_trim = overlap_trim, tolerance = tolerance, focal = focal,
    B = as.integer(B), conf_level = conf_level,
    min_unit_n = as.integer(min_unit_n), max_ref = as.integer(max_ref),
    extra_terms = extra_terms
  )

  any_alert <- any(res$drift$alert != "none")
  any_unit  <- !is.null(res$units) && res$units$summary$n_flagged > 0
  res$verdict <- if (any_alert || any_unit) "REVIEW" else "PASS"
  class(res) <- c("aigov_drift", "list")

  gov$results$drift <- res
  if (verbose) print(res)
  gov
}

#' @export
print.aigov_drift <- function(x, ...) {
  s <- x$settings
  cli::cli_h2("Decision Drift Audit")
  cli::cli_bullets(c(
    "*" = "Periods      : {s$baseline} (baseline) vs {s$current} (current)",
    "*" = "Decision     : {s$decision} [{paste(s$levels, collapse = ' / ')}], focal = {s$focal}",
    "*" = "Covariates   : {paste(c(s$covariates, s$extra_terms), collapse = ', ')}",
    "*" = "Units        : {if (is.null(s$unit)) 'none (single-level)' else s$unit}",
    "*" = "Reference Q  : {s$reference}; overlap trim = {s$overlap_trim}; tolerance = {s$tolerance}",
    "*" = "Analysis n   : {s$n_analysis}; bootstrap replicates used = {s$n_boot_ok}"
  ))
  ov <- x$overlap$share_trimmed[x$overlap$group == "Overall"]
  if (length(ov) && ov > 0.10) {
    cli::cli_alert_warning("{round(100 * ov, 1)}% of cases were outside the overlap region.")
  }
  if (isTRUE(s$separation)) {
    cli::cli_alert_warning("Some decision models showed (quasi-)separation; check sparse groups or units.")
  }
  if (length(s$units_dropped) > 0) {
    cli::cli_alert_info("{length(s$units_dropped)} unit(s) dropped (fewer than {s$min_unit_n} decisions in a period).")
  }

  dec <- x$decomposition[x$decomposition$category == s$focal, ]
  show <- intersect(c("group", "rate_baseline", "rate_current", "raw_change",
                      "case_mix", "allocation", "rule_change"), names(dec))
  cli::cli_text("Decomposition of the change in P(D = {s$focal}):")
  print(as.data.frame(lapply(dec[, show], function(v) if (is.numeric(v)) round(v, 4) else v)),
        row.names = FALSE)

  cli::cli_text("Net and absolute drift over the reference case mix:")
  dt <- x$drift[, c("group", "net_drift", "net_lo", "net_hi", "abs_drift",
                    "abs_drift_bc", "abs_lo", "alert")]
  print(as.data.frame(lapply(dt, function(v) if (is.numeric(v)) round(v, 4) else v)),
        row.names = FALSE)

  if (!is.null(x$units)) {
    u <- x$units$summary
    cli::cli_text("Unit-level drift ({u$n_units} units):")
    cli::cli_bullets(c(
      "*" = "Mean unit drift = {round(u$mean_drift, 4)}; between-unit SD (noise-corrected) = {round(u$tau, 4)}; reliability = {round(u$reliability, 2)}",
      "*" = "Mean |shrunken unit drift| = {round(u$mean_abs_drift_shrunk, 4)}; share beyond tolerance = {round(u$share_beyond_tolerance, 3)}",
      "*" = "Units flagged (BH q < {1 - s$conf_level} and |shrunken drift| > tolerance): {u$n_flagged}"
    ))
  }

  if (x$verdict == "PASS") {
    cli::cli_alert_success("Result: PASS \u2014 no drift beyond tolerance detected.")
  } else {
    cli::cli_alert_danger("Result: REVIEW \u2014 drift beyond tolerance detected; examine rule changes, coding, model versions and override practice.")
  }
  cli::cli_alert_info(
    "Drift is conditional on recorded characteristics; it does not identify the causal effect of an AI system."
  )
  invisible(x)
}
