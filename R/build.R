#' Build an AIGovernance audit object
#'
#' @description
#' Constructs an \code{aigov} object from employment decision data. This is the
#' entry point for all auditing, classification, and reporting functions in the
#' \pkg{AIGovernance} package.
#'
#' \strong{Disclaimer:} \pkg{AIGovernance} provides statistical and
#' documentation support tools only. It does not provide legal advice and does
#' not certify compliance with any law or regulation.
#'
#' @param data A data frame containing the employment decision records.
#' @param outcome Unquoted column name of the binary decision variable
#'   (1 = selected / hired / advanced; 0 = not selected).
#' @param group Unquoted column name of the protected-class variable
#'   (e.g., race/ethnicity, gender).
#' @param ref_group Character string identifying the reference group (typically
#'   the highest-selection-rate group, e.g. \code{"White"} or \code{"Male"}).
#' @param frameworks Character vector of governance frameworks to activate.
#'   One or more of \code{"EEOC"}, \code{"NYC_LL144"}, \code{"NIST_RMF"},
#'   \code{"EU_AI_Act"}. Default is \code{c("EEOC", "NYC_LL144", "NIST_RMF")}.
#' @param org_name Optional character string — organisation name for reports.
#' @param system_name Optional character string — name of the AI system being
#'   audited (e.g., \code{"Resume screening tool v2.1"}).
#' @param audit_date Optional \code{Date} or character string (ISO format).
#'   Defaults to \code{Sys.Date()}.
#'
#' @return An object of class \code{"aigov"} containing:
#' \describe{
#'   \item{\code{data}}{The input data frame.}
#'   \item{\code{outcome}}{Name of the outcome column (character).}
#'   \item{\code{group}}{Name of the group column (character).}
#'   \item{\code{ref_group}}{Reference group label.}
#'   \item{\code{frameworks}}{Active frameworks.}
#'   \item{\code{org_name}}{Organisation name.}
#'   \item{\code{system_name}}{AI system name.}
#'   \item{\code{audit_date}}{Audit date.}
#'   \item{\code{group_levels}}{All observed group labels.}
#'   \item{\code{n_total}}{Total number of records.}
#' }
#'
#' @examples
#' data(hiring_sim)
#' gov <- aigov_build(
#'   data        = hiring_sim,
#'   outcome     = selected,
#'   group       = race_ethnicity,
#'   ref_group   = "White",
#'   frameworks  = c("EEOC", "NYC_LL144", "NIST_RMF"),
#'   org_name    = "Acme Corp",
#'   system_name = "Resume Screening Tool v1.0"
#' )
#' print(gov)
#'
#' @export
aigov_build <- function(data,
                        outcome,
                        group,
                        ref_group,
                        frameworks  = c("EEOC", "NYC_LL144", "NIST_RMF"),
                        org_name    = NULL,
                        system_name = NULL,
                        audit_date  = NULL) {

  # --- input checks ----------------------------------------------------------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  outcome_nm <- rlang::as_name(rlang::ensym(outcome))
  group_nm   <- rlang::as_name(rlang::ensym(group))

  if (!outcome_nm %in% names(data)) {
    cli::cli_abort("Column {.val {outcome_nm}} not found in {.arg data}.")
  }
  if (!group_nm %in% names(data)) {
    cli::cli_abort("Column {.val {group_nm}} not found in {.arg data}.")
  }

  outcome_vec <- data[[outcome_nm]]
  if (!all(outcome_vec %in% c(0L, 1L, NA))) {
    cli::cli_abort(
      "{.arg outcome} must be binary (0/1). Found values: {unique(outcome_vec)}."
    )
  }

  valid_fw <- c("EEOC", "NYC_LL144", "NIST_RMF", "EU_AI_Act")
  bad_fw   <- setdiff(frameworks, valid_fw)
  if (length(bad_fw) > 0) {
    cli::cli_abort(
      "Unknown framework(s): {.val {bad_fw}}. ",
      "Choose from {.val {valid_fw}}."
    )
  }

  group_levels <- sort(unique(as.character(data[[group_nm]])))

  if (!ref_group %in% group_levels) {
    cli::cli_abort(
      "{.arg ref_group} = {.val {ref_group}} not found in {.field {group_nm}}.",
      " Available groups: {.val {group_levels}}."
    )
  }

  if (is.null(audit_date)) {
    audit_date <- Sys.Date()
  } else {
    audit_date <- tryCatch(
      as.Date(audit_date),
      error = function(e) {
        cli::cli_abort("Cannot parse {.arg audit_date} as a Date.")
      }
    )
  }

  # --- construct object -------------------------------------------------------
  obj <- structure(
    list(
      data         = data,
      outcome      = outcome_nm,
      group        = group_nm,
      ref_group    = ref_group,
      frameworks   = frameworks,
      org_name     = org_name    %||% "Organisation not specified",
      system_name  = system_name %||% "AI system not specified",
      audit_date   = audit_date,
      group_levels = group_levels,
      n_total      = nrow(data),
      results      = list()     # populated by audit functions
    ),
    class = "aigov"
  )

  cli::cli_alert_success(
    "AIGovernance object built: {obj$n_total} records, ",
    "{length(group_levels)} groups, ",
    "{length(frameworks)} framework(s) active."
  )
  cli::cli_alert_info(
    "Disclaimer: {.pkg AIGovernance} is a statistical support tool. ",
    "It does not provide legal advice or certify legal compliance."
  )

  obj
}

# --------------------------------------------------------------------------- #
# S3 print method                                                              #
# --------------------------------------------------------------------------- #

#' @export
print.aigov <- function(x, ...) {
  cli::cli_h1("AIGovernance Audit Object")
  cli::cli_bullets(c(
    "*" = "Organisation : {x$org_name}",
    "*" = "AI system    : {x$system_name}",
    "*" = "Audit date   : {x$audit_date}",
    "*" = "Records      : {x$n_total}",
    "*" = "Outcome col  : {x$outcome}",
    "*" = "Group col    : {x$group}  ({length(x$group_levels)} levels)",
    "*" = "Ref group    : {x$ref_group}",
    "*" = "Frameworks   : {paste(x$frameworks, collapse = ', ')}"
  ))
  if (length(x$results) > 0) {
    cli::cli_alert_info("Completed modules: {paste(names(x$results), collapse = ', ')}")
  } else {
    cli::cli_alert_info("No audit modules run yet. Call aigov_adverse_impact(), aigov_audit_nyc(), etc.")
  }
  invisible(x)
}

# --------------------------------------------------------------------------- #
# S3 summary method                                                            #
# --------------------------------------------------------------------------- #

#' @export
summary.aigov <- function(object, ...) {
  print(object)
  if (length(object$results) > 0) {
    cli::cli_h2("Results Summary")
    for (nm in names(object$results)) {
      r <- object$results[[nm]]
      if (!is.null(r$verdict)) {
        icon <- if (r$verdict == "PASS") cli::col_green("\u2714") else cli::col_red("\u2718")
        cli::cli_bullets(c(" " = "{icon} {nm}: {r$verdict}"))
      }
    }
  }
  invisible(object)
}

# --------------------------------------------------------------------------- #
# internal helper                                                              #
# --------------------------------------------------------------------------- #
`%||%` <- function(a, b) if (!is.null(a)) a else b
