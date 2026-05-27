#' NYC Local Law 144 Bias Audit Module
#'
#' @description
#' Implements the statistical components of a NYC Local Law 144 (2023) bias
#' audit for Automated Employment Decision Tools (AEDTs). The law requires
#' employers using AEDTs to:
#' \enumerate{
#'   \item Conduct an annual independent bias audit.
#'   \item Publish a summary of audit results on their website.
#'   \item Provide advance notice to candidates/employees.
#' }
#'
#' This function computes the required \strong{impact ratio} statistics for
#' each race/ethnicity and sex category and generates a publication-ready
#' summary table matching the format expected in public disclosures.
#'
#' The NYC LL144 impact ratio is:
#' \deqn{IR_g = \frac{\text{selection rate}_g}{\text{selection rate of most-selected category}}}
#'
#' \strong{Note:} LL144 uses the \emph{most-selected category} (not a
#' user-specified reference group) as the denominator -- this differs from the
#' EEOC approach. This function implements both.
#'
#' @param gov An \code{aigov} object from \code{\link{aigov_build}}.
#' @param use_most_selected Logical. If \code{TRUE} (default), the denominator
#'   is the group with the highest selection rate (NYC LL144 standard). If
#'   \code{FALSE}, uses \code{gov$ref_group} (EEOC standard).
#'
#' @return The input \code{gov} object with \code{gov$results$nyc_ll144}
#'   appended, containing:
#'   \describe{
#'     \item{\code{impact_table}}{Tibble with selection rates and impact ratios.}
#'     \item{\code{most_selected_group}}{The reference category used.}
#'     \item{\code{disclosure_table}}{Formatted table for public disclosure.}
#'     \item{\code{checklist}}{LL144 procedural checklist (named logical vector).}
#'     \item{\code{verdict}}{Statistical verdict: \code{"PASS"} or \code{"REVIEW"}.}
#'   }
#'
#' @references
#' New York City Local Law 144 of 2021 (effective January 1, 2023).
#' NYC Department of Consumer and Worker Protection (DCWP).
#' \url{https://www.nyc.gov/site/dca/about/automated-employment-decision-tools.page}
#'
#' @examples
#' data(hiring_sim)
#' gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White",
#'                   frameworks = c("NYC_LL144"))
#' gov <- aigov_audit_nyc(gov)
#'
#' @export
aigov_audit_nyc <- function(gov, use_most_selected = TRUE) {

  .check_aigov(gov)

  data    <- gov$data
  outcome <- gov$outcome
  grp_col <- gov$group

  # --- compute selection rates -----------------------------------------------
  rates <- data %>%
    dplyr::mutate(.grp = as.character(.data[[grp_col]])) %>%
    dplyr::group_by(.data$.grp) %>%
    dplyr::summarise(
      n             = dplyr::n(),
      n_selected    = sum(.data[[outcome]], na.rm = TRUE),
      selection_rate = .data$n_selected / .data$n,
      .groups       = "drop"
    ) %>%
    dplyr::rename(category = .data$.grp)

  # --- reference category ----------------------------------------------------
  if (use_most_selected) {
    most_sel_grp  <- rates$category[which.max(rates$selection_rate)]
    most_sel_rate <- max(rates$selection_rate, na.rm = TRUE)
  } else {
    most_sel_grp  <- gov$ref_group
    most_sel_rate <- rates$selection_rate[rates$category == gov$ref_group]
  }

  # --- impact ratios ---------------------------------------------------------
  impact_table <- rates %>%
    dplyr::mutate(
      impact_ratio  = round(.data$selection_rate / most_sel_rate, 4),
      ir_flag       = .data$impact_ratio < 0.80 &
                        .data$category != most_sel_grp
    )

  # --- disclosure table (matches NYC DCWP format) ----------------------------
  disclosure_table <- impact_table %>%
    dplyr::transmute(
      `Category`                        = .data$category,
      `Total Assessed`                  = .data$n,
      `Number Selected`                 = .data$n_selected,
      `Selection Rate`                  = round(.data$selection_rate, 4),
      `Impact Ratio`                    = .data$impact_ratio,
      `Flagged (IR < 0.80)`             = .data$ir_flag
    )

  # --- procedural checklist --------------------------------------------------
  checklist <- c(
    "Annual bias audit conducted"                          = TRUE,
    "Audit performed prior to AEDT use or within 1 year"  = NA,
    "Impact ratios computed for all categories"            = TRUE,
    "Audit results published on employer website"          = NA,
    "Publication includes audit date"                      = NA,
    "Publication includes distribution date of AEDT"       = NA,
    "Candidate/employee notice provided (10 days prior)"   = NA,
    "Notice includes AEDT purpose and data collected"      = NA,
    "Accommodation process documented for candidates"      = NA,
    "Audit conducted by independent auditor"               = NA
  )

  # statistical verdict
  n_flagged <- sum(impact_table$ir_flag, na.rm = TRUE)
  verdict   <- if (n_flagged == 0) "PASS" else "REVIEW"

  gov$results$nyc_ll144 <- list(
    impact_table       = impact_table,
    most_selected_group = most_sel_grp,
    disclosure_table   = disclosure_table,
    checklist          = checklist,
    verdict            = verdict,
    n_flagged          = n_flagged,
    use_most_selected  = use_most_selected
  )

  # --- CLI output ------------------------------------------------------------
  cli::cli_h2("NYC Local Law 144 \u2014 Bias Audit Results")
  cli::cli_bullets(c(
    "*" = "Reference category (most selected): {.val {most_sel_grp}}",
    "*" = "Reference selection rate: {round(most_sel_rate, 4)}"
  ))
  print(disclosure_table, n = Inf)

  if (verdict == "PASS") {
    cli::cli_alert_success(
      "Statistical result: PASS \u2014 All impact ratios >= 0.80."
    )
  } else {
    cli::cli_alert_danger(
      "Statistical result: REVIEW \u2014 {n_flagged} category(ies) with IR < 0.80."
    )
  }

  cli::cli_h3("Procedural Checklist (NA = requires employer confirmation)")
  for (item in names(checklist)) {
    val  <- checklist[item]
    icon <- if (isTRUE(val)) cli::col_green("\u2714") else
              if (isFALSE(val)) cli::col_red("\u2718") else
              cli::col_yellow("?")
    cli::cli_bullets(c(" " = "{icon} {item}"))
  }

  cli::cli_alert_info(
    "Disclaimer: This output is a statistical support tool. ",
    "Full LL144 compliance requires legal review, independent auditor, ",
    "and public disclosure. This package does not certify compliance."
  )

  gov
}
