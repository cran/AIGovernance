#' NIST AI Risk Management Framework (AI RMF 1.0) Audit
#'
#' @description
#' Implements a structured checklist audit aligned with the NIST AI Risk
#' Management Framework (AI RMF 1.0, NIST, 2023). The RMF organises AI risk
#' management into four core functions: \strong{GOVERN}, \strong{MAP},
#' \strong{MEASURE}, and \strong{MANAGE}.
#'
#' This function presents the checklist items most relevant to employment AI
#' systems and records user-supplied responses (or defaults to \code{NA}
#' for items that cannot be verified from data alone).
#'
#' @param gov An \code{aigov} object from \code{\link{aigov_build}}.
#' @param responses An optional named list of logical values (\code{TRUE}/
#'   \code{FALSE}) for checklist items. Use \code{aigov_checklist(gov,
#'   "NIST_RMF")} to see item names. Items not supplied default to \code{NA}.
#'
#' @return The input \code{gov} object with \code{gov$results$nist_rmf}
#'   appended, containing:
#'   \describe{
#'     \item{\code{checklist}}{Named list with \code{TRUE}/\code{FALSE}/\code{NA}
#'       per item.}
#'     \item{\code{scores}}{Per-function completion scores (proportion of
#'       confirmed items).}
#'     \item{\code{overall_score}}{Overall confirmed proportion.}
#'     \item{\code{verdict}}{One of \code{"GREEN"} (>= 0.75), \code{"AMBER"}
#'       (0.50--0.74), or \code{"RED"} (< 0.50).}
#'   }
#'
#' @references
#' National Institute of Standards and Technology (2023).
#' \emph{Artificial Intelligence Risk Management Framework (AI RMF 1.0)}.
#' NIST AI 100-1. \doi{10.6028/NIST.AI.100-1}
#'
#' @examples
#' data(hiring_sim)
#' gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
#' gov <- aigov_audit_nist(gov, responses = list(
#'   GOVERN_1_1 = TRUE,
#'   GOVERN_1_2 = TRUE,
#'   MAP_1_1    = TRUE
#' ))
#'
#' @export
aigov_audit_nist <- function(gov, responses = list()) {

  .check_aigov(gov)

  # --- Full RMF item bank (employment AI subset) ----------------------------
  items <- list(

    # GOVERN
    GOVERN_1_1 = list(fn = "GOVERN", text = "Organisational AI risk policies are established and communicated."),
    GOVERN_1_2 = list(fn = "GOVERN", text = "Roles and responsibilities for AI risk management are defined."),
    GOVERN_1_3 = list(fn = "GOVERN", text = "Senior leadership oversight of AI systems is in place."),
    GOVERN_2_1 = list(fn = "GOVERN", text = "Internal accountability mechanisms exist for AI-related decisions."),
    GOVERN_2_2 = list(fn = "GOVERN", text = "Processes exist for escalating AI concerns or incidents."),
    GOVERN_3_1 = list(fn = "GOVERN", text = "AI system lifecycle is documented (design, development, deployment, decommission)."),
    GOVERN_4_1 = list(fn = "GOVERN", text = "Diversity, equity, and inclusion are considered in AI governance."),

    # MAP
    MAP_1_1 = list(fn = "MAP", text = "AI system context and purpose are documented."),
    MAP_1_2 = list(fn = "MAP", text = "Intended users and affected populations are identified."),
    MAP_1_3 = list(fn = "MAP", text = "Legal and regulatory requirements for the AI system are identified."),
    MAP_2_1 = list(fn = "MAP", text = "Potential harms to individuals from AI decisions are identified."),
    MAP_2_2 = list(fn = "MAP", text = "Protected-group impacts are assessed prior to deployment."),
    MAP_3_1 = list(fn = "MAP", text = "Third-party or vendor AI components are inventoried and reviewed."),
    MAP_5_1 = list(fn = "MAP", text = "Residual risks after mitigation are acknowledged and documented."),

    # MEASURE
    MEASURE_1_1 = list(fn = "MEASURE", text = "Bias and fairness metrics are defined for the AI system."),
    MEASURE_1_2 = list(fn = "MEASURE", text = "Adverse impact analysis has been conducted (e.g., 4/5ths rule)."),
    MEASURE_1_3 = list(fn = "MEASURE", text = "Model performance is evaluated across demographic subgroups."),
    MEASURE_2_1 = list(fn = "MEASURE", text = "Evaluation methods are appropriate and documented."),
    MEASURE_2_2 = list(fn = "MEASURE", text = "Uncertainty and confidence levels in AI outputs are communicated."),
    MEASURE_2_3 = list(fn = "MEASURE", text = "AI system is tested with data representative of deployment context."),
    MEASURE_3_1 = list(fn = "MEASURE", text = "Ongoing monitoring plan for bias drift is established."),
    MEASURE_4_1 = list(fn = "MEASURE", text = "Feedback mechanisms exist for individuals affected by AI decisions."),

    # MANAGE
    MANAGE_1_1 = list(fn = "MANAGE", text = "Risk response plans (accept / mitigate / transfer / avoid) are documented."),
    MANAGE_1_2 = list(fn = "MANAGE", text = "Identified bias risks have defined mitigation actions."),
    MANAGE_2_1 = list(fn = "MANAGE", text = "Human review process is in place for high-stakes AI decisions."),
    MANAGE_2_2 = list(fn = "MANAGE", text = "Override or appeal mechanism exists for affected individuals."),
    MANAGE_3_1 = list(fn = "MANAGE", text = "AI incident response plan is documented."),
    MANAGE_4_1 = list(fn = "MANAGE", text = "Post-deployment monitoring results are reviewed periodically.")
  )

  # --- auto-populate from computed results -----------------------------------
  # If adverse impact already run, mark MEASURE_1_2 = TRUE
  if (!is.null(gov$results$adverse_impact)) {
    if (is.null(responses$MEASURE_1_2)) responses$MEASURE_1_2 <- TRUE
  }

  # --- merge user responses --------------------------------------------------
  checklist <- lapply(names(items), function(k) {
    val <- if (k %in% names(responses)) responses[[k]] else NA
    c(items[[k]], status = list(val))
  })
  names(checklist) <- names(items)

  # --- score per function ---------------------------------------------------
  fns <- c("GOVERN", "MAP", "MEASURE", "MANAGE")

  scores <- vapply(fns, function(fn) {
    idx     <- vapply(checklist, function(x) x$fn == fn, logical(1))
    vals    <- vapply(checklist[idx], function(x) {
      if (is.null(x$status) || is.na(x$status)) NA
      else isTRUE(x$status)
    }, logical(1))
    confirmed <- sum(vals, na.rm = TRUE)
    total     <- length(vals)
    if (total == 0) return(NA_real_)
    confirmed / total
  }, numeric(1))

  overall_score <- mean(scores, na.rm = TRUE)

  verdict <- dplyr::case_when(
    overall_score >= 0.75 ~ "GREEN",
    overall_score >= 0.50 ~ "AMBER",
    TRUE                   ~ "RED"
  )

  gov$results$nist_rmf <- list(
    checklist     = checklist,
    scores        = scores,
    overall_score = overall_score,
    verdict       = verdict
  )

  # --- CLI output -----------------------------------------------------------
  cli::cli_h2("NIST AI RMF 1.0 \u2014 Employment AI Audit")

  score_tbl <- tibble::tibble(
    Function = fns,
    Score    = round(scores, 2),
    Status   = dplyr::case_when(
      scores >= 0.75 ~ "GREEN",
      scores >= 0.50 ~ "AMBER",
      TRUE           ~ "RED"
    )
  )
  print(score_tbl)
  cli::cli_bullets(c("*" = "Overall score: {round(overall_score, 2)} \u2014 {verdict}"))

  col_fn <- switch(verdict,
    GREEN = cli::col_green,
    AMBER = cli::col_yellow,
    RED   = cli::col_red
  )

  if (verdict == "GREEN") {
    cli::cli_alert_success("NIST RMF: {col_fn(verdict)} \u2014 Strong governance evidence.")
  } else if (verdict == "AMBER") {
    cli::cli_alert_warning("NIST RMF: {col_fn(verdict)} \u2014 Moderate gaps; review incomplete items.")
  } else {
    cli::cli_alert_danger("NIST RMF: {col_fn(verdict)} \u2014 Significant governance gaps identified.")
  }

  cli::cli_alert_info(
    "Supply {.arg responses} list to {.fn aigov_audit_nist} to record ",
    "confirmed items. Use {.fn aigov_checklist} to see all item names."
  )

  gov
}
