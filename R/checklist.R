#' Determine Applicable Governance Frameworks
#'
#' @description
#' Returns a summary of which governance frameworks apply to the AI system
#' described in the \code{aigov} object, based on domain and jurisdiction.
#'
#' @param gov An \code{aigov} object from \code{\link{aigov_build}}.
#' @param domain Character. Application domain (default \code{"employment"}).
#' @param us_state Optional character. US state, e.g. \code{"NY"}, \code{"CO"},
#'   \code{"IL"} for state-specific law flags.
#'
#' @return A tibble with columns \code{framework}, \code{applies},
#'   \code{jurisdiction}, and \code{note}.
#'
#' @examples
#' data(hiring_sim)
#' gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
#' aigov_scope(gov, domain = "employment", us_state = "NY")
#'
#' @export
aigov_scope <- function(gov, domain = "employment", us_state = NULL) {

  .check_aigov(gov)

  scope_db <- tibble::tibble(
    framework    = c(
      "EEOC Uniform Guidelines (4/5ths rule)",
      "NYC Local Law 144 (LL144)",
      "Colorado AI Act (SB 205)",
      "Illinois AI Video Interview Act",
      "NIST AI RMF 1.0",
      "EU AI Act \u2014 High Risk (Annex III)",
      "GDPR Article 22",
      "ECOA / Regulation B"
    ),
    jurisdiction = c("US Federal", "US - NYC", "US - CO",
                     "US - IL", "US Federal (voluntary)",
                     "EU", "EU", "US Federal"),
    domain_match = c(
      "employment",
      "employment",
      "employment",
      "employment",
      "any",
      "employment",
      "any",
      "credit"
    ),
    state_req    = c(NA, "NY", "CO", "IL", NA, NA, NA, NA),
    note         = c(
      "Applies to all US employers using selection procedures",
      "Mandatory annual audit for NYC employers using AEDTs",
      "Effective 2026: consequential decisions affecting CO residents",
      "Applies to AI video interviews for IL job applicants",
      "Voluntary framework; required for US federal agencies",
      "Employment/worker management AI is Annex III high-risk",
      "Right to explanation for automated decisions",
      "Does not apply to employment domain"
    )
  )

  # filter by domain
  scope_db <- scope_db %>%
    dplyr::mutate(
      applies = dplyr::case_when(
        .data$domain_match == "any" ~ TRUE,
        .data$domain_match == domain ~ TRUE,
        TRUE ~ FALSE
      )
    )

  # state filters
  if (!is.null(us_state)) {
    scope_db <- scope_db %>%
      dplyr::mutate(
        applies = dplyr::case_when(
          !is.na(.data$state_req) & .data$state_req != us_state ~ FALSE,
          TRUE ~ .data$applies
        )
      )
  } else {
    # if no state given, mark state-specific as "Check jurisdiction"
    scope_db <- scope_db %>%
      dplyr::mutate(
        applies = dplyr::case_when(
          !is.na(.data$state_req) ~ NA,
          TRUE ~ .data$applies
        )
      )
  }

  out <- scope_db %>%
    dplyr::select("framework", "applies", "jurisdiction", "note") %>%
    dplyr::arrange(dplyr::desc(.data$applies))

  cli::cli_h2("Governance Scope \u2014 Applicable Frameworks")
  for (i in seq_len(nrow(out))) {
    app <- out$applies[i]
    icon <- if (isTRUE(app)) cli::col_green("\u2714 APPLIES   ") else
              if (isFALSE(app)) cli::col_silver("\u2718 NOT APPLICABLE") else
              cli::col_yellow("? CHECK JURISDICTION")
    cli::cli_bullets(c(" " = "{icon} | {out$framework[i]}"))
  }

  invisible(out)
}


# ============================================================================ #
# aigov_checklist                                                               #
# ============================================================================ #

#' Display Governance Checklist for a Framework
#'
#' @description
#' Returns all checklist item names and descriptions for a given framework.
#' Use the returned item names as keys in the \code{responses} argument of
#' \code{\link{aigov_audit_nist}}.
#'
#' @param gov An \code{aigov} object (used only for class checking).
#' @param framework Character. One of \code{"NYC_LL144"}, \code{"EEOC"},
#'   \code{"NIST_RMF"}, or \code{"EU_AI_Act"}.
#'
#' @return A tibble with columns \code{item_id}, \code{function_area},
#'   and \code{description}.
#'
#' @examples
#' data(hiring_sim)
#' gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
#' aigov_checklist(gov, "NYC_LL144")
#' aigov_checklist(gov, "NIST_RMF")
#'
#' @export
aigov_checklist <- function(gov, framework) {

  .check_aigov(gov)

  valid <- c("NYC_LL144", "EEOC", "NIST_RMF", "EU_AI_Act")
  if (!framework %in% valid) {
    cli::cli_abort("framework must be one of {.val {valid}}.")
  }

  cl <- switch(framework,

    NYC_LL144 = tibble::tibble(
      item_id       = paste0("LL144_", seq_len(10)),
      function_area = rep("NYC Local Law 144", 10),
      description   = c(
        "Annual bias audit has been conducted",
        "Audit was performed before AEDT use or within 1 year of current use",
        "Impact ratios computed for each race/ethnicity category",
        "Impact ratios computed for each sex category",
        "Audit results published on employer website",
        "Publication includes the date of the audit",
        "Publication includes the distribution date of the AEDT",
        "Candidates/employees notified at least 10 business days before use",
        "Notice describes AEDT type and data/criteria used",
        "Audit was conducted by an independent auditor"
      )
    ),

    EEOC = tibble::tibble(
      item_id       = paste0("EEOC_", seq_len(8)),
      function_area = rep("EEOC Uniform Guidelines", 8),
      description   = c(
        "Selection rates computed for all race/ethnicity groups",
        "Selection rates computed for gender groups",
        "Adverse Impact Ratio (AIR) calculated for all groups",
        "4/5ths (80%) rule applied to all protected categories",
        "Sample sizes sufficient for reliable AIR estimates (n >= 30 per group)",
        "Differential validity analysis conducted",
        "Selection procedure validated for job-relatedness",
        "Records retained per EEOC recordkeeping requirements"
      )
    ),

    NIST_RMF = tibble::tibble(
      item_id       = c(
        "GOVERN_1_1","GOVERN_1_2","GOVERN_1_3","GOVERN_2_1","GOVERN_2_2",
        "GOVERN_3_1","GOVERN_4_1",
        "MAP_1_1","MAP_1_2","MAP_1_3","MAP_2_1","MAP_2_2","MAP_3_1","MAP_5_1",
        "MEASURE_1_1","MEASURE_1_2","MEASURE_1_3","MEASURE_2_1","MEASURE_2_2",
        "MEASURE_2_3","MEASURE_3_1","MEASURE_4_1",
        "MANAGE_1_1","MANAGE_1_2","MANAGE_2_1","MANAGE_2_2",
        "MANAGE_3_1","MANAGE_4_1"
      ),
      function_area = c(
        rep("GOVERN", 7), rep("MAP", 7),
        rep("MEASURE", 8), rep("MANAGE", 6)
      ),
      description   = c(
        "Organisational AI risk policies are established and communicated",
        "Roles and responsibilities for AI risk management are defined",
        "Senior leadership oversight of AI systems is in place",
        "Internal accountability mechanisms exist for AI-related decisions",
        "Processes exist for escalating AI concerns or incidents",
        "AI system lifecycle is documented",
        "Diversity, equity, and inclusion are considered in AI governance",
        "AI system context and purpose are documented",
        "Intended users and affected populations are identified",
        "Legal and regulatory requirements are identified",
        "Potential harms to individuals from AI decisions are identified",
        "Protected-group impacts are assessed prior to deployment",
        "Third-party or vendor AI components are inventoried and reviewed",
        "Residual risks after mitigation are acknowledged and documented",
        "Bias and fairness metrics are defined for the AI system",
        "Adverse impact analysis has been conducted",
        "Model performance is evaluated across demographic subgroups",
        "Evaluation methods are appropriate and documented",
        "Uncertainty in AI outputs is communicated",
        "AI system is tested with representative deployment data",
        "Ongoing monitoring plan for bias drift is established",
        "Feedback mechanisms exist for affected individuals",
        "Risk response plans are documented",
        "Identified bias risks have defined mitigation actions",
        "Human review process is in place for high-stakes decisions",
        "Override or appeal mechanism exists for affected individuals",
        "AI incident response plan is documented",
        "Post-deployment monitoring results are reviewed periodically"
      )
    ),

    EU_AI_Act = tibble::tibble(
      item_id       = paste0("EU_", c(9,10,11,13,14,15,43,49)),
      function_area = paste0("Art. ", c(9,10,11,13,14,15,43,49)),
      description   = c(
        "Art. 9  \u2014 Risk management system is established and maintained",
        "Art. 10 \u2014 Training data governance and bias examination completed",
        "Art. 11 \u2014 Technical documentation prepared before market placement",
        "Art. 13 \u2014 Transparency information provided to deployers",
        "Art. 14 \u2014 Human oversight measures are implemented",
        "Art. 15 \u2014 Accuracy, robustness, and cybersecurity requirements met",
        "Art. 43 \u2014 Conformity assessment completed (self-assessment or notified body)",
        "Art. 49 \u2014 System registered in EU AI database"
      )
    )
  )

  # --- derive status from results already stored in gov ----------------------
  nist_done <- gov$results$nist_rmf$checklist
  ai_done   <- !is.null(gov$results$adverse_impact)
  nyc_done  <- !is.null(gov$results$nyc_ll144)

  cl$status <- vapply(seq_len(nrow(cl)), function(i) {
    id <- cl$item_id[i]
    # NIST items: pull directly from the stored checklist
    if (!is.null(nist_done) && id %in% names(nist_done)) {
      s <- nist_done[[id]]$status
      return(if (isTRUE(s)) "Complete" else if (isFALSE(s)) "Incomplete" else "Pending")
    }
    # EEOC items auto-derived from adverse impact run
    if (framework == "EEOC") {
      if (ai_done && id %in% c("EEOC_1", "EEOC_3", "EEOC_4")) return("Complete")
    }
    # NYC LL144 items auto-derived from nyc audit run
    if (framework == "NYC_LL144") {
      if (nyc_done && id %in% c("LL144_1", "LL144_3")) return("Complete")
    }
    "Pending"
  }, character(1))

  cli::cli_h2("Checklist: {framework}")
  print(cl, n = Inf)
  invisible(cl)
}
