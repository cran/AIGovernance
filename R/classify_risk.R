#' Classify AI System Risk Level
#'
#' @description
#' Classifies the AI system under two frameworks:
#' \enumerate{
#'   \item \strong{EU AI Act (2024)} -- Assigns one of four risk tiers:
#'     Unacceptable, High, Limited, or Minimal risk. Employment/worker-management
#'     AI is listed in Annex III as \emph{High Risk}.
#'   \item \strong{NIST AI RMF} -- Assigns a risk tier (1--4) based on impact
#'     on individuals' rights and opportunities.
#' }
#'
#' @param gov An \code{aigov} object from \code{\link{aigov_build}}.
#' @param domain Character. Application domain. One of \code{"employment"},
#'   \code{"education"}, \code{"credit"}, \code{"housing"}, \code{"healthcare"},
#'   \code{"law_enforcement"}, \code{"other"}. Default \code{"employment"}.
#' @param makes_final_decision Logical. Does the AI system make or
#'   substantially influence a final employment decision? Default \code{TRUE}.
#' @param human_oversight Logical. Is meaningful human review in place before
#'   the AI decision takes effect? Default \code{NA} (unknown).
#'
#' @return The input \code{gov} object with \code{gov$results$risk_class}
#'   appended, containing EU AI Act and NIST risk classifications with
#'   explanatory text.
#'
#' @references
#' European Parliament and Council (2024). Regulation (EU) 2024/1689 (EU AI Act).
#' \url{https://digital-strategy.ec.europa.eu/en/policies/regulatory-framework-ai}
#'
#' NIST (2023). \emph{AI RMF 1.0}. \doi{10.6028/NIST.AI.100-1}
#'
#' @examples
#' data(hiring_sim)
#' gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
#' gov <- aigov_classify(gov, domain = "employment",
#'                        makes_final_decision = TRUE,
#'                        human_oversight = FALSE)
#'
#' @export
aigov_classify <- function(gov,
                            domain               = "employment",
                            makes_final_decision = TRUE,
                            human_oversight      = NA) {

  .check_aigov(gov)

  valid_domains <- c("employment", "education", "credit", "housing",
                     "healthcare", "law_enforcement", "other")
  if (!domain %in% valid_domains) {
    cli::cli_abort(
      "{.arg domain} must be one of {.val {valid_domains}}."
    )
  }

  # --- EU AI Act classification ----------------------------------------------
  eu_high_risk_domains <- c("employment", "education", "credit",
                             "housing", "law_enforcement", "healthcare")

  eu_tier <- if (domain %in% eu_high_risk_domains && isTRUE(makes_final_decision)) {
    "HIGH RISK"
  } else if (domain %in% eu_high_risk_domains) {
    "HIGH RISK (verify scope)"
  } else {
    "LIMITED / MINIMAL RISK"
  }

  eu_annex <- switch(domain,
    employment     = "Annex III, Point 4 \u2014 Employment, workers management, and access to self-employment",
    education      = "Annex III, Point 3 \u2014 Education and vocational training",
    credit         = "Annex III, Point 5b \u2014 Access to financial services",
    housing        = "Annex III, Point 5a \u2014 Access to housing",
    healthcare     = "Annex III, Point 6 \u2014 Access to health and life services",
    law_enforcement = "Annex III, Point 6 \u2014 Law enforcement",
    other          = "Not listed in Annex III \u2014 likely Limited or Minimal risk"
  )

  eu_obligations <- if (grepl("HIGH", eu_tier)) {
    c(
      "Art. 9  \u2014 Risk management system (documented, ongoing)",
      "Art. 10 \u2014 Data governance and training data requirements",
      "Art. 11 \u2014 Technical documentation (before market placement)",
      "Art. 13 \u2014 Transparency and provision of information to deployers",
      "Art. 14 \u2014 Human oversight measures",
      "Art. 15 \u2014 Accuracy, robustness, and cybersecurity",
      "Art. 43 \u2014 Conformity assessment",
      "Art. 49 \u2014 Registration in EU AI database"
    )
  } else {
    c("Art. 50 \u2014 Transparency obligations (if applicable)")
  }

  # --- NIST RMF risk tier ---------------------------------------------------
  nist_tier <- dplyr::case_when(
    domain == "law_enforcement"  ~ 4L,
    domain %in% c("employment", "credit", "housing", "healthcare") &
      isTRUE(makes_final_decision) ~ 3L,
    domain %in% c("employment", "education") &
      !isTRUE(makes_final_decision) ~ 2L,
    TRUE ~ 1L
  )

  nist_desc <- switch(as.character(nist_tier),
    "4" = "Tier 4 \u2014 Critical impact: potential violation of fundamental rights",
    "3" = "Tier 3 \u2014 Significant impact on individuals' rights and opportunities",
    "2" = "Tier 2 \u2014 Moderate impact; human review typically present",
    "1" = "Tier 1 \u2014 Minimal impact; general-purpose or advisory systems"
  )

  # --- human oversight note -------------------------------------------------
  oversight_note <- if (isTRUE(human_oversight)) {
    "Human oversight: CONFIRMED \u2014 may reduce regulatory burden under Art. 14."
  } else if (isFALSE(human_oversight)) {
    "Human oversight: NOT IN PLACE \u2014 HIGH PRIORITY gap for EU Art. 14 and EEOC."
  } else {
    "Human oversight: UNKNOWN \u2014 requires clarification."
  }

  gov$results$risk_class <- list(
    domain               = domain,
    makes_final_decision = makes_final_decision,
    human_oversight      = human_oversight,
    eu = list(
      tier         = eu_tier,
      annex        = eu_annex,
      obligations  = eu_obligations
    ),
    nist = list(
      tier         = nist_tier,
      description  = nist_desc
    ),
    oversight_note = oversight_note
  )

  # --- CLI output -----------------------------------------------------------
  cli::cli_h2("AI System Risk Classification")
  cli::cli_bullets(c(
    "*" = "Domain               : {domain}",
    "*" = "Makes final decision : {makes_final_decision}"
  ))

  cli::cli_h3("EU AI Act")
  cli::cli_bullets(c(
    "*" = "Risk tier   : {eu_tier}",
    "*" = "Annex basis : {eu_annex}"
  ))
  cli::cli_alert_info("Key obligations:")
  for (ob in eu_obligations) cli::cli_bullets(c(" " = ob))

  cli::cli_h3("NIST AI RMF")
  cli::cli_bullets(c("*" = nist_desc))

  if (grepl("NOT IN PLACE", oversight_note)) {
    cli::cli_alert_danger(oversight_note)
  } else {
    cli::cli_alert_info(oversight_note)
  }

  gov
}
