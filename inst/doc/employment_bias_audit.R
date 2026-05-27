## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----load---------------------------------------------------------------------
library(AIGovernance)

data(hiring_sim)
head(hiring_sim)

## ----summary------------------------------------------------------------------
table(hiring_sim$race_ethnicity, hiring_sim$selected)

## ----build--------------------------------------------------------------------
gov <- aigov_build(
  data        = hiring_sim,
  outcome     = selected,
  group       = race_ethnicity,
  ref_group   = "White",
  frameworks  = c("EEOC", "NYC_LL144", "NIST_RMF"),
  org_name    = "Acme Corporation",
  system_name = "ResumeAI v1.0"
)

print(gov)

## ----scope--------------------------------------------------------------------
aigov_scope(gov, domain = "employment", us_state = "NY")

## ----eeoc---------------------------------------------------------------------
gov <- aigov_adverse_impact(gov)
gov$results$adverse_impact$table

## ----nyc----------------------------------------------------------------------
gov <- aigov_audit_nyc(gov)
gov$results$nyc_ll144$disclosure_table

## ----nist---------------------------------------------------------------------
gov <- aigov_audit_nist(gov, responses = list(
  GOVERN_1_1 = TRUE,   # Risk policy documented
  GOVERN_1_2 = TRUE,   # Roles defined
  GOVERN_1_3 = FALSE,  # Leadership oversight not yet formalised
  MAP_1_1    = TRUE,   # System purpose documented
  MAP_1_2    = TRUE,   # Affected populations identified
  MAP_1_3    = TRUE,   # Regulatory requirements identified
  MAP_2_1    = TRUE,   # Harms identified
  MEASURE_1_1 = TRUE,  # Fairness metrics defined
  MANAGE_2_1  = FALSE  # Human review not yet in place
))

gov$results$nist_rmf$scores
gov$results$nist_rmf$verdict

## ----classify-----------------------------------------------------------------
gov <- aigov_classify(
  gov,
  domain               = "employment",
  makes_final_decision = TRUE,
  human_oversight      = FALSE
)

## ----report, eval=FALSE-------------------------------------------------------
# # Produces a self-contained HTML report
# aigov_report(gov, format = "html", open = TRUE)

## ----checklist----------------------------------------------------------------
aigov_checklist(gov, "NYC_LL144")

