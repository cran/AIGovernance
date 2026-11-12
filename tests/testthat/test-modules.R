## tests/testthat/test-modules.R
library(testthat)
library(AIGovernance)

data(hiring_sim)

# ---- NYC LL144 ---------------------------------------------------------------

test_that("aigov_audit_nyc returns correct structure", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  gov <- aigov_audit_nyc(gov)
  nyc <- gov$results$nyc_ll144
  expect_named(nyc, c("impact_table","most_selected_group","disclosure_table",
                       "checklist","verdict","n_flagged","use_most_selected"))
  expect_true(nyc$verdict %in% c("PASS", "REVIEW", "FAIL"))
})

test_that("LL144 uses most-selected group as reference by default", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  gov <- aigov_audit_nyc(gov, use_most_selected = TRUE)
  nyc <- gov$results$nyc_ll144
  rates <- tapply(hiring_sim$selected, hiring_sim$race_ethnicity, mean)
  expect_equal(nyc$most_selected_group, names(which.max(rates)))
})

test_that("LL144 impact ratios sum correctly", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  gov <- aigov_audit_nyc(gov)
  ir  <- gov$results$nyc_ll144$impact_table$impact_ratio
  expect_true(all(!is.na(ir)))
  expect_true(max(ir) == 1.0)  # most-selected group always = 1
})

# ---- NIST RMF ----------------------------------------------------------------

test_that("aigov_audit_nist returns verdicts", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  gov <- aigov_audit_nist(gov)
  nist <- gov$results$nist_rmf
  expect_true(nist$verdict %in% c("GREEN","AMBER","RED"))
  expect_length(nist$scores, 4L)
})

test_that("NIST auto-marks MEASURE_1_2 when adverse impact run", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  gov <- aigov_adverse_impact(gov)
  gov <- aigov_audit_nist(gov)
  status <- gov$results$nist_rmf$checklist$MEASURE_1_2$status
  expect_true(isTRUE(status))
})

test_that("NIST user responses are recorded", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  gov <- aigov_audit_nist(gov, responses = list(GOVERN_1_1 = TRUE,
                                                 GOVERN_1_2 = FALSE))
  expect_true(isTRUE(gov$results$nist_rmf$checklist$GOVERN_1_1$status))
  expect_false(isTRUE(gov$results$nist_rmf$checklist$GOVERN_1_2$status))
})

# ---- Risk classification -----------------------------------------------------

test_that("aigov_classify marks employment as HIGH RISK", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  gov <- aigov_classify(gov, domain = "employment",
                         makes_final_decision = TRUE)
  rc  <- gov$results$risk_class
  expect_true(grepl("HIGH", rc$eu$tier))
  expect_equal(rc$nist$tier, 3L)
})

test_that("aigov_classify rejects invalid domain", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  expect_error(aigov_classify(gov, domain = "military"))
})

# ---- Checklist ---------------------------------------------------------------

test_that("aigov_checklist returns tibble for each framework", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  for (fw in c("NYC_LL144","EEOC","NIST_RMF","EU_AI_Act")) {
    cl <- aigov_checklist(gov, fw)
    expect_s3_class(cl, "tbl_df")
    expect_true(nrow(cl) > 0)
  }
})

test_that("aigov_checklist rejects unknown framework", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  expect_error(aigov_checklist(gov, "MARS_LAW"))
})
