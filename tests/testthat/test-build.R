## tests/testthat/test-build.R
library(testthat)
library(AIGovernance)

data(hiring_sim)

test_that("aigov_build returns aigov object", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  expect_s3_class(gov, "aigov")
  expect_equal(gov$n_total, 500L)
  expect_equal(gov$ref_group, "White")
  expect_equal(gov$outcome, "selected")
  expect_equal(gov$group, "race_ethnicity")
})

test_that("aigov_build errors on bad outcome", {
  df <- hiring_sim
  df$selected <- df$selected + 2
  expect_error(aigov_build(df, selected, race_ethnicity, ref_group = "White"))
})

test_that("aigov_build errors on missing ref_group", {
  expect_error(
    aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "Martian")
  )
})

test_that("aigov_build errors on bad framework", {
  expect_error(
    aigov_build(hiring_sim, selected, race_ethnicity,
                ref_group = "White", frameworks = "BADLAW")
  )
})

test_that("aigov_build accepts custom audit_date", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity,
                     ref_group = "White", audit_date = "2026-01-15")
  expect_equal(gov$audit_date, as.Date("2026-01-15"))
})
