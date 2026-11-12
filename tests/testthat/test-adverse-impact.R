## tests/testthat/test-adverse-impact.R
library(testthat)
library(AIGovernance)

data(hiring_sim)

test_that("aigov_adverse_impact returns expected structure", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  gov <- aigov_adverse_impact(gov)
  ai  <- gov$results$adverse_impact
  expect_named(ai, c("table","verdict","n_flagged","ref_group","ref_selection_rate"))
  expect_true(nrow(ai$table) == 5L)
  expect_true(ai$table$AIR[ai$table$group == "White"] == 1.0)
})

test_that("adverse impact correctly flags groups below 0.80", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  gov <- aigov_adverse_impact(gov)
  ai  <- gov$results$adverse_impact
  # based on seed 2026, Black/Hispanic/Other/Asian all have AIR < 0.80
  expect_true(ai$n_flagged >= 1L)
  expect_equal(ai$verdict, "FAIL")
})

test_that("adverse impact PASS when no group below threshold", {
  # construct artificial data where all groups have equal rates
  df <- data.frame(
    id       = 1:200,
    group    = rep(c("A","B","C","D"), each = 50),
    selected = c(rep(c(1,0),25), rep(c(1,0),25),
                 rep(c(1,0),25), rep(c(1,0),25))
  )
  gov <- aigov_build(df, selected, group, ref_group = "A")
  gov <- aigov_adverse_impact(gov)
  expect_equal(gov$results$adverse_impact$verdict, "PASS")
})

test_that("small_n_flag is set correctly", {
  gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
  gov <- aigov_adverse_impact(gov, min_n = 200)
  ai  <- gov$results$adverse_impact
  # All groups have n < 200 except White (~225)
  expect_true(any(ai$table$small_n_flag))
})
