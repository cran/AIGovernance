## tests/testthat/test-drift.R
library(testthat)
library(AIGovernance)

covs <- c("qualification", "degree")

build <- function(d) suppressMessages(aigov_build(d, selected, group, ref_group = "A"))

# ---- simulator ----------------------------------------------------------------

test_that("simulator returns data and truth", {
  sim <- aigov_simulate_drift(n = 300, n_units = 6, scenario = "rule", seed = 1)
  expect_s3_class(sim, "data.frame")
  expect_equal(nrow(sim), 600L)
  tr <- attr(sim, "truth")
  expect_true(all(c("drift", "units") %in% names(tr)))
  expect_true(tr$drift$true_net_drift[tr$drift$group == "Overall"] < 0)
  expect_equal(nrow(tr$units), 6L)
})

test_that("simulator: stable has zero true drift; validation works", {
  sim <- aigov_simulate_drift(n = 200, n_units = 4, scenario = "stable", seed = 2)
  tr <- attr(sim, "truth")$drift
  expect_equal(tr$true_net_drift, rep(0, 4), tolerance = 1e-12)
  expect_equal(tr$true_abs_drift, rep(0, 4), tolerance = 1e-12)
  expect_error(aigov_simulate_drift(scenario = "nonsense"))
  expect_error(aigov_simulate_drift(scenario = "category_shift"))
})

# ---- decomposition identity ---------------------------------------------------

test_that("single-level components add to the raw change", {
  sim <- aigov_simulate_drift(n = 800, n_units = 8, scenario = c("case_mix", "rule"), seed = 3)
  gov <- aigov_drift(build(sim), period = period, covariates = covs,
                     B = 0, verbose = FALSE)
  d <- gov$results$drift$decomposition
  expect_false("allocation" %in% names(d))
  expect_lt(max(abs(d$raw_change - d$case_mix - d$rule_change)), 1e-6)
})

test_that("multilevel components add to the raw change", {
  sim <- aigov_simulate_drift(n = 4000, n_units = 20, scenario = "reallocation", seed = 12)
  gov <- aigov_drift(build(sim), period = period, covariates = covs,
                     unit = recruiter, B = 0, verbose = FALSE)
  d <- gov$results$drift$decomposition
  expect_lt(max(abs(d$raw_change - d$case_mix - d$allocation - d$rule_change)), 1e-6)
  ov <- d[d$group == "Overall" & d$category == "1", ]
  tr <- attr(sim, "truth")$decomposition
  tr <- tr[tr$group == "Overall", ]
  # reallocation is attributed to allocation, not to within-unit rule change
  expect_gt(tr$true_allocation, 0.01)
  expect_equal(tr$true_rule_change, 0, tolerance = 1e-12)
  expect_lt(abs(ov$allocation - tr$true_allocation), 0.03)
  expect_lt(abs(ov$rule_change - tr$true_rule_change), 0.03)
})

test_that("categorical components add to the raw change and sum to zero", {
  sim <- aigov_simulate_drift(n = 1000, n_units = 8, scenario = "category_shift",
                              categorical = TRUE, seed = 5)
  gov <- aigov_drift(build(sim), period = period, covariates = covs,
                     decision = decision, focal = "advance",
                     B = 0, verbose = FALSE)
  d <- gov$results$drift$decomposition
  expect_setequal(unique(d$category), c("reject", "review", "advance"))
  expect_lt(max(abs(d$raw_change - d$case_mix - d$rule_change)), 1e-4)
  ov <- d[d$group == "Overall", ]
  expect_equal(sum(ov$raw_change), 0, tolerance = 1e-8)
  expect_equal(sum(ov$net_drift), 0, tolerance = 1e-8)
  expect_lt(ov$net_drift[ov$category == "advance"], 0)
})

# ---- detection ---------------------------------------------------------------

test_that("offsetting case-level changes are flagged as masked drift", {
  sim <- aigov_simulate_drift(n = 2000, n_units = 10, scenario = "offsetting_cases",
                              effect = 1, seed = 6)
  gov <- aigov_drift(build(sim), period = period, covariates = covs,
                     B = 40, seed = 1, verbose = FALSE)
  dr <- gov$results$drift$drift
  ov <- dr[dr$group == "Overall", ]
  truth <- attr(sim, "truth")$drift
  expect_lt(abs(ov$net_drift), 0.05)
  expect_equal(ov$abs_drift, truth$true_abs_drift[truth$group == "Overall"],
               tolerance = 0.03)
  expect_equal(ov$alert, "masked drift")
  expect_equal(gov$results$drift$verdict, "REVIEW")
})

test_that("pure case-mix change does not trigger an alert", {
  sim <- aigov_simulate_drift(n = 2000, n_units = 10, scenario = "case_mix", seed = 7)
  gov <- aigov_drift(build(sim), period = period, covariates = covs,
                     B = 40, seed = 1, verbose = FALSE)
  dr <- gov$results$drift
  ov <- dr$decomposition[dr$decomposition$group == "Overall" &
                           dr$decomposition$category == "1", ]
  expect_lt(ov$raw_change, -0.03)
  expect_lt(abs(ov$rule_change), abs(ov$case_mix))
  expect_true(all(dr$drift$alert == "none"))
})

test_that("unit-level drift detects offsetting units", {
  sim <- aigov_simulate_drift(n = 3000, n_units = 12, scenario = "unit_offsetting",
                              effect = 1.2, seed = 8)
  gov <- aigov_drift(build(sim), period = period, covariates = covs,
                     unit = recruiter, B = 0, verbose = FALSE)
  u <- gov$results$drift$units
  expect_equal(u$summary$n_units, 12L)
  expect_gt(u$summary$tau, 0.05)
  m <- merge(u$table, attr(sim, "truth")$units, by = "unit")
  expect_gt(stats::cor(m$drift, m$true_unit_drift), 0.8)
  expect_gt(u$summary$n_flagged, 0)
})

# ---- monitor -----------------------------------------------------------------

test_that("aigov_monitor runs comparisons and adjusts p-values", {
  sim <- aigov_simulate_drift(n = 600, n_periods = 3, n_units = 6,
                              scenario = "rule", seed = 9)
  gov <- suppressMessages(aigov_monitor(build(sim), period = period,
                                        covariates = covs, B = 20, seed = 1,
                                        verbose = FALSE))
  tab <- gov$results$monitor$table
  expect_setequal(unique(tab$comparison), c("2024 -> 2025", "2024 -> 2026"))
  ok <- !is.na(tab$net_p)
  expect_true(all(tab$net_p_adj[ok] >= tab$net_p[ok] - 1e-12))
  gov2 <- suppressMessages(aigov_monitor(build(sim), period = period,
                                         covariates = covs, compare = "previous",
                                         B = 0, verbose = FALSE))
  expect_setequal(unique(gov2$results$monitor$table$comparison),
                  c("2024 -> 2025", "2025 -> 2026"))
})

# ---- input checks and report --------------------------------------------------

test_that("aigov_drift validates inputs", {
  sim <- aigov_simulate_drift(n = 200, n_units = 4, seed = 10)
  gov <- build(sim)
  expect_error(aigov_drift(gov, period = period, covariates = "nope", B = 0, verbose = FALSE))
  expect_error(aigov_drift(gov, period = period, covariates = covs,
                           baseline = 2024, current = 2024, B = 0, verbose = FALSE))
  expect_error(aigov_drift(gov, period = period, covariates = c("group"), B = 0, verbose = FALSE))
  expect_error(aigov_drift(gov, covariates = covs, B = 0, verbose = FALSE))
})

test_that("report includes the drift section", {
  data(hiring_panel)
  gov <- build(hiring_panel)
  gov <- aigov_drift(gov, period = "period", covariates = covs,
                     baseline = 2024, current = 2026, B = 0, verbose = FALSE)
  f <- tempfile(fileext = ".html")
  aigov_report(gov, format = "html", output_file = f, open = FALSE)
  html <- paste(readLines(f), collapse = "\n")
  expect_match(html, "Decision Drift Audit")
})

test_that("drift results feed NIST MEASURE_3_1 and EU Art. 72", {
  data(hiring_panel)
  gov <- build(hiring_panel)
  gov <- aigov_drift(gov, period = period, covariates = covs,
                     baseline = 2024, current = 2026, B = 0, verbose = FALSE)
  gov <- suppressMessages(aigov_audit_nist(gov))
  cl <- gov$results$nist_rmf$checklist
  expect_true(isTRUE(cl$MEASURE_3_1$status))
  expect_false(isTRUE(cl$MANAGE_4_1$status))
  gov2 <- suppressMessages(aigov_audit_nist(gov, responses = list(MEASURE_3_1 = FALSE)))
  expect_false(isTRUE(gov2$results$nist_rmf$checklist$MEASURE_3_1$status))
  eu <- suppressMessages(aigov_checklist(gov, "EU_AI_Act"))
  expect_equal(eu$status[eu$item_id == "EU_72"], "Evidence available")
  rc <- suppressMessages(aigov_classify(gov, domain = "employment"))$results$risk_class
  expect_true(any(grepl("Art. 72", rc$eu$obligations)))
})

test_that("no false net-drift alert under stability (interval null)", {
  sim <- aigov_simulate_drift(n = 1500, n_units = 10, scenario = "stable", seed = 21)
  gov <- aigov_drift(build(sim), period = period, covariates = covs,
                     B = 40, seed = 1, verbose = FALSE)
  dr <- gov$results$drift$drift
  expect_true(all(dr$alert == "none"))
  expect_true(all(dr$net_p_tol >= 0 & dr$net_p_tol <= 1, na.rm = TRUE))
})
