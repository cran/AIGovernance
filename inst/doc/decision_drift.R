## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----data---------------------------------------------------------------------
library(AIGovernance)
data(hiring_panel)
round(with(hiring_panel, tapply(selected, list(group, period), mean)), 3)

## ----single-------------------------------------------------------------------
gov <- aigov_build(hiring_panel[hiring_panel$period == 2026, ],
                   selected, group, ref_group = "A")
gov <- aigov_adverse_impact(gov)

## ----drift--------------------------------------------------------------------
gov <- aigov_build(hiring_panel, selected, group, ref_group = "A")
gov <- aigov_drift(gov, period = period,
                   covariates = c("qualification", "degree"),
                   unit = recruiter, baseline = 2024, current = 2026,
                   B = 50, seed = 1)

## ----truth--------------------------------------------------------------------
tr <- attr(hiring_panel, "truth")
tr$decomposition
tr$drift[, c("group", "true_net_drift", "true_abs_drift")]

## ----categorical--------------------------------------------------------------
gov <- aigov_drift(gov, period = period,
                   covariates = c("qualification", "degree"),
                   decision = decision, focal = "advance",
                   baseline = 2024, current = 2026, B = 50, seed = 1,
                   verbose = FALSE)
d <- gov$results$drift$decomposition
d[d$group == "Overall", c("category", "raw_change", "case_mix", "rule_change", "net_drift")]

## ----monitor------------------------------------------------------------------
gov <- aigov_monitor(gov, period = period,
                     covariates = c("qualification", "degree"),
                     B = 50, seed = 1)

## ----sim----------------------------------------------------------------------
sim <- aigov_simulate_drift(n = 1500, scenario = "case_mix", seed = 3)
g <- aigov_build(sim, selected, group, ref_group = "A")
g <- aigov_drift(g, period = period, covariates = c("qualification", "degree"),
                 B = 50, seed = 1, verbose = FALSE)
g$results$drift$drift[, c("group", "net_drift", "abs_drift_bc", "alert")]

