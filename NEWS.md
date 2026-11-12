# AIGovernance 0.2.0

## New functions
* `aigov_drift()` audits decision drift between two periods. It decomposes the
  change in each decision rate into case-mix and rule-change components, and,
  when a decision unit is supplied (recruiter, office, judge, court), into
  case-mix, allocation and within-unit rule-change components. The components
  add to the raw change exactly. It also reports net drift and absolute
  (total-variation) drift over a fixed reference case mix, with overlap
  trimming, cluster bootstrap intervals, a bias-corrected absolute drift and
  alerts that distinguish net drift from masked drift (offsetting changes
  that leave aggregate rates unchanged). Alerts use interval nulls: an alert
  requires the confidence interval to lie beyond the practical tolerance, so
  changes plausibly within tolerance never trigger one. `net_p_tol` gives the
  matching one-sided p-value, and `aigov_monitor()` adjusts it across
  comparisons.
* Unit-level drift with delta-method standard errors, a noise-corrected
  between-unit variance, empirical-Bayes shrinkage and Benjamini-Hochberg
  q-values.
* Categorical decisions (for example reject / review / advance) through the
  new `decision` argument.
* `aigov_monitor()` runs prespecified comparisons across several periods
  (against the baseline or the previous period) with multiplicity adjustment.
* `aigov_simulate_drift()` simulates decision data from known mechanisms
  (stable, case mix, rule change, offsetting group or case changes,
  unrecorded characteristic, reallocation across units, offsetting unit
  changes, category shift) and returns the true drift targets.

## Data
* `hiring_panel` - three periods, 24 recruiters, three-category decision,
  with known drift mechanisms.

## Framework links
* Running `aigov_drift()` or `aigov_monitor()` now fills NIST AI RMF item
  MEASURE_3_1 (ongoing monitoring for bias drift) in `aigov_audit_nist()`.
  MANAGE_4_1 still requires a human review and is not set automatically.
* EU AI Act output adds Art. 72 (post-market monitoring). In
  `aigov_checklist(gov, "EU_AI_Act")` the item shows "Evidence available" once
  a drift audit has been run.
* The report's drift section states which NIST and EU AI Act items the
  analysis supports.

## Other changes
* `aigov_report()` includes decision-drift and monitoring sections when those
  modules have been run, and reports the installed package version.
* Title and description now cover AI-assisted decision systems beyond
  employment.
* Repository URLs moved to <https://github.com/subirhait/AIGovernance>.
* `utils` is now declared in Imports; `nnet` is a new import.

# AIGovernance 0.1.0

## Initial Release

### New functions
* `aigov_build()` — construct governance audit object from employment data
* `aigov_scope()` — identify applicable frameworks by domain and jurisdiction
* `aigov_adverse_impact()` — EEOC 4/5ths adverse impact rule with Z and Fisher tests
* `aigov_audit_nyc()` — NYC Local Law 144 impact ratio table and procedural checklist
* `aigov_audit_nist()` — NIST AI RMF 1.0 GOVERN/MAP/MEASURE/MANAGE scoring
* `aigov_classify()` — EU AI Act risk tier and NIST risk tier classification
* `aigov_checklist()` — display checklist items for any supported framework
* `aigov_report()` — generate self-contained HTML audit report

### Data
* `hiring_sim` — synthetic employment screening dataset (500 applicants, 5 race/ethnicity groups)

### Frameworks covered
* EEOC Uniform Guidelines (US Federal)
* NYC Local Law 144 (New York City)
* NIST AI RMF 1.0 (US)
* EU AI Act — risk classification (EU)
