# AIGovernance

**Statistical Auditing and Governance Reporting for Employment AI Systems**

[![R-CMD-check](https://github.com/causalfragility-lab/AIGovernance/workflows/R-CMD-check/badge.svg)](https://github.com/causalfragility-lab/AIGovernance/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue)](https://cran.r-project.org/)

> **Disclaimer:** `AIGovernance` provides statistical auditing and
> documentation support tools only. It does not provide legal advice and
> does not certify compliance with any law or regulation.

---

## Motivation

Organisations deploying AI systems in employment decisions face growing
regulatory requirements — from the **EEOC 4/5ths adverse impact rule** to
**NYC Local Law 144** (mandatory annual bias audits for AEDTs) to the
**EU AI Act** (High Risk classification for employment AI). Yet no
dedicated R package provides a unified statistical workflow for these
governance tasks.

`AIGovernance` fills this gap as a focused MVP for the employment domain.

---

## Frameworks Covered (v0.1.0)

| Framework | Jurisdiction | Coverage |
|---|---|---|
| **EEOC Uniform Guidelines** | US Federal | Adverse impact (4/5ths rule), Fisher & Z tests |
| **NYC Local Law 144** | New York City | Impact ratio table, disclosure format, procedural checklist |
| **NIST AI RMF 1.0** | US (voluntary) | GOVERN / MAP / MEASURE / MANAGE scoring |
| **EU AI Act** | European Union | Risk tier classification, Annex III, key obligations |

---

## Installation

```r
# From GitHub (development version)
remotes::install_github("causalfragility-lab/AIGovernance")

# From CRAN (once accepted)
install.packages("AIGovernance")
```

---

## Quick Start

```r
library(AIGovernance)

# --- 1. Built-in synthetic hiring data ---
data(hiring_sim)

# --- 2. Build governance object ---
gov <- aigov_build(
  data        = hiring_sim,
  outcome     = selected,
  group       = race_ethnicity,
  ref_group   = "White",
  frameworks  = c("EEOC", "NYC_LL144", "NIST_RMF"),
  org_name    = "Acme Corporation",
  system_name = "ResumeAI v1.0"
)

# --- 3. Check applicable laws ---
aigov_scope(gov, domain = "employment", us_state = "NY")

# --- 4. EEOC adverse impact ---
gov <- aigov_adverse_impact(gov)

# --- 5. NYC Local Law 144 ---
gov <- aigov_audit_nyc(gov)

# --- 6. NIST AI RMF ---
gov <- aigov_audit_nist(gov, responses = list(
  GOVERN_1_1 = TRUE, MAP_1_1 = TRUE, MEASURE_1_1 = TRUE
))

# --- 7. EU AI Act risk classification ---
gov <- aigov_classify(gov, domain = "employment",
                       makes_final_decision = TRUE,
                       human_oversight = FALSE)

# --- 8. Generate HTML report ---
aigov_report(gov, format = "html")
```

---

## Core Functions

| Function | Description |
|---|---|
| `aigov_build()` | Construct governance audit object |
| `aigov_scope()` | Determine applicable frameworks by domain + jurisdiction |
| `aigov_adverse_impact()` | EEOC 4/5ths rule + statistical tests |
| `aigov_audit_nyc()` | NYC Local Law 144 impact ratios + checklist |
| `aigov_audit_nist()` | NIST AI RMF 1.0 GOVERN/MAP/MEASURE/MANAGE scoring |
| `aigov_classify()` | EU AI Act risk tier + NIST risk tier |
| `aigov_checklist()` | Display checklist items for any framework |
| `aigov_report()` | Generate HTML audit report |

---

## Ecosystem

`AIGovernance` is designed to work alongside:

| Package | Role |
|---|---|
| [`decisionpaths`](https://github.com/causalfragility-lab/decisionpaths) | Longitudinal decision path construction |
| [`DecisionDrift`](https://github.com/causalfragility-lab/DecisionDrift) | Temporal drift detection in repeated AI decisions |
| [`AIBias`](https://github.com/causalfragility-lab/AIBias) | Longitudinal bias amplification analysis |

---

## Planned for v0.2.0

- `aigov_monitor()` — threshold-based drift alerts
- `aigov_remediate()` — actionable remediation suggestions
- Colorado AI Act (SB 205) module
- Illinois AI Video Interview Act module
- Integration with `AIBias` and `DecisionDrift` objects

---

## References

- EEOC (1978). Uniform Guidelines on Employee Selection Procedures.
  *Federal Register*, 43(166), 38295–38309.
- New York City Local Law 144 (2021, effective 2023).
  <https://www.nyc.gov/site/dca/about/automated-employment-decision-tools.page>
- NIST (2023). *AI Risk Management Framework (AI RMF 1.0)*.
  <https://doi.org/10.6028/NIST.AI.100-1>
- European Parliament and Council (2024). Regulation (EU) 2024/1689
  (EU AI Act). <https://digital-strategy.ec.europa.eu/en/policies/regulatory-framework-ai>

---

## Citation

```bibtex
@Manual{Hait2026AIGovernance,
  title  = {AIGovernance: Statistical Auditing and Governance Reporting
             for Employment AI Systems},
  author = {Hait, Subir},
  year   = {2026},
  note   = {R package version 0.1.0},
  url    = {https://github.com/causalfragility-lab/AIGovernance}
}
```

## Author

**Subir Hait**  
Michigan State University  
<haitsubi@msu.edu>  
ORCID: [0009-0004-9871-9677](https://orcid.org/0009-0004-9871-9677)

## License

MIT © Subir Hait
