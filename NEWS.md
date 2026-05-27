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
