#' Generate an AI Governance Audit Report
#'
#' @description
#' Produces a self-contained HTML (or plain-text) governance audit report
#' from a completed \code{aigov} object. The report is suitable for internal
#' documentation, legal review, or public disclosure (e.g., NYC LL144 website
#' posting requirement).
#'
#' @param gov A completed \code{aigov} object (at least one audit module run).
#' @param format Character. Output format: \code{"html"} (default) or
#'   \code{"text"}.
#' @param output_file Optional character. File path for the output. If
#'   \code{NULL}, a temporary file is used and opened in the browser (HTML) or
#'   printed to console (text).
#' @param open Logical. If \code{TRUE} (default) and \code{format = "html"},
#'   attempt to open the report in the default browser.
#'
#' @return Invisibly returns the path to the generated file.
#'
#' @examples
#' \dontrun{
#' data(hiring_sim)
#' gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
#' gov <- aigov_adverse_impact(gov)
#' gov <- aigov_audit_nyc(gov)
#' gov <- aigov_audit_nist(gov)
#' aigov_report(gov, format = "html")
#' }
#'
#' @export
aigov_report <- function(gov,
                          format      = "html",
                          output_file = NULL,
                          open        = TRUE) {

  .check_aigov(gov)

  if (!format %in% c("html", "text")) {
    cli::cli_abort("{.arg format} must be 'html' or 'text'.")
  }

  if (format == "text") {
    .report_text(gov)
    return(invisible(NULL))
  }

  # ---- build HTML -----------------------------------------------------------
  html <- .build_html_report(gov)

  if (is.null(output_file)) {
    output_file <- tempfile(
      pattern = paste0("AIGovernance_report_",
                       format(gov$audit_date, "%Y%m%d"), "_"),
      fileext = ".html"
    )
  }

  writeLines(html, output_file)
  cli::cli_alert_success("Report written to: {.file {output_file}}")

  if (open && interactive()) {
    tryCatch(utils::browseURL(output_file), error = function(e) NULL)
  }

  invisible(output_file)
}

# ============================================================================ #
# internal helpers                                                              #
# ============================================================================ #

.build_html_report <- function(gov) {

  ai   <- gov$results$adverse_impact
  nyc  <- gov$results$nyc_ll144
  nist <- gov$results$nist_rmf
  rc   <- gov$results$risk_class
  dr   <- gov$results$drift
  mon  <- gov$results$monitor

  # helper: badge
  badge <- function(v, pass_label = "PASS", fail_label = "FAIL") {
    if (is.null(v)) return("<span style='color:gray'>NOT RUN</span>")
    col <- switch(v,
      PASS    = "#2a9d8f", GREEN = "#2a9d8f",
      AMBER   = "#e9c46a",
      FAIL    = "#e76f51", RED   = "#e76f51", REVIEW = "#e76f51",
      "gray"
    )
    sprintf("<span style='background:%s;color:white;padding:2px 8px;
             border-radius:4px;font-weight:bold;'>%s</span>", col, v)
  }

  tbl_html <- function(df) {
    if (is.null(df) || nrow(df) == 0) return("<p><em>No data.</em></p>")
    hdr <- paste0("<th style='padding:6px 10px;background:#264653;color:white'>",
                  names(df), "</th>", collapse = "")
    rows <- apply(df, 1, function(r) {
      cells <- paste0("<td style='padding:5px 10px;border-bottom:1px solid #eee'>",
                      r, "</td>", collapse = "")
      paste0("<tr>", cells, "</tr>")
    })
    paste0(
      "<table style='border-collapse:collapse;width:100%;font-size:13px'>",
      "<thead><tr>", hdr, "</tr></thead><tbody>",
      paste(rows, collapse = ""), "</tbody></table>"
    )
  }

  # sections
  sec_ai <- if (!is.null(ai)) {
    paste0(
      "<h2>2. EEOC Adverse Impact (4/5ths Rule)</h2>",
      "<p>Verdict: ", badge(ai$verdict), "</p>",
      "<p>Reference group: <strong>", ai$ref_group, "</strong> &mdash; ",
      "Selection rate: <strong>", round(ai$ref_selection_rate, 4), "</strong></p>",
      tbl_html(
        ai$table[, c("group","n","n_selected","selection_rate","AIR",
                     "fourfifths_flag","p_value")]
      )
    )
  } else "<h2>2. EEOC Adverse Impact</h2><p><em>Module not run.</em></p>"

  sec_nyc <- if (!is.null(nyc)) {
    paste0(
      "<h2>3. NYC Local Law 144 Bias Audit</h2>",
      "<p>Verdict: ", badge(nyc$verdict), "</p>",
      "<p>Most-selected category: <strong>",
      nyc$most_selected_group, "</strong></p>",
      tbl_html(nyc$disclosure_table),
      "<h3>Procedural Checklist</h3>",
      {
        items_html <- paste0(
          vapply(names(nyc$checklist), function(item) {
            v    <- nyc$checklist[item]
            icon <- if (isTRUE(v)) "&#10004;" else
                      if (isFALSE(v)) "&#10008;" else "?"
            col  <- if (isTRUE(v)) "#2a9d8f" else
                      if (isFALSE(v)) "#e76f51" else "#888"
            sprintf("<li style='color:%s'>%s %s</li>", col, icon, item)
          }, character(1)),
          collapse = ""
        )
        paste0("<ul>", items_html, "</ul>")
      }
    )
  } else "<h2>3. NYC Local Law 144</h2><p><em>Module not run.</em></p>"

  sec_nist <- if (!is.null(nist)) {
    score_rows <- paste0(
      vapply(names(nist$scores), function(fn) {
        sc  <- round(nist$scores[fn], 2)
        col <- if (!is.na(sc) && sc >= 0.75) "#2a9d8f" else
                 if (!is.na(sc) && sc >= 0.50) "#e9c46a" else "#e76f51"
        sprintf(
          "<tr><td style='padding:5px 10px'>%s</td>
           <td style='padding:5px 10px;color:%s;font-weight:bold'>%s</td></tr>",
          fn, col, sc
        )
      }, character(1)),
      collapse = ""
    )
    paste0(
      "<h2>4. NIST AI RMF 1.0</h2>",
      "<p>Overall score: <strong>", round(nist$overall_score, 2), "</strong> &mdash; ",
      badge(nist$verdict), "</p>",
      "<table style='border-collapse:collapse;font-size:13px'>",
      "<thead><tr><th style='padding:6px 10px;background:#264653;color:white'>Function</th>",
      "<th style='padding:6px 10px;background:#264653;color:white'>Score</th></tr></thead>",
      "<tbody>", score_rows, "</tbody></table>"
    )
  } else "<h2>4. NIST AI RMF</h2><p><em>Module not run.</em></p>"

  sec_class <- if (!is.null(rc)) {
    obs_list <- paste0("<li>", rc$eu$obligations, "</li>", collapse = "")
    paste0(
      "<h2>5. Risk Classification</h2>",
      "<p><strong>EU AI Act:</strong> ", rc$eu$tier, "<br>",
      rc$eu$annex, "</p>",
      "<p><strong>Key obligations:</strong></p><ul>", obs_list, "</ul>",
      "<p><strong>NIST AI RMF:</strong> ", rc$nist$description, "</p>",
      "<p>", rc$oversight_note, "</p>"
    )
  } else "<h2>5. Risk Classification</h2><p><em>Module not run.</em></p>"

  rnd <- function(df) {
    df <- as.data.frame(df)
    for (nm in names(df)) if (is.numeric(df[[nm]])) df[[nm]] <- round(df[[nm]], 4)
    df
  }

  sec_drift <- if (!is.null(dr)) {
    s <- dr$settings
    dec <- dr$decomposition[dr$decomposition$category == s$focal, ]
    keep <- intersect(c("group", "rate_baseline", "rate_current", "raw_change",
                        "case_mix", "allocation", "rule_change"), names(dec))
    unit_html <- if (!is.null(dr$units)) {
      u <- dr$units$summary
      paste0(
        "<h3>Unit-level drift</h3><p>Units analysed: <strong>", u$n_units,
        "</strong>; noise-corrected between-unit SD of drift: <strong>",
        round(u$tau, 4), "</strong>; units flagged: <strong>", u$n_flagged,
        "</strong>.</p>"
      )
    } else ""
    paste0(
      "<h2>6. Decision Drift Audit</h2>",
      "<p>Verdict: ", badge(dr$verdict), "</p>",
      "<p>Periods: <strong>", s$baseline, "</strong> (baseline) vs <strong>",
      s$current, "</strong>. Covariates: ", paste(s$covariates, collapse = ", "),
      ". Reference case mix: ", s$reference, ". Tolerance: ", s$tolerance,
      ". Focal decision: ", s$focal, ".</p>",
      "<h3>Decomposition of the change in the focal decision rate</h3>",
      tbl_html(rnd(dec[, keep])),
      "<h3>Net and absolute drift</h3>",
      tbl_html(rnd(dr$drift[, c("group", "net_drift", "net_lo", "net_hi",
                                "abs_drift_bc", "abs_lo", "alert")])),
      unit_html,
      "<p><strong>Framework links:</strong> this analysis supports ",
      "documentation for NIST AI RMF MEASURE 3.1 (ongoing monitoring for ",
      "bias drift) and MANAGE 4.1 (periodic review of monitoring results), ",
      "and for EU AI Act Art. 72 (post-market monitoring). It does not by ",
      "itself establish that a monitoring system or review process exists.</p>",
      "<p><em>Drift is measured conditional on recorded characteristics. ",
      "It does not identify the causal effect of an AI system or of human ",
      "discretion. An alert prompts review of rule changes, data coding, ",
      "model versions and override practice.</em></p>"
    )
  } else ""

  sec_mon <- if (!is.null(mon)) {
    paste0(
      "<h2>7. Drift Monitoring Across Periods</h2>",
      "<p>Verdict: ", badge(mon$verdict), " &mdash; design: ", mon$compare,
      "; adjustment: ", mon$adjust, ".</p>",
      tbl_html(rnd(mon$table[, c("comparison", "group", "net_drift",
                                 "net_p_tol_adj", "abs_drift_bc", "alert_adj")]))
    )
  } else ""

  # assemble
  paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>AIGovernance Audit Report</title>
<style>
  body{font-family:Georgia,serif;max-width:900px;margin:40px auto;
       padding:0 20px;color:#222;line-height:1.6}
  h1{background:#264653;color:white;padding:18px 24px;border-radius:6px}
  h2{color:#264653;border-bottom:2px solid #264653;padding-bottom:4px;margin-top:36px}
  h3{color:#457b9d}
  .meta{background:#f1f1f1;padding:14px 20px;border-radius:6px;font-size:14px}
  .disclaimer{background:#fff3cd;border-left:4px solid #e9c46a;
               padding:12px 16px;font-size:13px;margin-top:40px}
  table{margin:12px 0}
</style>
</head>
<body>
<h1>AIGovernance Audit Report</h1>
<div class="meta">
  <strong>Organisation:</strong> ', gov$org_name, '<br>
  <strong>AI system:</strong> ', gov$system_name, '<br>
  <strong>Audit date:</strong> ', as.character(gov$audit_date), '<br>
  <strong>Records audited:</strong> ', gov$n_total, '<br>
  <strong>Outcome variable:</strong> ', gov$outcome, '<br>
  <strong>Group variable:</strong> ', gov$group, ' (', length(gov$group_levels),
  ' categories)<br>
  <strong>Reference group:</strong> ', gov$ref_group, '<br>
  <strong>Frameworks:</strong> ', paste(gov$frameworks, collapse = ", "), '
</div>

<h2>1. Scope</h2>
<p>This report documents the results of a statistical governance audit
conducted using the <strong>AIGovernance</strong> R package.</p>

', sec_ai, '
', sec_nyc, '
', sec_nist, '
', sec_class, '
', sec_drift, '
', sec_mon, '

<div class="disclaimer">
<strong>Disclaimer:</strong> This report was generated by the
<strong>AIGovernance</strong> R package (Hait, ', format(Sys.Date(), "%Y"), ').
It is a statistical and documentation support tool only. It does not
constitute legal advice and does not certify compliance with any law or
regulation. Users should seek qualified legal counsel for compliance
determinations.
</div>

<p style="font-size:12px;color:#aaa;margin-top:30px">
Generated: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ' |
AIGovernance v', as.character(utils::packageVersion("AIGovernance")), ' | R package by Subir Hait, Michigan State University
</p>
</body></html>')
}

.report_text <- function(gov) {
  cat("\n====================================================\n")
  cat("  AIGovernance Audit Report\n")
  cat("====================================================\n")
  cat("Organisation :", gov$org_name, "\n")
  cat("AI system    :", gov$system_name, "\n")
  cat("Audit date   :", as.character(gov$audit_date), "\n")
  cat("Records      :", gov$n_total, "\n")
  cat("Frameworks   :", paste(gov$frameworks, collapse = ", "), "\n")
  cat("----------------------------------------------------\n")
  summary(gov)
  cat("\nDisclaimer: Statistical support tool only. Not legal advice.\n")
}
