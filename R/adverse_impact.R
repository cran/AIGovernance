#' EEOC Adverse Impact Analysis (4/5ths Rule)
#'
#' @description
#' Computes selection rates by group and applies the EEOC Uniform Guidelines
#' 4/5ths (80\%) rule to assess adverse impact in employment selection
#' procedures (EEOC, 1978).
#'
#' The \strong{Adverse Impact Ratio (AIR)} for each group is:
#' \deqn{AIR_g = \frac{\text{selection rate}_g}{\text{selection rate}_{\text{ref}}}}
#'
#' A group is flagged for adverse impact when \eqn{AIR < 0.80}.
#' The function also reports the two-proportion Z-test and Fisher's exact
#' test p-values as supplementary statistics.
#'
#' \strong{Note:} The 4/5ths rule is a rule of thumb, not a bright-line legal
#' standard. Small sample sizes reduce reliability. This function does not
#' provide legal advice.
#'
#' @param gov An \code{aigov} object from \code{\link{aigov_build}}.
#' @param min_n Integer. Minimum group sample size for a reliable AIR estimate.
#'   Groups below this threshold are flagged with a warning. Default \code{30}.
#'
#' @return The input \code{gov} object with \code{gov$results$adverse_impact}
#'   appended, a \code{tibble} containing:
#'   \describe{
#'     \item{\code{group}}{Group label.}
#'     \item{\code{n}}{Total applicants in group.}
#'     \item{\code{n_selected}}{Number selected.}
#'     \item{\code{selection_rate}}{Proportion selected.}
#'     \item{\code{AIR}}{Adverse Impact Ratio vs reference group.}
#'     \item{\code{fourfifths_flag}}{Logical: \code{TRUE} if AIR < 0.80.}
#'     \item{\code{z_stat}}{Two-proportion Z statistic.}
#'     \item{\code{p_value}}{Two-sided p-value from Z test.}
#'     \item{\code{fisher_p}}{Fisher's exact test p-value.}
#'     \item{\code{small_n_flag}}{Logical: \code{TRUE} if n < \code{min_n}.}
#'   }
#'
#' @references
#' Equal Employment Opportunity Commission (1978). Uniform guidelines on
#' employee selection procedures. \emph{Federal Register}, 43(166),
#' 38295--38309.
#'
#' @examples
#' data(hiring_sim)
#' gov <- aigov_build(hiring_sim, selected, race_ethnicity, ref_group = "White")
#' gov <- aigov_adverse_impact(gov)
#' gov$results$adverse_impact
#'
#' @export
aigov_adverse_impact <- function(gov, min_n = 30L) {

  .check_aigov(gov)

  data      <- gov$data
  outcome   <- gov$outcome
  grp_col   <- gov$group
  ref       <- gov$ref_group

  # --- selection rates -------------------------------------------------------
  rates <- data %>%
    dplyr::mutate(.grp = as.character(.data[[grp_col]])) %>%
    dplyr::group_by(.data$.grp) %>%
    dplyr::summarise(
      n          = dplyr::n(),
      n_selected = sum(.data[[outcome]], na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(
      selection_rate = .data$n_selected / .data$n
    )

  ref_rate <- rates$selection_rate[rates$.grp == ref]

  if (length(ref_rate) == 0 || is.na(ref_rate)) {
    cli::cli_abort("Reference group {.val {ref}} has no valid selection rate.")
  }

  ref_n          <- rates$n[rates$.grp == ref]
  ref_n_selected <- rates$n_selected[rates$.grp == ref]

  # --- AIR + tests -----------------------------------------------------------
  results <- rates %>%
    dplyr::rename(group = .data$.grp) %>%
    dplyr::mutate(
      AIR = ifelse(.data$group == ref, 1.0,
                   .data$selection_rate / ref_rate),
      fourfifths_flag = ifelse(.data$group == ref, FALSE,
                               .data$AIR < 0.80),
      small_n_flag    = .data$n < min_n
    )

  # two-proportion z-test and fisher for non-reference groups
  stats_list <- lapply(seq_len(nrow(results)), function(i) {
    g <- results$group[i]
    if (g == ref) return(data.frame(z_stat = NA_real_,
                                    p_value = NA_real_,
                                    fisher_p = NA_real_))
    x <- c(results$n_selected[i], ref_n_selected)
    n <- c(results$n[i], ref_n)
    p_pool <- sum(x) / sum(n)
    se     <- sqrt(p_pool * (1 - p_pool) * (1/n[1] + 1/n[2]))
    z      <- if (se > 0) (x[1]/n[1] - x[2]/n[2]) / se else NA_real_
    pz     <- if (!is.na(z)) 2 * stats::pnorm(-abs(z)) else NA_real_
    # Fisher exact
    mat    <- matrix(c(x[1], n[1]-x[1], x[2], n[2]-x[2]), nrow = 2)
    pf     <- tryCatch(stats::fisher.test(mat)$p.value, error = function(e) NA_real_)
    data.frame(z_stat = z, p_value = pz, fisher_p = pf)
  })

  stats_df <- do.call(rbind, stats_list)
  results  <- dplyr::bind_cols(results, stats_df)

  # --- store & report --------------------------------------------------------
  n_flagged <- sum(results$fourfifths_flag, na.rm = TRUE)

  verdict <- if (n_flagged == 0) "PASS" else "FAIL"

  gov$results$adverse_impact <- list(
    table   = results,
    verdict = verdict,
    n_flagged = n_flagged,
    ref_group = ref,
    ref_selection_rate = ref_rate
  )

  cli::cli_h2("EEOC Adverse Impact (4/5ths Rule)")
  print(
    results %>%
      dplyr::select("group", "n", "n_selected", "selection_rate",
                    "AIR", "fourfifths_flag", "p_value", "small_n_flag"),
    n = Inf
  )

  if (verdict == "PASS") {
    cli::cli_alert_success("Result: PASS \u2014 No group flagged for adverse impact (AIR >= 0.80 for all groups).")
  } else {
    cli::cli_alert_danger(
      "Result: FAIL \u2014 {n_flagged} group(s) flagged (AIR < 0.80)."
    )
  }

  flagged_small <- results$group[results$small_n_flag]
  if (length(flagged_small) > 0) {
    cli::cli_alert_warning(
      "Small sample warning: groups {.val {flagged_small}} have n < {min_n}. ",
      "AIR estimates may be unreliable."
    )
  }

  cli::cli_alert_info(
    "Disclaimer: The 4/5ths rule is a statistical rule of thumb. ",
    "This output does not constitute legal advice."
  )

  gov
}
