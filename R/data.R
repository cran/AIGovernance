#' Simulated Employment Screening Dataset
#'
#' @description
#' A synthetic dataset of 500 job applicants processed by a hypothetical
#' automated resume-screening tool. Generated for illustrative and testing
#' purposes only. All individuals are fictional.
#'
#' @format A data frame with 500 rows and 6 variables:
#' \describe{
#'   \item{\code{applicant_id}}{Integer applicant identifier.}
#'   \item{\code{race_ethnicity}}{Character. One of \code{"White"},
#'     \code{"Black"}, \code{"Hispanic"}, \code{"Asian"}, \code{"Other"}.}
#'   \item{\code{gender}}{Character. \code{"Male"} or \code{"Female"}.}
#'   \item{\code{years_experience}}{Numeric. Years of relevant experience.}
#'   \item{\code{score}}{Numeric. AI screening score (0--100).}
#'   \item{\code{selected}}{Integer (0/1). Whether the applicant was advanced
#'     to the next stage (1 = selected, 0 = not selected).}
#' }
#'
#' @details
#' Selection probabilities were set to produce a realistic adverse impact
#' pattern across race/ethnicity groups, consistent with published empirical
#' ranges. The data are purely synthetic and do not represent any real
#' organisation or hiring process.
#'
#' @examples
#' data(hiring_sim)
#' head(hiring_sim)
#' table(hiring_sim$race_ethnicity, hiring_sim$selected)
#'
#' @source Simulated by the package authors for illustration purposes.
"hiring_sim"

#' Simulated Multi-Period Hiring Decisions With Known Drift
#'
#' @description
#' A synthetic panel of screening decisions made by 24 recruiters over three
#' annual periods, generated with \code{\link{aigov_simulate_drift}}. It is
#' built so that a single-period audit and a drift audit tell different
#' stories: applicant qualifications fall over time (case-mix change), the
#' rule changes in opposite directions for applicants with and without a
#' degree (masked drift), stronger applicants are increasingly routed to
#' more lenient recruiters (allocation change), and the threshold between
#' "review" and "advance" rises (category shift). All records are fictional.
#'
#' @format A data frame with 4,500 rows and 8 variables:
#' \describe{
#'   \item{\code{case_id}}{Integer case identifier.}
#'   \item{\code{period}}{Integer period (2024, 2025, 2026).}
#'   \item{\code{group}}{Character group label (\code{"A"}, \code{"B"},
#'     \code{"C"}).}
#'   \item{\code{qualification}}{Numeric standardized qualification score.}
#'   \item{\code{degree}}{Integer (0/1) degree indicator.}
#'   \item{\code{recruiter}}{Character recruiter (decision unit) identifier.}
#'   \item{\code{selected}}{Integer (0/1): passed the first screen
#'     (\code{decision} is "review" or "advance").}
#'   \item{\code{decision}}{Factor: "reject", "review" or "advance".}
#' }
#'
#' @details
#' The attribute \code{"truth"} holds the true net and absolute drift between
#' 2024 and 2026 by group and the true drift of each recruiter. See
#' \code{data-raw/hiring_panel.R} in the package sources for the generating
#' call.
#'
#' @examples
#' data(hiring_panel)
#' with(hiring_panel, tapply(selected, list(group, period), mean))
#' attr(hiring_panel, "truth")$drift
#'
#' @source Simulated with \code{aigov_simulate_drift(n = 1500, n_periods = 3,
#'   n_units = 24, scenario = c("case_mix", "offsetting_cases",
#'   "reallocation", "category_shift"), categorical = TRUE, seed = 2026)}.
"hiring_panel"
