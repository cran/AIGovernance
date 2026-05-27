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
