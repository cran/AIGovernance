# Internal utilities for AIGovernance

#' @keywords internal
.check_aigov <- function(gov) {
  if (!inherits(gov, "aigov")) {
    cli::cli_abort(
      "Expected an {.cls aigov} object from {.fn aigov_build}. Got {.cls {class(gov)}}."
    )
  }
}

# re-export pipe if needed internally
`%>%` <- dplyr::`%>%`
`.data` <- rlang::.data
