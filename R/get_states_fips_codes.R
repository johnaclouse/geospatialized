#' get_state_fips_codes
#'
#' @param states character vector name of states to lookup
#'
#' Returns the FIPS codes for the requested states.
#'
#' @return character vector with numeric fips codes
#' @export
#'
#' @examples
#' get_state_fips_codes("Oregon")
get_state_fips_codes <- function(states) {
  state_name <- state_code <- NULL
  utils::data("fips_codes", package = "tidycensus")
  states <- tolower(states)
  tidycensus::fips_codes %>%
    dplyr::filter(tolower(state_name) %in% states) %>%
    dplyr::distinct(state_code) %>%
    dplyr::pull(state_code)
}
