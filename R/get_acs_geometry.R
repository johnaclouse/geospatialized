#' Title
#'
#' @param states_needed character vector of state geometries to be returned
#'
#' @return a list of lists containing shape geometry and total population at the
#'   state, county, census tract, and block group level.
#' @export
#'
#' @examples
#' \dontrun{
#' get_acs_geometry()
#' }
get_acs_geometry <- function(states_needed = "Oregon") {

  state_fips <- get_state_fips_codes(states_needed)

  acs_geometry <- list()

  # get geometry from ACS total population variable
  acs_geometry$block_groups <- tidycensus::get_acs(geography = 'block group',
                                                   variables = 'B01003_001',
                                                   state = state_fips,
                                                   geometry = TRUE) %>%
    sf::st_transform(crs = 4326)

  acs_geometry$census_tracts <- tidycensus::get_acs(geography = 'tract',
                                                    variables = 'B01003_001',
                                                    state = state_fips,
                                                    geometry = TRUE) %>%
    sf::st_transform(crs = 4326)


  acs_geometry$counties <- tidycensus::get_acs(geography = 'county',
                                               variables = 'B01003_001',
                                               state = state_fips,
                                               geometry = TRUE) %>%
    sf::st_transform(crs = 4326)

  acs_geometry$states <- tidycensus::get_acs(geography = 'state',
                                             variables = 'B01003_001',
                                             state = state_fips,
                                             geometry = TRUE) %>%
    sf::st_transform(crs = 4326)

  return(acs_geometry)
}
