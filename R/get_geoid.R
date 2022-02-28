#' get_geoid
#'
#' @param lon numeric longitude of target location
#' @param lat numeric latitude of target location
#' @param acs_geometry list of acs geometries
#'
#' If the acs_geometry is at the block group level, the returned GEOID will be
#' the block group of the target location. Likewise, if the acs_geometry is at
#' the census tract level, the GEOID returned will be the census tract
#' containing the target location.
#'
#' @return charcter value of GEOID (FIPS code)
#' @export
#'
#' @examples
#' str(get_geoid(lon = GOTO_M1$lon, lat = GOTO_M1$lat))

get_geoid <- function(lon,
                      lat,
                      acs_geometry = get_acs_geometry()$block_groups
) {

  pt <- data.frame(lon, lat) %>%
    sf::st_as_sf(coords = c("lon", "lat"),
                 crs = sf::st_crs(acs_geometry))
  # ensure only one row is returned
  census_row = unlist(sf::st_within(pt, acs_geometry))[1]

  return(acs_geometry[census_row, ]$GEOID)
}
