#' get_geoid
#'
#' @param location data frame with columns "lon" and "lat"
#' @param acs_geometry list of acs geometries
#'
#'   If the acs_geometry is at the block group level, the returned GEOID will be
#'   the block group of the target location. Likewise, if the acs_geometry is at
#'   the census tract level, the GEOID returned will be the census tract
#'   containing the target location.
#'
#'   When a location lies on the edge of multiple geographies, only the first
#'   detected geography will be returned. If multiple geographies are detected,
#'   the \code{multiple_intersections} flag is set to \code{TRUE}.
#'
#' @return data frame containing input coordinates, GEOID for geographic region,
#'   geography name, multiple intersections flag, ACS total population, and
#'   matching geometry
#' @export
#'
#' @examples
#' \dontrun{
#' options(tigris_use_cache = TRUE)
#' get_geoid(GOTO_M1)
#' }

get_geoid <- function(location,
                      acs_geometry = get_acs_geometry()$block_groups) {


  locations <- sf::st_as_sf(location, coords = c("lon", "lat"),
                            crs = sf::st_crs(acs_geometry))


  geometry_row = ((sf::st_within(locations, acs_geometry)))

  # sapply used to extract the first sub-element only
  # prevents points on the edge of multiple polygons from producing multiple matches

  multiple_instersections <- lengths(geometry_row) > 1

  geometry_information <- acs_geometry[sapply(geometry_row, "[[", 1), ]
  geometry_information <- cbind(lon = location$lon,
                                lat = location$lat,
                                geometry_information,
                                multiple_instersections)
  input_count <- nrow(location)
  output_count <- nrow(geometry_information)

  if (input_count != output_count)
    stop(glue::glue("get_geoid received {input_count} rows but returned {output_count} rows."))

  return(geometry_information)
}

