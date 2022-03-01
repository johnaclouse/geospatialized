#' plot_geographic_areas
#'
#' Plot the state, county, census tract, and block group geographic areas containing the target_location.
#'
#' @param lon numeric longitude of target
#' @param lat numeric latitude of target
#' @param states_needed character vector containing states needed for geography geometeries
#' @param acs_geometry a list of acs_geometries from \code{get_acs_geometry()}
#'
#' @return leaflet object
#' @export
#'
#' @examples
plot_geographic_areas <- function(lon = geospatialized::GOTO_M1$lon,
                                  lat = geospatialized::GOTO_M1$lat,
                                  states_needed = "Oregon",
                                  acs_geometry = NULL) {

  GEOID <- NULL

  if(is.null(acs_geometry))
    acs_geometry <- get_acs_geometry(states_needed)


  target_block_group <- get_geoid(lon, lat, acs_geometry$block_groups)
  target_census_tract <- stringr::str_sub(as.character(target_block_group), 1, 11)
  target_county <- stringr::str_sub(as.character(target_block_group), 1, 5)
  target_state <- stringr::str_sub(as.character(target_block_group), 1, 2)

  target_sf <-
    sf::st_as_sf(data.frame(lon,lat),
                 coords = c("lon", "lat"), crs = 4326, agr = "constant")

  leaflet::leaflet() %>%
    leaflet::addProviderTiles(provider = "CartoDB.Positron") %>%
    leaflet::addPolygons(
      data =   dplyr::filter(acs_geometry$states, stringr::str_sub(GEOID, 1, 2) == target_state),
      popup = ~ paste(GEOID,  NAME),
      smoothFactor = 0,
      weight = 4,
      fillOpacity = 0,
      color = "#60a3d9") %>%
    leaflet::addPolygons(
      data =   dplyr::filter(acs_geometry$counties, stringr::str_sub(GEOID, 1, 5) == target_county),
      popup = ~ paste(GEOID,  NAME),
      smoothFactor = 0,
      weight = 3,
      fillOpacity = 0,
      color = "#0074B7") %>%
    leaflet::addPolygons(
      data =   dplyr::filter(acs_geometry$census_tracts, stringr::str_sub(GEOID, 1, 11) == target_census_tract),
      popup = ~ paste(GEOID,  NAME),
      smoothFactor = 0,
      weight = 2,
      fillOpacity = 0,
      color = "#003B73") %>%
    leaflet::addPolygons(
      data =   dplyr::filter(acs_geometry$block_groups,GEOID == target_block_group),
      popup = ~ paste(GEOID,  NAME),
      smoothFactor = 0,
      weight = 2,
      fillOpacity =0.1,
      color = "#000000",
      fillColor = "#000000") %>%
    leaflet::addCircleMarkers(data = target_sf,
                     weight = 2,
                     fillOpacity = 0,
                     color = "#000000",
                     popup = paste(lon, lat, sep = ", ")) %>%
    leaflet::setView(lon, lat, zoom = 12)
}

