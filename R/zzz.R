.onLoad <- function(libname, pkgname) {

  stopifnot("No US Census API key found. Go to https://www.census.gov/data/developers.html,
          click on the Request a Key icon, and use the tidycensus::census_api_key()
          command to install a US Census API key in the current user .Renviron" =
              Sys.getenv("CENSUS_API_KEY") != "")

  options(tigris_use_cache = TRUE)
  packageStartupMessage("The tigris_use_cache has been set to TRUE by package geospatialized")
}



