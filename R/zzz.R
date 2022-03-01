.onLoad <- function(libname, pkgname) {

  if (Sys.getenv("CENSUS_API_KEY") == "")
    packageStartupMessage("A US Census API key is required. Go to https://www.census.gov/data/developers.html,
          click on the Request a Key icon, and use the tidycensus::census_api_key()
          command to install a US Census API key in the current user .Renviron")

  if (is.null(getOption("tigris_use_cache")))
    packageStartupMessage("Use options(tigris_use_cache = TRUE) to improve performance.")
}



