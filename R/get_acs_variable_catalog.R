#' get_acs_variable_catalog
#'
#' @param year numeric effective year for ACS data
#' @param dataset character typcially set to "acs5"
#' @param label_filter character use "*" to accept all variables or a character
#'   to filter the returned variabls
#' @param concept_filter character use "*" to accept all concepts or a character
#'   value to filter the returned concepts
#'
#' @return tibble containing name, label, and concept for 27,000 + ACS fields
#' @export
#'
#' @examples
#' \dontrun{
#' View(get_acs_variable_catalog())
#' View(get_acs_variable_catalog(label_filter = "transportation", concept_filter = "vehicle"))
#' }

get_acs_variable_catalog <- function(year = 2019,
                                     dataset = "acs5",
                                     label_filter = "*",
                                     concept_filter = "*") {

  label <- concept <- NULL
  tidycensus::load_variables(year, dataset, cache = TRUE) %>%
    dplyr::filter(grepl(label_filter, label, ignore.case = TRUE)) %>%
    dplyr::filter(grepl(concept_filter, concept, ignore.case = TRUE)) %>%
    dplyr::mutate(label = stringr::str_replace_all(label, c(
      "!!" = " ",
      "Estimate" = "",
      ":" = ""
    )),
    concept = curios::sentance_case(concept)
    )
}

