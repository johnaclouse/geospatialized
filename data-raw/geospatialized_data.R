# add constant for OSMI_HC_Kendall_Goto_M1 location

GOTO_M1 <- tibble::tibble("lon" = -122.718111, "lat" = 45.508776)

usethis::use_data(GOTO_M1, overwrite = TRUE)

