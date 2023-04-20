#' @title Compute the PRODES year of the given date.
#'
#' @name compute_prodes_year
#'
#' @description
#' This function computes the corresponding PRODES (the brazilian deforestation
#' monitoring system) year of the given date. The #' PRODES year usually goes
#' starts on the August the first of each year.
#' @param adate A date vector.
#' @param start_month A character. The month (number) of the first month of
#' a PRODES year.
#' @param start_day A character. The day (number) of the first day of a PRODES
#' year.
#' @return An integer.
#' @export
compute_prodes_year <- function(adate, start_month = "08", start_day = "01") {
    stopifnot("PRODES month length should be 1" = length(start_month) == 1)
    stopifnot("PRODES day length should be 1" = length(start_day) == 1)
    if (length(adate) > 1) {
        return(
            vapply(adate, FUN = compute_prodes_year,
                   FUN.VALUE = integer(1))
        )
    } else if (length(adate) == 1) {
        date_year <- lubridate::year(adate)
        prodes_start <- as.Date(paste(date_year, start_month, start_day,
                                      sep = "-"))
        if (adate >= prodes_start) {
            return(as.integer(date_year + 1))
        } else {
            return(as.integer(date_year))
        }
    } else {
        stop("Invalid length of PRODES date!")
    }
}


#' @title Get the PRODES codes.
#'
#' @name the_mode
#'
#' @description
#' This function returns the numeric codes using for the PRODES classes. This
#' function keeps track of the classes used in PRODES products along the years
#' (raster and vector).
#' @return A tibble.
#' @export
get_prodes_codes <- function() {
    # TODO: Move this to a configuration file.
    tibble::tribble(
        ~prodes_code, ~class_tif_2021,   ~class_shp_2021, ~label_eng_2021,
          7L,         "d2007 (mascara)", "d2007",         "Deforestation",
          8L,         "d2008",           "d2008",         "Deforestation",
          9L,         "d2009",           "d2009",         "Deforestation",
         10L,         "d2010",           "d2010",         "Deforestation",
         11L,         "d2011",           "d2011",         "Deforestation",
         12L,         "d2012",           "d2012",         "Deforestation",
         13L,         "d2013",           "d2013",         "Deforestation",
         14L,         "d2014",           "d2014",         "Deforestation",
         15L,         "d2015",           "d2015",         "Deforestation",
         16L,         "d2016",           "d2016",         "Deforestation",
         17L,         "d2017",           "d2017",         "Deforestation",
         18L,         "d2018",           "d2018",         "Deforestation",
         19L,         "d2019",           "d2019",         "Deforestation",
         20L,         "d2020",           "d2020",         "Deforestation",
         21L,         "d2021",           "d2021",         "Deforestation",
        #
         32L,         "Nuvem",           "NUVEM_2021",    "Cloud",
        #
         50L,         "r2010",           "r2010",         "Residual",
         51L,         "r2011",           "r2011",         "Residual",
         52L,         "r2012",           "r2012",         "Residual",
         53L,         "r2013",           "r2013",         "Residual",
         54L,         "r2014",           "r2014",         "Residual",
         55L,         "r2015",           "r2015",         "Residual",
         56L,         "r2016",           "r2016",         "Residual",
         57L,         "r2017",           "r2017",         "Residual",
         58L,         "r2018",           "r2018",         "Residual",
         59L,         "r2019",           "r2019",         "Residual",
         60L,         "r2020",           "r2020",         "Residual",
         61L,         "r2021",           "r2021",         "Residual",
        #
         91L,         "Hidrografia",     "HIDROGRAFIA",   "Water",
        #
         100L,        "Floresta",        "FOREST_2021",   "forest",
         101L,        "NaoFloresta",     "NAO_FLORESTA",  "No Forest",
         102L,        NA,                "NAO_FLORESTA2", "No Forest"
    ) %>%
    return()
}


#' @title Extract the PRODES codes from text file
#'
#' @name process_prodes_codes
#'
#' @description
#' This function extracts the codes in the text file associated to the PRODES
#' raster of a given year.
#' @param file_path Path to the text file downloaded along with the PRODES
#'                  raster.
#' @return A tibble with two columns: code, and class.
#' @export
process_prodes_codes <- function(file_path) {
    X2 <- X3 <- code <- NULL
    stopifnot("PRODES' code file not found!" = file.exists(file_path))
    file_path %>%
    readr::read_delim(delim = "|",
                      col_names = FALSE,
                      col_select = c(2, 3),
                      col_types = "ic",
                      skip = 15,
                      comment = "+---",
                      trim_ws = TRUE) %>%
    dplyr::rename(code = X2, class = X3) %>%
    dplyr::mutate(class = stringr::str_replace_all(class,
                                                   pattern = "\\.",
                                                   replacement = " "),
                  class = stringr::str_trim(class),
                  code = as.integer(code)) %>%
    return()
}

