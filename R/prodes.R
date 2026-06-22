#' @title Compute the PRODES year of the given date.
#'
#' @name compute_prodes_year
#'
#' @description
#' This function computes the corresponding PRODES year of the given date
#' (PRODES is the brazilian deforestation monitoring system). The PRODES year
#' starts on August and it takes the year of its last July.
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
        if (is.na(adate))
            return(NA)
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
#' function keeps track of the classes used in PRODES products (vector and 
#' raster) along time. 
#' @return A tibble.
#' @export
get_prodes_codes <- function() {
    # TODO: Move this to a configuration file.
    tibble::tribble(
~prodes_code, ~common_name, ~class_tif_2021, ~class_shp_2021, ~class_tif_2022, ~class_gpkg_2022,
 0L,  "Deforestation", NA,                NA,           NA,                      "DESMATAMENTO",
 0L,  "Deforestation", NA,                NA,           "d2000",                 "d2000",
 2L,  "Deforestation", NA,                NA,           NA,                      "d2002",
 4L,  "Deforestation", NA,                NA,           "d2004",                 "d2004",
 6L,  "Deforestation", NA,                NA,           "d2006",                 "d2006",
 7L,  "Deforestation", "d2007 (mascara)", "d2007",      "d2007",                 "d2007",
 8L,  "Deforestation", "d2008",           "d2008",      "d2008",                 "d2008",
 9L,  "Deforestation", "d2009",           "d2009",      "d2009",                 "d2009",
10L,  "Deforestation", "d2010",           "d2010",      "d2010",                 "d2010",
11L,  "Deforestation", "d2011",           "d2011",      "d2011",                 "d2011",
12L,  "Deforestation", "d2012",           "d2012",      "d2012",                 "d2012",
13L,  "Deforestation", "d2013",           "d2013",      "d2013",                 "d2013",
14L,  "Deforestation", "d2014",           "d2014",      "d2014",                 "d2014",
15L,  "Deforestation", "d2015",           "d2015",      "d2015",                 "d2015",
16L,  "Deforestation", "d2016",           "d2016",      "d2016",                 "d2016",
17L,  "Deforestation", "d2017",           "d2017",      "d2017",                 "d2017",
18L,  "Deforestation", "d2018",           "d2018",      "d2018",                 "d2018",
19L,  "Deforestation", "d2019",           "d2019",      "d2019",                 "d2019",
20L,  "Deforestation", "d2020",           "d2020",      "d2020",                 "d2020",
21L,  "Deforestation", "d2021",           "d2021",      "d2021",                 "d2021",
22L,  "Deforestation", NA,                NA,           "d2022",                 "d2022",
#
32L,  "Cloud",         "Nuvem",           "NUVEM_2021", NA,                      "NUVEM_2022",
32L,  NA,              NA,                NA,           NA,                      "NUVEM_2016",
32L,  NA,              NA,                NA,           NA,                      "NUVEM_2017",
32L,  NA,              NA,                NA,           NA,                      "NUVEM_2018",
32L,  NA,              NA,                NA,           NA,                      "NUVEM_2019",
32L,  NA,              NA,                NA,           NA,                      "NUVEM_2020",
32L,  NA,              NA,                NA,           NA,                      "NUVEM_2021",
32L,  NA,              NA,                NA,           NA,                      "NUVEM_2022",
#
50L,  "Residual",      NA,                 NA,           NA,                     "residuo",
50L,  "Residual",      "r2010",           "r2010",       "r2010",                "r2010",
51L,  "Residual",      "r2011",           "r2011",       "r2011",                "r2011",
52L,  "Residual",      "r2012",           "r2012",       "r2012",                "r2012",
53L,  "Residual",      "r2013",           "r2013",       "r2013",                "r2013",
54L,  "Residual",      "r2014",           "r2014",       "r2014",                "r2014",
55L,  "Residual",      "r2015",           "r2015",       "r2015",                "r2015",
56L,  "Residual",      "r2016",           "r2016",       "r2016",                "r2016",
57L,  "Residual",      "r2017",           "r2017",       "r2017",                "r2017",
58L,  "Residual",      "r2018",           "r2018",       "r2018",                "r2018",
59L,  "Residual",      "r2019",           "r2019",       "r2019",                "r2019",
60L,  "Residual",      "r2020",           "r2020",       "r2020",                "r2020",
61L,  "Residual",      "r2021",           "r2021",       "r2021",                "r2021",
62L,  "Residual",      NA,                 NA,           NA,                     "r2022",
#
91L,  "Water",         "Hidrografia",     "HIDROGRAFIA", "Hidrografia",          "HIDROGRAFIA",
91L,  NA,              NA,                NA,            NA,                     "hidrografia",
#
100L, "Forest",        "Floresta",        "FOREST_2021", "Floresta",             "FOREST_2022",
100L, NA,              NA,                NA,              NA,                   "FOREST_2016",
100L, NA,              NA,                NA,              NA,                   "FOREST_2017",
100L, NA,              NA,                NA,              NA,                   "FOREST_2018",
100L, NA,              NA,                NA,              NA,                   "FOREST_2019",
100L, NA,              NA,                NA,              NA,                   "FOREST_2020",
100L, NA,              NA,                NA,              NA,                   "FOREST_2021",
100L, NA,              NA,                NA,              NA,                   "FOREST_2022",
#
101L, "No Forest",     "NaoFloresta",     "NAO_FLORESTA",  "Nao Floresta",       "Nao Floresta",
102L, "No Forest",     NA,                "NAO_FLORESTA2", NA,                   "NAO_FLORESTA2",
103L, NA,              NA,                NA,              NA,                   "NAO_FLORESTA3",
#
    ) %>%
    return()
}



#' @title Prepare a vector with PRODES classes.
#'
#' @name get_prodes_names
#'
#' @description
#' This function returns a named vector with PRODES common names and their
#' corresponding classes from the raster and vector PRODES data. This vector
#' is useful for recoding.
#' @param code_col        A character. Name of the column with PRODES codes.
#' @param common_name_col A character. Name of the column with common PRODES
#' names.
#' @param label_col       A character. Name of teh column with labels.
#' @return A character.
#' @export
get_prodes_names <- function(code_col = "prodes_code",
                             common_name_col = "common_name",
                             label_col = "class") {

    prodes_codes <- get_prodes_codes()
    stopifnot("Columns not found in PRODES codes!" =
              all(names(prodes_codes)[1:2] %in% c(code_col, common_name_col)))
    stopifnot("At least 3 columns are needed!" = ncol(prodes_codes) > 2)
    names_tb <- tibble::tibble()
    for (i in 3:ncol(prodes_codes)) {
        tmp_tb <- prodes_codes[, c(1:2, i)]
        colnames(tmp_tb)[3] <- label_col
        names_tb <- dplyr::bind_rows(names_tb, tmp_tb)
    }
    names_tb <-
        names_tb %>%
        tidyr::drop_na() %>%
        dplyr::distinct(.data[[code_col]],
                        .data[[common_name_col]],
                        .data[[label_col]])
    names_tb %>%
        dplyr::pull(tidyselect::all_of(common_name_col)) %>%
        magrittr::set_names(names_tb[[label_col]]) %>%
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
    stopifnot("PRODES' code file not found!" = file.exists(file_path))
    file_path %>%
        readr::read_delim(delim = "|",
                          col_names = FALSE,
                          col_select = c(2, 3),
                          col_types = "ic",
                          skip = 15,
                          comment = "+---",
                          trim_ws = TRUE) %>%
        dplyr::rename(code = tidyselect::all_of("X2"),
                      class = tidyselect::all_of("X3")) %>%
        dplyr::mutate(class = stringr::str_replace_all(class,
                                                       pattern = "\\.",
                                                       replacement = " "),
                      class = stringr::str_trim(.data[["class"]]),
                      code = as.integer(.data[["code"]])) %>%
        return()
}

