library(dplyr)
library(ensurer)
library(tidyr)
library(stringr)
library(purrr)
library(janitor)

###############################################################################
# PROCESS THE INVENTORY DATA
#------------------------------------------------------------------------------
# The data is composed of plots. Each plot has 2 files: a SHP and a CSV. The
# SHP contains the perimeter (a square) while the CSV contais data about each
# tree.
#------------------------------------------------------------------------------
# NOTE:
# - The trees' coordinates are given using UTM without zone; the zone is
#   available in each SHP.
###############################################################################


#---- Setup ----

# TODO: Download data again and test without resplacing semi-colons

# NOTE: Replace semi-colons with commas in these two files:
# sed -i 's/;/,/g' FN_A01_2015_Inventory.csv
# sed -i 's/;/,/g' SFX_A03_2012_Inventory.csv

base_dir <- "~/Documents/data/sustainable_landscapes_brazil/Forest_Inventory_Brazil_2007"



#---- Utilitary functions ----

#' Get the CRS of a file with vector geographic information.
#' @param file_path A path to a file.
#' @return          A CRS object.
get_crs <- function(file_path) {
    file_path %>%
        sf::read_sf() %>%
        sf::st_crs() %>%
        return()
}

#' Check that coordinate columns exist and that the aren't NAs.
#' @param x A tibble.
#' @param lon A character. Name of the longitude column.
#' @param lat A character. Name of the latitude column.
#' @return    A tibble.
drop_na_coords <- function(x, lon, lat) {
    stopifnot("Columns not found!" = all(c(lon, lat) %in% colnames(x)))
    x %>%
        tidyr::drop_na(tidyselect::any_of(c(lon, lat))) %>%
    return()
}


#---- Process the plot files (SHPs) ----

plot_files <-
    base_dir %>%
    file.path("data") %>%
    ensurer::ensure_that(dir.exists(.),
                         err_desc = "Missing directory!") %>%
    list.files(pattern = "*.shp$",
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(plot_path = value) %>%
    dplyr::mutate(
        plot_id = tools::file_path_sans_ext(basename(plot_path))
    ) %>%
    tidyr::separate(col = plot_id,
                    into = c("site", "subsite", "start", "end",
                             "inventory", "plot"),
                    remove = FALSE) %>%
    dplyr::mutate(end = stringr::str_match(end, pattern = "[0-9]{4}")[,1],
                  start = as.numeric(start),
                  end = as.numeric(end)) %>%
    dplyr::select(-inventory, -plot) %>%
    dplyr::mutate(plot_crs = purrr::map(plot_path, get_crs))



#---- Process the inventory files (CSVs) ----


inventory_files <-
    base_dir %>%
    file.path("data") %>%
    ensurer::ensure_that(dir.exists(.),
                         err_desc = "Missing directory!") %>%
    list.files(pattern = "*.csv$",
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(
        inventory_id = tools::file_path_sans_ext(basename(file_path))
    ) %>%
    tidyr::separate(col = inventory_id,
                    into = c("site", "subsite", "start", "end", "inventory"),
                    remove = FALSE) %>%
    dplyr::mutate(end = stringr::str_match(end, pattern = "[0-9]{4}")[,1],
                  start = as.numeric(start),
                  end = as.numeric(end)) %>%
    dplyr::select(-inventory) %>%
    dplyr::mutate(csv = purrr::map(file_path, readr::read_csv),
                  csv = purrr::map(csv, janitor::clean_names)) %>%
    dplyr::left_join(plot_files, by = c("site", "subsite", "start", "end"))


# Get the common column names in the CSVs.
common_names  <-
    inventory_files %>%
    dplyr::mutate(cnames = purrr::map(csv, colnames)) %>%
    dplyr::pull(cnames) %>%
    Reduce(intersect, .)



inventory_files <-
    inventory_files %>%
    dplyr::mutate(csv_sf = purrr::map(csv, drop_na_coords,
                                      lon = "utm_easting",
                                      lat = "utm_northing"))

# process_cords <- function(x) {
#     ifelse(!is.na(as.numeric(x)), x, gsub("\\.", "", y))
# }

cast_to_sf <- function(x, my_crs) {
    stopifnot(inherits(my_crs, what = "crs"))
    x %>%
        # NOTE: Deal with files with weird coordinates.
        dplyr::mutate(
            utm_e = dplyr::if_else(is.na(as.numeric(utm_easting)),
                                   gsub("\\.", "", utm_easting),
                                   NA_character_),
            utm_e = dplyr::if_else(!is.na(utm_e),
                                   gsub('^([0-9]{6})([0-9]+)$', '\\1\\.\\2',
                                        utm_e),
                                   utm_e)
            utm_easting = dplyr::if_else(is.na(utm_e), utm_easting, utm_e)
            utm_n = dplyr::if_else(is.na(as.numeric(utm_northing)),
                                   gsub("\\.", "", utm_northing),
                                   NA_character_),
            utm_n = dplyr::if_else(!is.na(utm_n),
                                   gsub('^([0-9]{7})([0-9]+)$', '\\1\\.\\2',
                                        utm_n),
                                   utm_e)
            utm_northing = dplyr::if_else(is.na(utm_n), utm_northing, utm_n)
        ) %>%
        dplyr::select(-utm_e, -utm_n) %>%
        sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
                     crs = my_crs) %>%
        return()
}

#safe_cast <- safely(cast_to_sf)



res <-
    inventory_files %>%
    # slice(4) %>%
    # pull(plot_crs) %>% class()
    dplyr::mutate(csv_sf2 = purrr::map2(csv_sf,
                                       plot_crs,
                                      #safe_cast))
                                       cast_to_sf))

y <- res[["csv_sf2"]] %>% purrr::transpose()
wrong <- res[!is_ok, ]



# TODO:
# - cast them as sf
# - assign them a CRS
# - project them to WGS84


# TODO: remove
get_east_north <- function(x) {
    x %>%
        dplyr::select(tidyselect::contains(c("east", "north"))) %>%
        janitor::clean_names()
}
