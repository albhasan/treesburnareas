#!/usr/bin/env Rscript
###############################################################################
# PROCESS DETER TO PRODUCE THE DATA REQUIRED BY THE EDA'S FIGURES.
###############################################################################

library(data.table)
library(dplyr)
library(ensurer)
library(forcats)
library(ggplot2)
library(ggsankey)
library(magrittr)
library(readr)
library(stringr)
library(terra)
library(tibble)
library(treemapify)
library(tidyr)
library(units)

library(treesburnareas)


#---- Configuration ----

reuse_temporal_files <- FALSE

# Inputs
prodes_year     <- "2021"
prodes_raster   <- "~/Documents/data/prodes/amazonia/prodes_raster.tif"
prodes_viewdate <- "~/Documents/data/prodes/amazonia/prodes_viewdate.tif"
deter_gpkg      <- "~/Documents/data/deter/amazonia_legal/deter_qgis.gpkg"
deter_lyr       <- "deter_qgis"

# Outputs
tmp_dir <- "~/Documents/trees_lab/deter_warning_recurrence/img"

# Validation
stopifnot("GeoPackage with DETER data not found! Use DETER scripts" =
          file.exists(deter_gpkg))
stopifnot("PRODES raster not found! Use script process_prodes.R" =
          file.exists(prodes_raster))
stopifnot("PRODES view dates raster not found! Use script procss_prodes.R" =
          file.exists(prodes_viewdate))
stopifnot("Temporal directory not found!" = dir.exists(tmp_dir))

# Get PRODES codes.
prodes_codes <-
    treesburnareas::get_prodes_codes() %>%
    tidyr::pivot_longer(cols = -tidyselect::any_of("prodes_code"),
                        names_to = c("X1", "source", "year"),
                        names_sep = "_",
                        values_to = "prodes_class") %>%
    dplyr::filter(source == "shp",
                  year == prodes_year) %>%
    dplyr::select(prodes_code, prodes_class) %>%
    ensurer::ensure_that(
        nrow(.) > 0,
        err_desc = "No PRODES codes found for the given year and source") %>%
    (function(x) {
         x %>%
            dplyr::pull(prodes_class) %>%
            magrittr::set_names(as.character(x[["prodes_code"]])) %>%
            return()
    })


#---- Load data ----

# Keep the geometry separated form the data handle it as a data.table.
subarea_sf <-
    deter_gpkg %>%
    sf::read_sf(layer = deter_lyr) %>%
    dplyr::mutate(VIEW_DATE = lubridate::as_date(VIEW_DATE),
                  year = compute_prodes_year(VIEW_DATE)) %>%
    dplyr::filter(!is.na(xy_id),
                  !is.na(subarea_ha),
                  subarea_ha > 0,
                  in_prodes == 1,
                  year <= as.integer(prodes_year)) %>%
    sf::st_cast(to = "POLYGON",
                do_split = TRUE) %>%
    dplyr::select(-x, -y) %>%
    dplyr::distinct(xy_id, VIEW_DATE, .keep_all = TRUE) %>%
    dplyr::arrange(xy_id, VIEW_DATE)

subarea_dt <-
    subarea_sf %>%
    sf::st_drop_geometry() %>%
    data.table::setDT(key = c("subarea_id", "VIEW_DATE"))

subarea_sf <-
    subarea_sf %>%
    dplyr::select(subarea_id, xy_id)

stopifnot("Subarea missmatch!" = nrow(subarea_sf) == nrow(subarea_dt))
stopifnot("Some subareas are missing subarea_id" =
          length(unique(subarea_sf$subarea_id)) == nrow(subarea_sf))
stopifnot("The subareas only have one trajectory step. Nothing more to do!" =
          length(unique(subarea_sf$subarea_id)) >
          length(unique(subarea_sf$xy_id)))

rm(deter_lyr)
rm(deter_gpkg)


#---- Prepare data ----

subarea_flat_sf <-
    subarea_sf %>%
    treesburnareas::get_flat_subarea()

# NOTE: To save time, check if we already did this computation.
subarea_prodes_file <- file.path(tmp_dir, "prodes_subarea.rds")
if (reuse_temporal_files && file.exists(subarea_prodes_file)) {
    warning("Existing version of subarea_prodes. I'm gonna use it!")
    subarea_prodes <- readRDS(subarea_prodes_file)
} else {
    subarea_prodes <-
        terra::extract(x = terra::rast(prodes_raster),
                       y = terra::vect(subarea_flat_sf),
#TODO: Add the use of the mode to the slides!
                       fun = the_mode,
                       ID = FALSE,
                       weights = FALSE,
                       exact = FALSE,
                       touches = FALSE,
                       bind = FALSE,
                       raw = FALSE)
    saveRDS(subarea_prodes, file = subarea_prodes_file)
}

# Get the PRODES' view date mode in each subarea.
# NOTE: To save time, check if we already did this computation.
subarea_prodes_date_file <- file.path(tmp_dir, "prodes_subarea_date.rds")
if (reuse_temporal_files && file.exists(subarea_prodes_date_file)) {
    warning("Existing version of subarea_prodes_date_found. I'm gonna use it!")
    subarea_prodes_date <- readRDS(subarea_prodes_date_file)
} else {
    subarea_prodes_date <-
        terra::extract(x = terra::rast(prodes_viewdate),
                       y = terra::vect(subarea_flat_sf),
                       fun = the_mode,
                       ID = FALSE,
                       weights = FALSE,
                       exact = FALSE,
                       touches = FALSE,
                       bind = FALSE,
                       raw = FALSE)
    saveRDS(subarea_prodes_date, file = subarea_prodes_date_file)
}

subarea_flat_sf <- cbind(subarea_flat_sf, subarea_prodes, subarea_prodes_date)

rm(subarea_prodes, subarea_prodes_date)
rm(subarea_prodes_file, subarea_prodes_date_file)
rm(prodes_raster, prodes_viewdate)

subarea_prodes <-
    subarea_flat_sf %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(prodes_date = as.Date(prodes_date, origin = "1970-01-01"),
                  prodes_code = as.integer(prodes_code),
                  prodes_name = dplyr::recode(prodes_code,
                                              !!!prodes_classes,
                                              .default = NA_character_,
                                              .missing = NA_character_)) %>%
    #dplyr::select(-prodes_code) %>%
    dplyr::rename(CLASSNAME = prodes_name,
                  VIEW_DATE = prodes_date) %>%
    dplyr::mutate(data_source = "PRODES") %>%
    dplyr::filter(!is.na(CLASSNAME),
                  !is.na(VIEW_DATE))

stopifnot("Unique xy_ids expected" =
          length(unique(subarea_prodes[["xy_id"]])) == nrow(subarea_prodes))


#---- Join subarea_prodes to DETER subareas ----

subarea_dt <-
    subarea_dt %>%
    dplyr::mutate(data_source = "DETER") %>%
    dplyr::bind_rows(subarea_prodes) %>%
    data.table::setDT(key = "xy_id")

rm(subarea_prodes)

# Save data to package.
usethis::use_data(subarea_dt, subarea_sf)

