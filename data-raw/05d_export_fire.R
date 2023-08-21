#!/usr/bin/env Rscript
###############################################################################
# PROCESS FIRE SPOTS TO PRODUCE THE DATA REQUIRED BY THE EDA'S FIGURES.
###############################################################################

library(dplyr)
library(lubridate)
library(sf)


#---- Configuration ----

# Path to fire spots with subarea id.
fire_gpkg <- "/home/alber/Documents/data/treesburnedareas/npp_375_xyid.gpkg"
fire_layer <- "fire_spots"

stopifnot("Fire spots not found!" = file.exists(fire_gpkg))


#---- Load data ----

fire_sf <-
    fire_gpkg %>%
    sf::read_sf(layer = fire_layer)


#---- Prepare data ----

fire_sf <-
    fire_sf %>%
    dplyr::mutate(datahora = lubridate::as_datetime(datahora),
                  prodes_date = as.Date(prodes_date, origin = "1970-01-01")) %>%
    dplyr::arrange(xy_id, datahora, prodes_date)


#---- Save ----

# Save data to package.
usethis::use_data(fire_sf, overwrite = TRUE)

