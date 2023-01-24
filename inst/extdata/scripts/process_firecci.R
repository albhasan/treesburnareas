#!/usr/bin/env Rscript

###############################################################################
# PROCESS THE RAW FIRECCI51 DATA BEFORE ANALYSIS.
#------------------------------------------------------------------------------
# This script uncompress and crop FireCCI to match the Brazilian Legal Amazon.
#------------------------------------------------------------------------------
# NOTE:
# - FireCCI vectorization using the terra package throws an "out of memory"
#   error.
# - The vectorization is going to take place using GRASS GIS and R.
###############################################################################

library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(lubridate)
library(gdalUtilities)
library(terra)
library(furrr)

in_dir  <- "~/data/fire_cci/burned_area/MODIS/pixel/v5.1/compressed"
out_dir <- "/home/alber/Documents/trees_lab/deter_warning_recurrence/data/fire_cci"
prodes_mask_file <- "/home/alber/data/prodes/gdal_mask.shp"

tmp_dir <- tempdir()

stopifnot("Input directory not found! " = dir.exists(in_dir))
stopifnot("Out directory not found! " = dir.exists(out_dir))
stopifnot("Temporal directory not found! " = dir.exists(tmp_dir))
stopifnot("PRODES maks not found!" = file.exists(prodes_mask_file))

# Get the file name of a tar.gz file (remove directory and extension).
get_tgz_name <- function(file_path){
    file_path %>%
        basename() %>%
        tools::file_path_sans_ext() %>%
        tools::file_path_sans_ext() %>%
        return()
}


untar_and_list <- function(tar_path, exdir) {
    untar(tar_path, exdir = exdir)
    untar(tar_path, list = TRUE)
}

# Get a tibble with FireCCI51 images.
images_tb <-
    in_dir %>%
    list.files(
        pattern = "[0-9]{8}-ESACCI-L3S_FIRE-BA-MODIS-AREA_2-fv5.1.tar.gz$",
        full.names = TRUE,
        recursive = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(tar_path = value) %>%
    dplyr::mutate(parent_dir = basename(dirname(tar_path)),
                  file_name = get_tgz_name(tar_path)) %>%
    dplyr::filter(parent_dir != "old-redacted") %>%
    tidyr::separate(col = file_name,
                    into = c("view_date", NA, NA, NA, NA, NA, NA),
                    sep = "-") %>%
    dplyr::mutate(view_date = lubridate::as_date(view_date),
                  view_year = lubridate::year(view_date)) %>%
    dplyr::filter(view_date >= lubridate::as_date("20160101")) %>%
    dplyr::mutate(img_path = purrr::map2(tar_path,
                                         file.path(tmp_dir, view_year),
                                         untar_and_list),
                  img_path = purrr::map(img_path, tibble::as_tibble)) %>%
    tidyr::unnest(img_path) %>%
    dplyr::rename(img_path = value) %>%
    dplyr::mutate(img_path = file.path(tmp_dir, view_year, img_path)) %>%
    dplyr::filter(tools::file_ext(img_path) == "tif") %>%
    dplyr::mutate(img_name = tools::file_path_sans_ext(basename(img_path))) %>%
    tidyr::separate(col = img_name,
                    into = c("view_date", NA, NA, NA, NA, NA, NA, "type"),
                    sep = "-")

# Image type:
# JD: Day of first detection (Julian Day) of the burned area
# CL: Confidence level of burned area detection
# LC: Land cover of the pixel detected as burned

# Make sure the given paths exist.
make_dirs <- function(paths) {
    new_paths <- unique(paths[!dir.exists(paths)])
    if (length(new_paths) > 0)
        res <- sapply(new_paths, FUN = dir.create, recursive = TRUE)
    dir.exists(paths)
}

# Crop the image to match PRODES.
future::plan(multisession, workers = parallel::detectCores() / 2)
images_tb <-
    images_tb %>%
    dplyr::mutate(img_crop = file.path(out_dir, view_year, basename(img_path)),
                  img_dir_exists = make_dirs(dirname(img_crop)),
                  img_crop = furrr::future_map2(img_path, img_crop,
                                                gdalUtilities::gdalwarp,
                                                cutline = prodes_mask_file,
                                                crop_to_cutline = TRUE,
                               .options = furrr::furrr_options(seed = NULL)))

# Read and vectorze a raster.
to_vector <- function(img_path, view_unix) {
    img_path %>%
        terra::rast() %>%
        terra::as.polygons(trunc = TRUE,
                           dissolve = TRUE,
                           values = TRUE,
                           na.rm = TRUE,
                           na.all = FALSE,
                           extent = FALSE) %>%
        magrittr::set_names(c("value", "geometry")) %>%
        dplyr::filter(value > 0) %>%
        dplyr::mutate(view_unix = view_unix) %>%
        return()
}

future::plan(sequential)

# Vectorize the rasters.
# NOTE: R can't handle the amount of data. Use GRASS GIS instead.

