###############################################################################
# PROCESS DETER DATA TO PRODUCE THE TIBBLE REQUIRED FOR THE FIGURES.
###############################################################################

library(data.table)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggsankey)
library(magrittr)
library(stringr)
library(terra)
library(tibble)
library(treemapify)
library(tidyr)
library(units)


#---- Configuration ----

reuse_temporal_files <- FALSE

out_dir <- "~/Documents/trees_lab/deter_warning_recurrence/img"

data_dir <- "~/Documents/data"
deter_lyr <- "deter_qgis"
deter_gpkg <- file.path(data_dir, "deter",  "amazonia_legal",
                        "deter_qgis.gpkg")
prodes_raster <- file.path(data_dir, "prodes", "amazonia", "prodes_raster.tif")
prodes_viewdate <- file.path(data_dir, "prodes", "amazonia",
                             "prodes_viewdate.tif")
prodes_classes_file  <-
    file.path(data_dir, "prodes", "amazonia",
              "PDigital2000_2021_AMZ_raster_v20220915_bioma.txt")

stopifnot("Out directory not found!" = dir.exists(out_dir))
stopifnot("GeoPackage not found!" = file.exists(deter_gpkg))
stopifnot("PRODES raster not found! Use script process_prodes.R" =
          file.exists(prodes_raster))
stopifnot("PRODES view dates raster not found! Use script procss_prodes.R" =
          file.exists(prodes_viewdate))
stopifnot("PRODES file with classes not found!" =
          file.exists(prodes_classes_file))

rm(prodes_classes_file)

deter_classes <- c(
    "CICATRIZ_DE_QUEIMADA" = "Burn scar",
    "CORTE_SELETIVO"       = "Selective cut",
    "CS_DESORDENADO"       = "Untidy slash",
    "CS_GEOMETRICO"        = "Geometric slash",
    "DEGRADACAO"           = "Degradation",
    "DESMATAMENTO_CR"      = "Clear cut",
    "DESMATAMENTO_VEG"     = "Slash with veg.",
    "MINERACAO"            = "Mining"
)

# NOTE: Obtained from the text file
#       PDigital2000_2021_AMZ_raster_v20220915_bioma.txt
prodes_classes <- c("54" =  "r2014",
                    "52" =  "r2012",
                    "61" =  "r2021",
                    "53" =  "r2013",
                    "58" =  "r2018",
                    "57" =  "r2017",
                    "51" =  "r2011",
                    "60" =  "r2020",
                    "56" =  "r2016",
                    "50" =  "r2010",
                    "59" =  "r2019",
                    "55" =  "r2015",
                    "12" =  "d2012",
                    "14" =  "d2014",
                    "13" =  "d2013",
                    "11" =  "d2011",
                    "10" =  "d2010",
                     "9" =  "d2009",
                    "15" =  "d2015",
                    "17" =  "d2017",
                    "18" =  "d2018",
                    "16" =  "d2016",
                    "20" =  "d2020",
                    "19" =  "d2019",
                    "21" =  "d2021",
                     "8" =  "d2008",
                    "91" =  "P_Water",        # Hidrografia
                    "32" =  "P_Clouds",       # Nuvem
                   "101" =  "P_Non-forest",   # NaoFloresta
                     "7" =  "d2007 (mask)", # d2007 (mascara)
                   "100" =  "P_Forest")       # Floresta


#---- Load data ----

# NOTE:
# - subarea is what we call each of the subareas of each DETER warning which
#   overlaps with some other subarea of other warnings.
# - subarea_id identifies features afer applying GIS' union operation and
#   pre-processing.

# Keep the geometry separated form the data handle it as a data.table.
subarea_sf <-
    deter_gpkg %>%
    sf::read_sf(layer = deter_lyr) %>%
    dplyr::filter(!is.na(xy_id),
                  !is.na(subarea_ha),
                  subarea_ha > 0,
                  in_prodes == 1) %>%
    sf::st_cast(to = "POLYGON",
                do_split = TRUE)

subarea_dt <-
    subarea_sf %>%
    sf::st_drop_geometry() %>%
    data.table::setDT(key = c("subarea_id", "VIEW_DATE"))

subarea_sf <-
    subarea_sf %>%
    dplyr::select(subarea_id)

stopifnot("Some subareas are missing subarea_id" =
          length(unique(subarea_sf$subarea_id)) == nrow(subarea_sf))

rm(deter_lyr)
rm(deter_gpkg)

# Recode DETER classes.
# Compute PRODES year.
# Filter out DETER warnigs before 2022.
# Sort by subarea and date.
subarea_dt <-
    subarea_dt %>%
    dplyr::mutate(
        CLASSNAME = dplyr::recode(CLASSNAME, !!!deter_classes,
                                  .default = NA_character_,
                                  .missing = NA_character_),
        CLASSNAME = forcats::fct_relevel(CLASSNAME, sort),
        VIEW_DATE = lubridate::as_date(VIEW_DATE),
        start_date = lubridate::as_date(paste(year(VIEW_DATE), "08", "01",
                                              sep = "-")),
        year = dplyr::if_else(VIEW_DATE > start_date,
                              as.double(year(start_date)) + 1,
                              as.double(year(start_date))),
        year = as.integer(year)) %>%
    dplyr::select(-start_date, -x, -y) %>%
    dplyr::filter(year < 2022) %>%
    dplyr::distinct(xy_id, VIEW_DATE, .keep_all = TRUE) %>%
    dplyr::arrange(xy_id, VIEW_DATE)


#---- Prepare plot data ----

# Get unique DETER subareas ids (ids of poligons without overlap).
subarea_unique_tb <-
    subarea_dt %>%
    dplyr::select(xy_id, subarea_id) %>%
    dplyr::group_by(xy_id) %>%
    dplyr::summarize(n_warnings_deter = dplyr::n(),
                     subarea_id = dplyr::first(subarea_id)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    tibble::as_tibble()

subarea_unique_sf  <- merge(subarea_sf,
                            subarea_unique_tb,
                            by = "subarea_id",
                            all.y = TRUE)

stopifnot("NAs in the number of deter warnings found" =
          sum(is.na(subarea_unique_sf$n_warnings_deter)) == 0)
stopifnot("Expected more subareas than unique subareas" =
          nrow(subarea_sf) >= nrow(subarea_unique_tb))

subarea_prodes_file <- file.path(out_dir, "prodes_subarea.rds")
if (reuse_temporal_files && file.exists(subarea_prodes_file)) {
    warning("Existing version of subarea_prodes. I'm gonna use it!")
    subarea_prodes <- readRDS(subarea_prodes_file)
} else {
    # NOTE: This takes long!
    subarea_prodes <-
        terra::extract(x = terra::rast(prodes_raster),
                       y = terra::vect(subarea_unique_sf["xy_id"]),
                       fun = the_mode,
                       ID = TRUE,
                       weights = FALSE,
                       exact = FALSE,
                       touches = FALSE,
                       bind = FALSE,
                       raw = FALSE)
    saveRDS(subarea_prodes, file = subarea_prodes_file)
}

stopifnot("Some PRODES data is missing" =
          nrow(subarea_prodes) == nrow(subarea_unique_sf))

# Get the PRODES' view date mode in each subarea.
subarea_prodes_date_file <- file.path(out_dir, "prodes_subarea_date.rds")
if (reuse_temporal_files && file.exists(subarea_prodes_date_file)) {
    warning("Existing version of subarea_prodes_date_found. I'm gonna use it!")
    subarea_prodes_date <- readRDS(subarea_prodes_date_file)
} else {
    # NOTE: This takes long!
    subarea_prodes_date <-
        terra::extract(x = terra::rast(prodes_viewdate),
                       y = terra::vect(subarea_unique_sf["xy_id"]),
                       fun = the_mode,
                       ID = TRUE,
                       weights = FALSE,
                       exact = FALSE,
                       touches = FALSE,
                       bind = FALSE,
                       raw = FALSE)
    saveRDS(subarea_prodes_date, file = subarea_prodes_date_file)
}

stopifnot("Some PRODES VIEWDATE data is missing" =
          nrow(subarea_prodes_date) == nrow(subarea_unique_sf))

# sf::write_sf(subarea_unique_sf,
#              dsn = file.path(out_dir, "deter_warning_recurrence.gpkg"),
#              layer = "subarea_unique_sf")

rm(subarea_unique_sf)
rm(subarea_prodes_file)
rm(subarea_prodes_date_file)
rm(prodes_raster)
rm(prodes_viewdate)

# Recode subarea_prodes' PRODES codes to PRODES years.
subarea_prodes <-
    subarea_prodes %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(c("row_id", "prodes_code")) %>%
    dplyr::mutate(row_id = as.integer(row_id),
                  prodes_code = as.integer(prodes_code),
                  prodes_name = dplyr::recode(prodes_code,
                                              !!!prodes_classes,
                                              .default = NA_character_,
                                              .missing = NA_character_))

# Recode subarea_prodes_date
subarea_prodes_date <-
    subarea_prodes_date %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(c("row_id", "prodes_date")) %>%
    dplyr::mutate(row_id = as.integer(row_id),
                  prodes_date = as.Date(prodes_date,
                                        origin = "1970-01-01"))

stopifnot(nrow(subarea_prodes) == nrow(subarea_unique_tb))
stopifnot(nrow(subarea_prodes_date) == nrow(subarea_prodes))

# Join PRODES class and viewdate.
# Add the PRODES date estimated from CLASSNAME.
subarea_prodes <-
    subarea_prodes %>%
    dplyr::left_join(subarea_prodes_date, by = "row_id") %>%
    dplyr::right_join(subarea_unique_tb, by = "row_id") %>%
    dplyr::select(-row_id, -subarea_id, -prodes_code) %>%
    dplyr::rename(CLASSNAME = prodes_name,
                  VIEW_DATE = prodes_date) %>%
    dplyr::mutate(data_source = "PRODES",
                  VIEW_DATE_est = dplyr::if_else(
                      stringr::str_starts(CLASSNAME, "d"),
                      stringr::str_c(stringr::str_sub(CLASSNAME, 2, 5),
                                     "-08-01"),
                      NA_character_),
                  VIEW_DATE_est = lubridate::as_date(VIEW_DATE_est)) %>%
    dplyr::filter(!is.na(CLASSNAME),
                  !is.na(VIEW_DATE))

stopifnot("Unique xy_ids expected" =
          length(unique(subarea_prodes[["xy_id"]])) == nrow(subarea_prodes))

rm(subarea_unique_tb)
rm(subarea_prodes_date)

print("Difference between theoretical and actual VIEW_DATE of deforestation:")
# NOTE: The median is at most 14 days off from the theoretical PRODES VIEW_DATE.
#       However the SD is up to 241 days, the minimum is 4759 days, and the
#       maximum up to 732 days.
# NOTE: Don't use the VIEW_DATE. Use the theoretical PRODES data of
#       deforestation.
subarea_prodes %>%
    dplyr::filter(stringr::str_starts(CLASSNAME, "d")) %>%
    dplyr::mutate(
        view_diff = difftime(VIEW_DATE_est, VIEW_DATE, units = "days")
    ) %>%
    group_by(CLASSNAME) %>%
    summarize(min = min(view_diff),
              mean = mean(view_diff),
              median = stats::median(view_diff),
              max = max(view_diff),
              sd = sd(view_diff)) %>%
    knitr::kable()


#---- Join subarea_prodes to DETER subareas ----

subarea_prodes <-
    data.table::setDT(subarea_prodes,
                      key = "xy_id")

prodes_ids <-
    subarea_prodes %>%
    pull(xy_id)

subarea_dt <-
    subarea_dt %>%
    dplyr::mutate(data_source = "DETER") %>%
    tibble::as_tibble() %>%
    dplyr::bind_rows(subarea_prodes) %>%
    dplyr::filter(xy_id %in% prodes_ids)

rm(prodes_ids)
rm(subarea_prodes)

subarea_dt <-
    data.table::setDT(subarea_dt,
                      key = "xy_id")

# Save data to package.
usethis::use_data(subarea_dt, subarea_sf)

