###############################################################################
# Analysis data for submission to the Simposio Brasileiro de Sensoramento
# Remoto 2023
#
# Compare DETER's Burned Areas to those of other products using segmentation
# metrics.
#-----------------------------------------------------------------------------
# TODO:
# - DONE: Choose year.  2020 was the year with the most fires according to
#   TerraBrasilis.
# - Download data:
#   - DONE (got Guilherme's copy): Fire_cci
#   - DONE: GFW Global forest loss due to fire
#   - DONE: MCD64A1
#   - DONE: MapBiomas Fogo
#   - NOTE: Mapbiomas fogo monthly. I tried but instead it downloads the annual
#           fire. I used mapbiomas scripts.
#   - GABAM. NOTE: Get copy from Guilherme.
# - Choose study area:
#   - DONE: Town: São Félix do Xingu
#   - DONE: Protected area: APA Triunfo do Xingu
#   - DONE: Indegenous land: Ti Parque do Xingu
#   - Aline's area: Autazes (AM-256), Alta Floresta, e Santarem (Flona de
#     Tapajos).
###############################################################################

library(dplyr)
library(sf)
library(ensurer)

#---- Configuration ----

base_dir <- "~/Documents/sbsr2023"

town_file <-  "~/data/terrabrasilis/municipalities_amazon_biome.shp"
il_file   <-  "~/data/terrabrasilis/indigenous_area_amazon_biome.shp"
cu_file   <-  "~/data/terrabrasilis/conservation_units_amazon_biome.shp"
#deter_file <- "~/Documents/data/deter/amazonia_legal/deter_public_valid.shp"
deter_file <- "~/Documents/sbsr2023/data/deter_public.shp"
firecci_dir <- "~/data/fire_cci/burned_area/MODIS/pixel/v5.1/compressed/2020"
mcd64a1_dir <- "~/data/mcd64a1/MCD64A1"

stopifnot("Towns shp not found!"              = file.exists(town_file))
stopifnot("Indigenous lands shp not found!"   = file.exists(il_file))
stopifnot("Conservation units shp not found!" = file.exists(cu_file))
stopifnot("DETER shp not found!"              = file.exists(deter_file))
stopifnot("FireCCI directory not found!"      = dir.exists(firecci_dir))
stopifnot("MCD64A1 directory not found!"      = dir.exists(mcd64a1_dir))
stopifnot("Base directory not found!"         = dir.exists(base_dir))

modis_tiles_amazon <- c("h10v09", "h11v08", "h11v09", "h11v10",
                        "h12v08", "h12v09", "h12v10", "h13v09")



#---- Utilitary functions ----

#' Build a tibble with the names of the files in the given directory.
#'
#' @param dir       A path to a directory.
#' @param pattern   A pattern for filtering files.
#' @param col_names Names of the columns for splitting the file names.
#' @param separator Character used for splitting the file names.
#' @return          A tibble.
list_files <- function(dir, pattern, col_names, separator) {
    dir %>%
        list.files(pattern = pattern,
                   full.names = TRUE,
                   recursive = TRUE) %>%
        tibble::as_tibble() %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(
            file_name = tools::file_path_sans_ext(basename(file_path))
        ) %>%
        tidyr::separate(col = file_name,
                        into = col_names,
                        sep = separator) %>%
        return()
}



#---- Preprocess data ----

deter_sf <-
    deter_file %>%
    sf::read_sf() %>%
    sf::st_make_valid() %>%
    dplyr::mutate(is_valid = sf::st_is_valid(.)) %>%
    dplyr::filter(is_valid) %>%
    dplyr::select(-is_valid)

aoi_sf <-
    town_file %>%
    sf::read_sf() %>%
    dplyr::filter(geocodigo == "1507300") %>% # NOTE: Town São Félix do Xingu.
    sf::st_transform(crs = sf::st_crs(deter_sf)) %>%
    sf::st_make_valid() %>%
    dplyr::mutate(is_valid = sf::st_is_valid(.)) %>%
    dplyr::filter(is_valid) %>%
    dplyr::select(-is_valid)

deter_aoi <-
    deter_sf %>%
    dplyr::filter(sf::st_intersects(x = ., y = aoi_sf, sparse = FALSE)[1,])

firecci_tb <-
    firecci_dir %>%
    list_files(pattern = "*.tif",
           col_names = c( "date", "esacci", "ls3_fire", "ba",
                         "sensor", "area", "version", "type"),
           separator = "-") %>%
    dplyr::mutate(date = lubridate::as_date(date),
                  type = dplyr::recode(type,
                                       "CL" = "confidence_level",
                                       "JD" = "julian_day",
                                       "LC" = "land_cover"))




    dplyr::filter(lubridate::year(date) == 2020,
                  type == "julian_day")





mcd64a1_tb <-
    mcd64a1_dir %>%
    list_files(pattern = "*.hdf",
               col_names = c("product", "ydoy", "tile",
                             "collection", "proc_date"),
               separator = "[.]") %>%
    dplyr::mutate(ydoy = stringr::str_sub(ydoy, start = 2L),
                  date = lubridate::as_date(ydoy, format = "%Y%j")) %>%
    dplyr::filter(lubridate::year(date) == 2020,
                  tile %in% modis_tiles_amazon)




#---- Pre-process MCD64A1 ----

# TODO:
# - Choose a Confidence Level threshold.
# - Mask the Julian Day (JD) raster using the the Confidence Level (CL) raster.
# - Convert the results to binary.
# - Cut to the AOI.
# - Add the monthly binaries of the time period matching DETER.
# - Vectorize raster.



#---- Pre-process MAPBIOMAS fire ----

# TODO:
# - Once a year.
# - How to match different time periods?



#---- Filter data using AOI ----

deter_aoi_sf <-
    deter_sf %>%
    sf::st_filter(y = aoi_sf)

deter_aoi_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::count(CLASSNAME)




# NOTE: Vectorization in R
#terra::as.polygons



