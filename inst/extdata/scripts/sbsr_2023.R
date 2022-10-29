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
#deter_file <- "~/Documents/sbsr2023/data/deter_public.shp"
deter_file <- "~/Documents/sbsr2023/data/deter_public_sfx_epsg32722_fix_geom_remove_duplicates_vertex_forced_right_hand.shp"
firecci_dir <- "~/data/fire_cci/burned_area/MODIS/pixel/v5.1/compressed"
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

source("util.R")



#---- Preprocess data ----

# Load DETER and filter out invalid data.
deter_sf <-
    deter_file %>%
    sf::read_sf() %>%
    fix_geom_sf()

# # Load AOI data, project it to match DETER, and filter out invalid data.
# aoi_sf <-
#     town_file %>%
#     sf::read_sf() %>%
#     dplyr::filter(geocodigo == "1507300") %>% # NOTE: Town São Félix do Xingu.
#     sf::st_transform(crs = sf::st_crs(deter_sf)) %>%
#     fix_geom_sf()

# # Filter DETER data using AOI.
# deter_sf <-
#     deter_sf %>%
#     dplyr::filter(sf::st_intersects(x = .,
#                                     y = aoi_sf,
#                                     sparse = FALSE)[1, ]) %>%
#     dplyr::filter(MUNICIPALI %in% "Sao Felix do Xingu") %>%
#     # Add convenient information.
#     dplyr::mutate(VIEW_DATE = lubridate::as_date(VIEW_DATE),
#                   area_km2 = units::drop_units(sf::st_area(.) / (1000^2)),
#                   year_prodes = to_year_prodes(VIEW_DATE))



#---- DETER self intersecting polygons ----

deter_self_inter <-
    deter_sf %>%
    sf::st_buffer(dist = 1) %>%
    sf::st_intersection()


deter_sf_copy <- deter_sf["id"]
deter_sf_copy <- st_set_precision(deter_sf_copy, 100)
deter_sf_copy <- sf::st_make_valid(deter_sf_copy)
deter_sf_copy <- sf::st_buffer(deter_sf_copy, 0)
# deter_sf_copy <- sf::st_set_precision(deter_sf_copy, 1)
# deter_sf_copy <- sf::st_set_precision(deter_sf_copy, 10)
# deter_sf_copy <- sf::st_set_precision(deter_sf_copy, 100)
#deter_sf_copy <- sf::st_set_crs(deter_sf_copy, NA)
res <- sf::st_intersection(deter_sf_copy)

    #dplyr::mutate(id = dplyr::row_number()) %>%
    sf::st_set_precision(0.001) %>%
    #sf::st_transform(crs = 32722) %>%
    #set_meter_precison() %>%
    sf::st_intersection()

deter_self_inter["geometry"]
deter_self_inter$n.overlaps %>% unique()

    # sf::st_collection_extract(type = "POLYGON") %>%
    # dplyr::filter(!sf::st_is_empty(.))



# # List FireCCI files.
# firecci_tb <-
#     firecci_dir %>%
#     list_files(pattern = "*.tif",
#                col_names = c("date", "esacci", "ls3_fire", "ba",
#                              "sensor", "area", "version", "type"),
#                separator = "-",
#                recursive = TRUE) %>%
#     dplyr::mutate(
#         file_name = basename(file_path),
#         date = lubridate::as_date(date),
#         type = dplyr::recode(type,
#                              "CL" = "confidence_level",
#                              "JD" = "julian_day",
#                              "LC" = "land_cover"),
#         new_corrected = stringr::str_detect(file_path,
#                                             pattern = "new-corrected"),
#         old_redacted  = stringr::str_detect(file_path,
#                                             pattern = "old-redacted")
#     ) %>%
#     dplyr::filter(!old_redacted) %>%
#     dplyr::select(-old_redacted)
# # Select the FireCCI corrected files over the others.
# firecci_nfiles_tb <-
#     firecci_tb %>%
#     dplyr::mutate(file_name = basename(file_path)) %>%
#     dplyr::count(file_name, name = "n_files")
# firecci_tb <-
#     firecci_tb %>%
#     dplyr::left_join(firecci_nfiles_tb, by = "file_name") %>%
#     dplyr::mutate(keep = dplyr::if_else(n_files == 1 | new_corrected,
#                                         TRUE,
#                                         FALSE)) %>%
#     dplyr::filter(keep) %>%
#     dplyr::select(-keep, -n_files, -new_corrected, -file_name)
# rm(firecci_nfiles_tb)
# firecci_tb <-
#     firecci_tb %>%
#     dplyr::filter(lubridate::year(date) >= 2016,
#                   type == "julian_day")




# # List MCD64 files.
# mcd64a1_tb <-
#     mcd64a1_dir %>%
#     list_files(pattern = "*.hdf",
#                col_names = c("product", "ydoy", "tile",
#                              "collection", "proc_date"),
#                separator = "[.]",
#                recursive = TRUE) %>%
#     dplyr::mutate(ydoy = stringr::str_sub(ydoy, start = 2L),
#                   date = lubridate::as_date(ydoy, format = "%Y%j")) %>%
#     dplyr::filter(lubridate::year(date) == 2020,
#                   tile %in% modis_tiles_amazon)




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

#deter_aoi_sf %>%
#    sf::st_drop_geometry() %>%
#    dplyr::count(CLASSNAME)

##terra::as.polygons



#---- Plot ----

# plot_deter_warnings_area <-
#     deter_sf %>%
#     sf::st_drop_geometry() %>%
#     dplyr::group_by(CLASSNAME, year_prodes) %>%
#     dplyr::summarize(area_km2 = sum(area_km2)) %>%
#     dplyr::ungroup() %>%
#     dplyr::arrange(CLASSNAME, year_prodes) %>%
#     ggplot2::ggplot() +
#     ggplot2::geom_line(ggplot2::aes(x = year_prodes,
#                                     y = area_km2,
#                                     color = CLASSNAME),
#                        size = 1) +
#     ggplot2::geom_point(ggplot2::aes(x = year_prodes,
#                                      y = area_km2,
#                                      color = CLASSNAME,
#                                      shape = CLASSNAME),
#                         size = 3) +
#     ggplot2::ggtitle(label = "Area of DETER warnings by PRODES year") +
#     ggplot2::xlab("Year PRODES") +
#     ggplot2::ylab("Area Km2") +
#     ggplot2::scale_colour_viridis_d()

