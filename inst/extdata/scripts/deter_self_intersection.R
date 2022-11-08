stop("R can't run this. Use QGIS union operation instead.")

library(dplyr)
library(lwgeom)
library(sf)
library(purrr)
library(ensurer)

sf_use_s2(FALSE)

source("~/Documents/github/treesburnareas/inst/extdata/scripts/util.R")

# Original file downloaded from terrabrasilis.
deter_file <- "~/Documents/data/deter/amazonia_legal/deter_public.shp"
stopifnot("File not found" = file.exists(deter_file))

# Directory for storing results.
out_dir <- "~/Documents/data/results"
stopifnot("Directory not found" = dir.exists(out_dir))

# Name of DETER vector file after removing invalid geometries.
deter_clean_file <-
    deter_file %>%
    tools::file_path_sans_ext() %>%
    paste0(., "_valid.shp")

# Ensure geometires are valid.
if (!file.exists(deter_clean_file)) {
    # 268796 features!
    deter_sf <-
        deter_file %>%
slice(1:10000) %>%
        sf::read_sf() %>%
        sf::st_make_valid() %>%
        (function(x) {
             print(" Fix geometries!")
            if (sf::sf_use_s2())
                deter_sf %>%
                    s2::s2_rebuild() %>%
                    return()
        }) %>%
        dplyr::mutate(is_valid = sf::st_is_valid(.))



    # NOTE: Using S2, there are 47 invalid features Not using S2, there are 0.
    deter_sf %>%
        dplyr::pull(is_valid) %>%
        table()
    # Why are the features invalid?
    deter_sf %>%
        dplyr::filter(is_valid == FALSE) %>%
        sf::st_is_valid(reason = TRUE)
    # NOTE: a 0-buffer doesn't fix the invalid geometries.
    # deter_sf %>%
    #     dplyr::filter(is_valid == FALSE) %>%
    #     st_buffer(dist = 0) %>%
    #     dplyr::mutate(is_valid = sf::st_is_valid(.)) %>%
    #     dplyr::pull(is_valid) %>%
    #     table()
    # Filter out invalid features.
    deter_sf %>%
        dplyr::filter(is_valid) %>%
    # Add convenient information.
        dplyr::mutate(
                     # VIEW_DATE = lubridate::as_date(VIEW_DATE),
                      area_km2 = units::drop_units(sf::st_area(.) / (1000^2)),
                      year = to_year_prodes(VIEW_DATE)) %>%
    # Save to disc.
        sf::write_sf(deter_clean_file)
}

# Read data.
deter_sf <-
    deter_clean_file %>%
    sf::read_sf() %>%
    ensurer::ensure_that(
        all(sf::st_is_valid(.)),
        err_desc = "Invalid geometries found!"
    )

# Intersect only burn areas.
# NOTE: This ran in 5 minutes
start <- Sys.time()
deter_intersection_ba <-
    deter_sf %>%
    dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
    sf::st_set_precision(1) %>%
    sf::st_intersection() %>%
    sf::st_collection_extract(type = "POLYGON") %>%
    dplyr::filter(!sf::st_is_empty(.))
end <- Sys.time()
print(end - start)
saveRDS(deter_intersection_ba, file = "deter_intersection_ba.shp")
# TODO: Check these results!

###############################################################################

start <- Sys.time()
deter_intersects_ba <-
    deter_sf %>%
    dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
    sf::st_set_precision(1) %>%
    sf::st_intersects()
end <- Sys.time()
print(end - start)
saveRDS(deter_intersects_ba, file = "deter_intersection_ba.shp")








deter_intersection_ba <-
    deter_sf %>%
    dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
    # NOTE: Precision set at 1 meter at WGS84 equator.
    sf::st_set_precision(1 / 6378137) %>%
    sf::st_intersection() %>%
    sf::st_collection_extract(type = "POLYGON") %>%
    dplyr::filter(!sf::st_is_empty(.))

# NOTE: This throws an error:
# Error in CPL_nary_intersection(x) : GEOS exception
# In addition: Warning message:
# In CPL_nary_intersection(x) : GEOS difference returns NULL
deter_intersection_ba <-
    deter_sf %>%
    dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
    sf::st_set_precision(0) %>%
    sf::st_intersection() %>%
    sf::st_collection_extract(type = "POLYGON") %>%
    dplyr::filter(!sf::st_is_empty(.))



# NOTE: This throws an error:
# Error in CPL_nary_intersection(x) : GEOS exception
# In addition: Warning message:
# In CPL_nary_intersection(x) : GEOS difference returns NULL
deter_intersection_ba <-
    deter_sf %>%
    dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
    sf::st_collection_extract(type = "POLYGON") %>%
    dplyr::filter(!sf::st_is_empty(.))





###############################################################################

# Self -intersect DETER data from different dates
if (!file.exists(deter_self_inter_file)) {
    stop("This takes a lot of time ~4 days in averno!")
    deter_self_inter <-
        deter_sf %>%
        # One meter in degrees at the equator on a WGS84 ellipsoid (approx.).
        #sf::st_set_precision(1 / 6378137) %>%
        # NOTE: Any other precisions are omitting polygons!
        #sf::st_set_precision(0) %>%
        # TODO: Should I intersect only Burned Area polygons?
        sf::st_intersection() %>%
        sf::st_collection_extract(type = "POLYGON") %>%
        dplyr::filter(!sf::st_is_empty(.))
    saveRDS(deter_self_inter, file = deter_self_inter_file)
}else{
    deter_self_inter <- readRDS(deter_self_inter_file)
}

###############################################################################
# TODO: how many times was the same land burned? How often?
# NOTE: All the polygons have only one self-intersection!
deter_self_inter %>%
    sf::st_drop_geometry() %>%
    select(n.overlaps, origins)
###############################################################################
# TODO: Self intersect only the Burn Scars.
deter_self_inter_ba_file <-
    deter_file %>%
    dirname() %>%
    file.path("deter_self_inter_ba.rds")
deter_self_inter_ba <-
    deter_sf %>%
    # One meter in degrees at the equator on a WGS84 ellipsoid (approx.).
    #sf::st_set_precision(1 / 6378137) %>%
    # NOTE: Any other precisions are omitting polygons!
    #sf::st_set_precision(0) %>%
    # TODO: Should I intersect only Burned Area polygons?
    sf::st_intersection() %>%
    sf::st_collection_extract(type = "POLYGON") %>%
    dplyr::filter(!sf::st_is_empty(.))
saveRDS(deter_self_inter_ba, file = deter_self_inter_ba_file)
###############################################################################

