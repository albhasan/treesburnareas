#!/usr/bin/env Rscript
###############################################################################
# Proces PRODES data.
#------------------------------------------------------------------------------
# To ensure accuracy, we rasterize PRODES data on our own.
###############################################################################

library(dplyr)
library(sf)
library(terra)



#---- Set up ----

out_dir <- "/home/alber/data/prodes/amazonia"
stopifnot(dir.exists(out_dir))

prodes_raster   <- file.path(out_dir, "prodes_raster.tif")
prodes_viewdate <- file.path(out_dir, "prodes_viewdate.tif")

clo_shp <- "/home/alber/data/prodes/amazonia/cloud_biome.shp"
d07_shp <- paste0("/home/alber/data/prodes/amazonia/",
                  "accumulated_deforestation_2007_biome.shp")
def_shp <- "/home/alber/data/prodes/amazonia/yearly_deforestation_biome.shp"
for_shp <- "/home/alber/data/prodes/amazonia/forest_biome_2021.shp"
hyd_shp <- "/home/alber/data/prodes/amazonia/hydrography_biome.shp"
nof_shp <- "/home/alber/data/prodes/amazonia/no_forest_biome.shp"
res_shp <- "/home/alber/data/prodes/amazonia/residual_biome.shp"
prodes_tif <- paste0("/home/alber/data/prodes/amazonia/",
                     "PDigital2000_2021_AMZ_raster_v20220915_bioma.tif")

stopifnot("PRODES files not found!" = file.exists(clo_shp, d07_shp, def_shp,
                                                  for_shp, hyd_shp, nof_shp,
                                                  res_shp, prodes_tif))


# NOTE: These codes were obtained from the medata of
#       PDigital2000_2021_AMZ_raster_v20220915_bioma.tif and adjusted to match
#       the class names used in the PRODES shapefiles.
prodes_codes <- c(
    "r2014"           =  54L,
    "r2012"           =  52L,
    "r2021"           =  61L,
    "r2013"           =  53L,
    "r2018"           =  58L,
    "r2017"           =  57L,
    "r2011"           =  51L,
    "r2020"           =  60L,
    "r2016"           =  56L,
    "r2010"           =  50L,
    "r2019"           =  59L,
    "r2015"           =  55L,
    "d2012"           =  12L,
    "d2014"           =  14L,
    "d2013"           =  13L,
    "d2011"           =  11L,
    "d2010"           =  10L,
    "d2009"           =   9L,
    "d2015"           =  15L,
    "d2017"           =  17L,
    "d2018"           =  18L,
    "d2016"           =  16L,
    "d2020"           =  20L,
    "d2019"           =  19L,
    "d2021"           =  21L,
    "d2008"           =   8L,
    "HIDROGRAFIA"     =  91L,
    "NUVEM_2021"      =  32L,
    "NAO_FLORESTA"    = 101L,
    "NAO_FLORESTA2"   = 101L,
    "d2007"           =   7L,
    "FOREST_2021"     = 100L
)



#---- Rasterize PRODES ----
# NOTE: We had to rasterize PRODES because of spatial missmatches.

# Read PRODES' shapefiles.
clo_sf <- sf::read_sf(clo_shp)
d07_sf <- sf::read_sf(d07_shp)
def_sf <- sf::read_sf(def_shp)
for_sf <- sf::read_sf(for_shp)
hyd_sf <- sf::read_sf(hyd_shp)
nof_sf <- sf::read_sf(nof_shp)
res_sf <- sf::read_sf(res_shp)

# Find the common names among PRODES' shapefiles.
col_names <- Reduce(intersect, lapply(list(clo_sf, d07_sf, def_sf, for_sf,
                                           hyd_sf, nof_sf, res_sf),
                                      colnames))

# Bind the PRODES' shapefiles into one.
prodes_sf <- rbind(clo_sf[clo_sf$class_name == "NUVEM_2021", col_names],
                   d07_sf[col_names],
                   def_sf[col_names],
                   for_sf[col_names],
                   hyd_sf[col_names],
                   nof_sf[col_names],
                   res_sf[col_names])

stopifnot("Missing PRODES codes found!" = unique(prodes_sf$class_name) %in%
          names(prodes_codes))

# Recode class names into integers.
prodes_sf <-
    prodes_sf %>%
    dplyr::mutate(code = dplyr::recode(class_name, !!!prodes_codes))

# Rasterize PRODES.
terra::rasterize(x = terra::vect(prodes_sf),
                 y = terra::rast(prodes_tif),
                 field = "code",
                 background = 255,
                 touches = FALSE,
                 update = FALSE,
                 sum = FALSE,
                 cover = FALSE,
                 filename = prodes_raster,
                 overwrite = FALSE,
                 wopt = list(datatype = "INT1U",
                             gdal = c("COMPRESS=LZW",
                                      "BIGTIFF=YES"),
                             NAflag = 255))



#---- Rasterize PRODES' view date ----

# NOTE: Convert PRODES' image date to number of days since 1970-01-01.
prodes_date_sf <-
    prodes_sf %>%
    dplyr::filter(!is.na(image_date),
                  image_date != " ") %>%
    dplyr::mutate(prodes_date = as.integer(as.Date(image_date)))

terra::rasterize(x = terra::vect(prodes_date_sf),
                 y = terra::rast(prodes_tif),
                 field = "prodes_date",
                 background = -32768,
                 touches = FALSE,
                 update = FALSE,
                 sum = FALSE,
                 cover = FALSE,
                 filename = prodes_viewdate,
                 overwrite = FALSE,
                 wopt = list(datatype = "INT2S",
                             gdal = c("COMPRESS=LZW",
                                      "BIGTIFF=YES"),
                             NAflag = -32768))

