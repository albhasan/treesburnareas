###############################################################################
# Exploratory analysis of burn scars in DETER
#------------------------------------------------------------------------------
# NOTE: This script creates the figures used in the slides.
# NOTE: Burn scars don't match with the fire products.
###############################################################################

library(dplyr)
library(sf)
library(ggplot2)
library(units)
library(classInt)



#---- Configuration ----

deter_file <- "~/Documents/data/deter/amazonia_legal/deter_public_AG.shp"
stopifnot("File not found" = file.exists(deter_file))

prodes_file <- "~/Documents/data/prodes/amazonia/yearly_deforestation_biome.shp"
stopifnot("File not found" = file.exists(prodes_file))

out_dir <- "~/Documents/github/treesburnareas/inst/extdata/slides/img"
stopifnot("Directory not found" = dir.exists(out_dir))

# DETER vector file after removing invalid geometries.
deter_clean_file <-
    deter_file %>%
    tools::file_path_sans_ext() %>%
    paste0(., "_valid.shp")

# DETER vector file after self-intersection (it contains different geometries).
deter_self_inter_file <-
    deter_file %>%
    dirname() %>%
    file.path("deter_self_inter.rds")

# PRODES vector file after removing invalid geometries.
prodes_clean_file <-
    prodes_file %>%
    tools::file_path_sans_ext() %>%
    paste0(., "_valid.shp")



#---- DETER clean and save data ----

if (!file.exists(deter_clean_file)) {
    # NOTE: 268796 features!
    deter_sf <-
        deter_file %>%
        sf::read_sf() %>%
        sf::st_make_valid() %>%
        dplyr::mutate(is_valid = sf::st_is_valid(.))
    # NOTE: 47 invalid features remain!
    deter_sf %>%
        dplyr::filter(is_valid == FALSE) %>%
        sf::st_is_valid(reason = TRUE)
    # Filter out invalid features.
    deter_sf %>%
        dplyr::filter(is_valid) %>%
    # Add convenient information.
        dplyr::mutate(VIEW_DATE = lubridate::as_date(VIEW_DATE),
                      area_km2 = units::drop_units(sf::st_area(.) / (1000^2)),
                      year = to_year_prodes(VIEW_DATE)) %>%
    # Save to disc.
        sf::write_sf(deter_clean_file)
}



#---- Read clean data ----

deter_sf <-
    deter_clean_file %>%
    sf::read_sf()



#---- Date range ----

deter_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::pull(VIEW_DATE) %>%
    range()



#---- DETER's area by state and type ----

plot_deter_warnings_area <-
    deter_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(CLASSNAME, UF, year) %>%
    dplyr::summarize(area_km2 = sum(area_km2)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(CLASSNAME, UF, year) %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = year,
                                    y = area_km2,
                                    color = CLASSNAME),
                       size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = year,
                                     y = area_km2,
                                     color = CLASSNAME,
                                     shape = CLASSNAME),
                        size = 3) +
    ggplot2::facet_wrap(~UF) +
    ggplot2::ggtitle(label = "Area of DETER warnings by state and year") +
    ggplot2::xlab("Year PRODES") +
    ggplot2::ylab("Area Km2") +
    ggplot2::scale_colour_viridis_d()

ggplot2::ggsave(
    plot = plot_deter_warnings_area,
    filename = file.path(out_dir, "deter_warnings_area.png"),
    height = 210,
    width = 297,
    units = "mm"
)

print(plot_deter_warnings_area)



#---- DETER's warnings by state and year ----

plot_deter_warnings_number <-
    deter_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(CLASSNAME, UF, year) %>%
    dplyr::summarize(events = dplyr::n()) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot() +
    ggplot2::geom_path(ggplot2::aes(x = year,
                                    y = events,
                                    color = CLASSNAME),
                       size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = year,
                                     y = events,
                                     color = CLASSNAME),
                        size = 3) +
    ggplot2::facet_wrap(~UF) +
    ggplot2::ggtitle(label = "Number of DETER warnings by state and year") +
    ggplot2::xlab("Year PRODES") +
    ggplot2::ylab("Number of events") +
    ggplot2::scale_colour_viridis_d()

ggplot2::ggsave(
    plot = plot_deter_warnings_number,
    filename = file.path(out_dir, "deter_warnings_number.png"),
    height = 210,
    width = 297,
    units = "mm"
)

print(plot_deter_warnings_number)



#---- DETER's warnings by satellite, state, and year ----

plot_deter_warnings_satellite <-
    deter_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
    dplyr::group_by(UF, SATELLITE, year) %>%
    dplyr::summarize(events = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(UF, SATELLITE, year) %>%
    ggplot2::ggplot() +
    ggplot2::geom_path(ggplot2::aes(x = year,
                                    y = events,
                                    color = SATELLITE)) +
    ggplot2::geom_point(ggplot2::aes(x = year,
                                     y = events,
                                     color = SATELLITE),
                        size = 3) +
    ggplot2::facet_wrap(~UF) +
    ggplot2::ggtitle(
        label = "Number of DETER warnings by satellite, state and year"
    ) +
    ggplot2::xlab("Year PRODES") +
    ggplot2::ylab("Number of events") +
    ggplot2::scale_colour_viridis_d()

ggplot2::ggsave(
    plot = plot_deter_warnings_satellite,
    filename = file.path(out_dir, "deter_warnings_satellite.png"),
    height = 210,
    width = 297,
    units = "mm"
)

print(plot_deter_warnings_satellite)



#---- DETER area by state, year ----

# Categorize the warnings' area.
area_breaks <- c(
    "0"        = 0,
    "6,25 ha"  = 0.0625,
    "10 ha"    = 0.10,
    "25 ha"    = 0.25,
    "50 ha"    = 0.50,
    "1 km2"    = 1,
    "2.5 km2"  = 2.5,
    "5 km2"    = 5,
    "10 km2"   = 10,
    "> 10 km2" = Inf
)



#---- DETER's burn area by state, type, and size ----

plot_deter_warnings_area_size <-
    deter_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(area_type = cut(area_km2,
                                  breaks = area_breaks,
                                  labels = names(area_breaks)[-1]),
                  year = ordered(year)) %>%
    dplyr::group_by(CLASSNAME, UF, area_type, year) %>%
    dplyr::summarize(area_km2 = sum(area_km2)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = year,
                                   y = area_km2,
                                   fill = area_type)) +
    ggplot2::facet_wrap(~UF) +
    ggplot2::ggtitle(label = "DETER's BURN SCAR area by state and year") +
    ggplot2::xlab("Year PRODES") +
    ggplot2::ylab("Area km2") +
    ggplot2::labs(fill = "Area less than") +
    ggplot2::scale_colour_viridis_d()

ggplot2::ggsave(
    plot = plot_deter_warnings_area_size,
    filename = file.path(out_dir, "deter_warnings_area_size.png"),
    height = 210,
    width = 297,
    units = "mm"
)

print(plot_deter_warnings_area_size)



#---- DETER's warnings by state, type, and size ----

plot_deter_warnings_size <-
    deter_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
    dplyr::mutate(year = ordered(year)) %>%
    dplyr::mutate(area_type = cut(area_km2,
                                  breaks = area_breaks,
                                  labels = names(area_breaks)[-1])) %>%
    dplyr::group_by(CLASSNAME, UF, area_type, year) %>%
    dplyr::summarize(events = dplyr::n()) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = year,
                                   y = events,
                                   fill = area_type)) +
    ggplot2::facet_wrap(~UF) +
    ggplot2::ggtitle(label = "DETER's BURN SCAR events by state and year") +
    ggplot2::xlab("Year PRODES") +
    ggplot2::ylab("Number of events") +
    ggplot2::labs(fill = "Area less than") +
    ggplot2::scale_colour_viridis_d()

ggplot2::ggsave(
    plot = plot_deter_warnings_size,
    filename = file.path(out_dir, "deter_warnings_size.png"),
    height = 210,
    width = 297,
    units = "mm"
)

print(plot_deter_warnings_size)



#---- DETER's warnings by state, type, size, and month of the year ----

plot_deter_warnings_size_month <-
    deter_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
    dplyr::mutate(moy = as.integer(format(as.Date(VIEW_DATE,
                                                  format = "%d/%m/%Y"), "%m")),
                  moy = ordered(moy, labels = month.abb),
                  year = ordered(year)) %>%
    dplyr::group_by(CLASSNAME, UF, year, moy) %>%
    dplyr::summarize(area_km2 = sum(area_km2)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = moy,
                                   y = area_km2,
                                   fill = year)) +
    ggplot2::facet_wrap(~UF) +
    ggplot2::ggtitle(
        label = "DETER's BURN SCAR area by state, and month"
    ) +
    ggplot2::xlab("Month") +
    ggplot2::ylab("Area km2") +
    ggplot2::scale_colour_viridis_d()

ggplot2::ggsave(
    plot = plot_deter_warnings_size_month,
    filename = file.path(out_dir, "deter_warnings_size_month.png"),
    height = 210,
    width = 297,
    units = "mm"
)

print(plot_deter_warnings_size_month)



#---- DETER's events by state, type, size, and month of the year ----

plot_deter_events_size_month <-
    deter_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
    dplyr::mutate(moy = as.integer(format(as.Date(VIEW_DATE,
                                                  format = "%d/%m/%Y"), "%m")),
                  moy = ordered(moy, labels = month.abb),
                  year = ordered(year)) %>%
    dplyr::group_by(CLASSNAME, UF, year, moy) %>%
    dplyr::summarize(events = dplyr::n()) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = moy,
                                   y = events,
                                   fill = year)) +
    ggplot2::facet_wrap(~UF) +
    ggplot2::ggtitle(
        label = "DETER's BURN SCAR events by state, and month"
    ) +
    ggplot2::xlab("Month") +
    ggplot2::ylab("Number of events") +
    ggplot2::scale_colour_viridis_d()

ggplot2::ggsave(
    plot = plot_deter_events_size_month,
    filename = file.path(out_dir, "deter_events_size_month.png"),
    height = 210,
    width = 297,
    units = "mm"
)

print(plot_deter_events_size_month)



#---- Compare DETER BA to PRODES ----



#---- PRODES clean and save data ----

if (!file.exists(prodes_clean_file)) {
    # NOTE: 599819 features!
    prodes_sf <-
        prodes_file %>%
        sf::read_sf() %>%
        sf::st_make_valid() %>%
        dplyr::mutate(is_valid = sf::st_is_valid(.))
    # NOTE: 0 invalid features remain!
    prodes_sf %>%
        dplyr::filter(is_valid == FALSE) %>%
        sf::st_is_valid(reason = TRUE)


     # Filter out invalid features.
    prodes_sf <-
        prodes_sf %>%
        dplyr::filter(is_valid) %>%
        dplyr::filter(!is.na(image_date)) %>%
    # Add convenient information.
        dplyr::mutate(image_date = stringr::str_sub(image_date, start = 1L,
                                                    end = 10L),
                      image_date = lubridate::as_date(image_date),
                      area_km2 = units::drop_units(sf::st_area(.) / (1000^2)),
                      year = to_year_prodes(image_date)) %>%
    # Save to disc.
        sf::write_sf(paste0(tools::file_path_sans_ext(prodes_file),
                            "_valid.shp"))
}

#---- How many times was the same area burned? ----

# Self -intersect DETER data from different dates
if (!file.exists(deter_self_inter_file)) {
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
# TODO: Check deter_self_intersection.R
##############################################################################




#---- PRODES ----



#---- Read clean data ----

prodes_sf <-
    prodes_clean_file %>%
    sf::read_sf()



#---- Date range ----

prodes_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::pull(image_date) %>%
    range()






#---- How many DETER polygons intersect DETER polygons and when? ----



