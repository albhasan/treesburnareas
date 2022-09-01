###############################################################################
# Exploratory analysis of burn scars in DETER
#------------------------------------------------------------------------------
# NOTE: This script creates the figures used in the slides.
###############################################################################

library(dplyr)
library(sf)
library(ggplot2)
library(units)
library(classInt)



#---- Configuration ----

deter_file <- "~/Documents/data/deter/amazonia_legal/deter_public.shp"
stopifnot("File not found" = file.exists(deter_file))

out_dir <- "~/Documents/github/treesburnareas/inst/extdata/slides/img"
stopifnot("Directory not found" = dir.exists(out_dir))

clean_file <- deter_file %>%
    tools::file_path_sans_ext() %>%
    paste0(., "_valid.shp")


#---- Utility functions ----


#' Estimate the PRODES year of the given date.
#'
#' @param x A date.
#' @param start_month The month when the year starts.
#' @param start_day   The day when the year starts.
#'
#' @return            An integer. The PRODES's year.
#'
to_year_prodes <- function(x, start_month = "08", start_day = "01") {
    stopifnot("At least a date is expected!" = length(x) != 0)
    stopifnot("Date object expected!" = lubridate::is.Date(x))
    if (length(x) > 1) {
        return(vapply(x,  FUN = to_year_prodes,  FUN.VALUE = integer(1)))
    }
    y <- as.integer(format(as.Date(x, format = "%d/%m/%Y"),"%Y"))
    start_year <- paste(y, start_month, start_day, sep = "-")
    if (x >= start_year)
        return(as.integer(y + 1))
    return(y)
}



#' Estimate the number of days since the start of the PRODES year.
#'
#' @param x A date.
#' @param start_month The month when the year starts.
#' @param start_day   The day when the year starts.
#'
#' @return            An integer.
#'
to_doy_prodes <- function(x, start_month = "08", start_day = "01") {
    stopifnot("At least a date is expected!" = length(x) != 0)
    stopifnot("Date object expected!" = lubridate::is.Date(x))
    if (length(x) > 1) {
        return(vapply(x,  FUN = to_doy_prodes,  FUN.VALUE = integer(1)))
    }
    y <- as.integer(format(as.Date(x, format = "%d/%m/%Y"),"%Y"))
    m <- as.integer(format(as.Date(x, format = "%d/%m/%Y"),"%m"))
    start_year <- paste(y, start_month, start_day, sep = "-")
    if (m < as.integer(start_month))
        start_year <- paste(y - 1, start_month, start_day, sep = "-")
    return(as.integer(difftime(x, start_year, units = "days")))
}



#---- Clean and save data ----

if (!file.exists(clean_file)) {
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
        sf::write_sf(paste0(tools::file_path_sans_ext(deter_file),
                            "_valid.shp"))
}



#---- Read clean data ----

deter_sf <- clean_file %>%
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

