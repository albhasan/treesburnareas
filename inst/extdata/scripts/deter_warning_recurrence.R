###############################################################################
# Analysis of the recurrence of DETER warnings.
#------------------------------------------------------------------------------
# NOTE:
# - input data is a QGIS geopackage with DETER warnings after running
#   fix_geometries, multi to single part, union, and multi to single part
#    (again) operations.
###############################################################################

library(data.table)
library(dtplyr)
library(dplyr)
library(sf)
library(units)
library(stringr)


#---- Configuration ----

base_dir <- "~/Documents/trees_lab/deter_warning_recurrence"
out_dir <- file.path(base_dir, "img")
deter_gpk <- "/home/alber/Documents/data/deter/amazonia_legal/deter.gpkg"
deter_lyr <- "deter_public_fix_m2s_union_m2s"

stopifnot("Base directory not found!" = dir.exists(base_dir))
stopifnot("Out directory not found!" = dir.exists(out_dir))
stopifnot("GeoPackage not found!" = file.exists(deter_gpk))

# Categorize the warnings' area.
area_breaks <- c(
    "0"        = 0,
    "6,25 ha"  = 6.25,
    "10 ha"    = 10,
    "25 ha"    = 25,
    "50 ha"    = 50,
    "100 ha"   = 100,
    "250 ha"   = 250,
    "500 ha"   = 500,
    "1000 ha"   = 1000,
    "> 1000 ha" = Inf
)

sf::sf_use_s2(FALSE)


#---- Load data ----

# NOTE:
# - deter_id identifies the original DETER features.
# - subarea_id identifies features afer applying QGIS' union operation.
union_sf <- sf::read_sf(deter_gpk, layer = deter_lyr)
union_sf["area_ha"] <- units::drop_units(sf::st_area(union_sf)) * 0.0001

# Keep the geometry separated and handle data as a data.table.
union_dt <- data.table::setDT(sf::st_drop_geometry(union_sf),,
                              key = c("subarea_id", "VIEW_DATE"))
union_dt <- dtplyr::lazy_dt(union_dt,
                            key_by = c(subarea_id, VIEW_DATE),
                            immutable = FALSE)
union_sf <- union_sf["subarea_id"]

# Build an ID based on coordinates and area.
union_dt <-
    union_dt %>%
    dplyr::mutate(x = round(x, digits = 10),
                  y = round(y, digits = 10),
                  xy_id = stringr::str_c(x, y, sep = ";")) %>%
    dplyr::select(-x, -y)

# Compute the PRODES year.
union_dt <-
    union_dt %>%
    dplyr::mutate(start_date = as.Date(paste(year(VIEW_DATE), "08", "01",
                                             sep = "-"))) %>%
    dplyr::mutate(year = dplyr::if_else(VIEW_DATE > start_date,
                                        year(start_date) + 1,
                                        year(start_date))) %>%
    dplyr::select(-start_date)

# Filter
union_dt <-
    union_dt %>%
    dplyr::filter(year < 2022)



#---- Prepare plot data ----

plot_data <- dtplyr::lazy_dt(union_dt,
                            key_by = c(xy_id, VIEW_DATE),
                            immutable = TRUE)

plot_data <-
    plot_data %>%
    dplyr::arrange(xy_id, VIEW_DATE) %>%
    dplyr::group_by(xy_id) %>%
    dplyr::mutate(last_VIEW_DATE = dplyr::lag(VIEW_DATE),
                  diff_days = as.vector(difftime(VIEW_DATE, last_VIEW_DATE,
                                                 units = "days"))) %>%
    dplyr::ungroup()



#---- Plot area by number of warnings ----

plot_area_by_warnings <-
    plot_data %>%
    dplyr::filter(area_ha > 0,
                  !is.na(area_ha)) %>%
    dplyr::group_by(xy_id) %>%
    dplyr::summarize(n_warnings = dplyr::n(),
                     area_ha = dplyr::first(area_ha),
                     xy_id = dplyr::first(xy_id)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(area_type = cut(area_ha,
                                  breaks = area_breaks,
                                  labels = names(area_breaks)[-1])) %>%
    dplyr::group_by(n_warnings, area_type) %>%
    dplyr::summarize(area_ha = sum(area_ha)) %>%
    dplyr::ungroup() %>%
    #dplyr::show_query()
    tibble::as_tibble() %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = n_warnings,
                                   y = area_ha,
                                   fill = area_type),
                      stat = "identity") +
    ggplot2::xlab("Number of wanings.") +
    ggplot2::ylab("Area (ha)") +
    ggplot2::labs(fill = "Area less than") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    #ggplot2::scale_y_log10(labels = scales::comma) +
    ggplot2::scale_colour_viridis_d()

if (interactive()) {
    plot_area_by_warnings +
        ggplot2::ggtitle(paste("Number of DETER warnings by area in the",
                               "Brazilian Amazon"))
}else{
    ggplot2::ggsave(
        plot = plot_area_by_warnings,
        filename = file.path(out_dir, "plot_area_by_warnings.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
}

rm(plot_area_by_warnings)



#---- Plot area by number of warnings by state ----

plot_area_by_warnings_state <-
    plot_data %>%
    dplyr::filter(area_ha > 0,
                  !is.na(area_ha)) %>%
    dplyr::group_by(xy_id) %>%
    dplyr::summarize(n_warnings = dplyr::n(),
                     area_ha = dplyr::first(area_ha),
                     xy_id = dplyr::first(xy_id),
                     UF = dplyr::first(UF)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(area_type = cut(area_ha,
                                  breaks = area_breaks,
                                  labels = names(area_breaks)[-1])) %>%
    dplyr::group_by(UF, n_warnings, area_type) %>%
    dplyr::summarize(area_ha = sum(area_ha)) %>%
    dplyr::ungroup() %>%
    #dplyr::show_query()
    tibble::as_tibble() %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = n_warnings,
                                   y = area_ha,
                                   fill = area_type),
                      stat = "identity") +
    ggplot2::xlab("Number of wanings.") +
    ggplot2::ylab("Area (ha)") +
    ggplot2::labs(fill = "Area less than") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    #ggplot2::scale_y_log10(labels = scales::comma) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::facet_wrap(~UF, scales = "free")

if (interactive()) {
    plot_area_by_warnings_state +
        ggplot2::ggtitle(paste("Number of DETER warnings by area in the",
                               "Amazon by Brazilian state"))
}else{
    ggplot2::ggsave(
        plot = plot_area_by_warnings_state,
        filename = file.path(out_dir, "plot_area_by_warnings_state.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
}

rm(plot_area_by_warnings_state, warnings_by_subarea)



#---- Plot days between warnings by area ----

plot_days_first_to_last <-
    plot_data %>%
    dplyr::select(xy_id, diff_days, area_ha, UF) %>%
    dplyr::group_by(xy_id) %>%
    dplyr::summarize(n_warnings = dplyr::n(),
                     area_ha = dplyr::first(area_ha),
                     xy_id = dplyr::first(xy_id),
                     UF = dplyr::first(UF),
                     days_first_last = sum(diff_days, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_warnings > 1,
                  days_first_last > 0,
                  area_ha > 3,
                  !is.na(area_ha)) %>%
    dplyr::mutate(area_type = cut(area_ha,
                                  breaks = area_breaks,
                                  labels = names(area_breaks)[-1])) %>%
    dplyr::arrange(UF, area_type, days_first_last) %>%
    tibble::as_tibble() %>%
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = area_type, y = days_first_last)) +
    ggplot2::facet_grid(rows = vars(n_warnings),
                        cols = vars(UF)) +
    #ggplot2::xlab("Number of DETER warnings") +
    ggplot2::xlab("") +
    ggplot2::ylab("Days from first to last DETER warning") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::geom_hline(yintercept = 365,  linetype = 3, color = "gray50") +
    ggplot2::geom_hline(yintercept = 730,  linetype = 3, color = "gray50") +
    ggplot2::geom_hline(yintercept = 1095, linetype = 3, color = "gray50") +
    ggplot2::geom_hline(yintercept = 1460, linetype = 3, color = "gray50") +
    ggplot2::geom_hline(yintercept = 1825, linetype = 3, color = "gray50") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                                       hjust=1))



