###############################################################################
# Analysis of the recurrence of DETER warnings.
#------------------------------------------------------------------------------
# NOTE:
# - input data is a QGIS geopackage with DETER warnings after running
#   fix_geometries, multi to single part, union, and multi to single part
#    (again) operations.
# TODO:
# - Time to PRODES
###############################################################################
#devtools::install_github("davidsjoberg/ggsankey")

library(data.table)
library(dtplyr)
library(dplyr)
library(sf)
library(units)
library(stringr)
library(treemapify)
library(forcats)
library(ggplot2)
library(ggsankey)
library(terra)



#---- Configuration ----

base_dir <- "~/Documents/trees_lab/deter_warning_recurrence"
out_dir <- file.path(base_dir, "img")
deter_gpk <- "/home/alber/Documents/data/deter/amazonia_legal/deter.gpkg"
deter_lyr <- "deter_public_fix_m2s_union_m2s"
prodes_raster <- "~/Documents/data/prodes/amazonia/PDigital2000_2021_AMZ_raster_v20220915_bioma.tif"


stopifnot("Base directory not found!" = dir.exists(base_dir))
stopifnot("Out directory not found!" = dir.exists(out_dir))
stopifnot("GeoPackage not found!" = file.exists(deter_gpk))
stopifnot("DETER raster not found!" = file.exists(prodes_raster))

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

sf::sf_use_s2(FALSE)


#---- Load data ----

# NOTE:
# - deter_id identifies the original DETER features.
# - subarea_id identifies features afer applying QGIS' union operation and
#   pre-processing.
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
                                        as.integer(year(start_date)) + 1,
                                        as.integer(year(start_date)))) %>%
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
    dplyr::mutate(last_CLASSNAME = dplyr::lag(CLASSNAME),
                  last_VIEW_DATE = dplyr::lag(VIEW_DATE),
                  diff_days = as.vector(difftime(VIEW_DATE, last_VIEW_DATE,
                                                 units = "days"))) %>%
    dplyr::ungroup()



#---- Treemap: DETER warning area by state, year, warning type and area ----

plot_area_by_state_year_type_area <-
    plot_data %>%
    dplyr::filter(area_ha > 0,
                  !is.na(area_ha)) %>%
    dplyr::group_by(UF, CLASSNAME, year) %>%
    dplyr::summarize(area_ha = sum(area_ha)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(UF = forcats::fct_relevel(UF, sort),
                  year = forcats::fct_relevel(as.character(year), sort),
                  CLASSNAME = dplyr::recode(CLASSNAME, !!!deter_classes),
                  CLASSNAME = forcats::fct_relevel(CLASSNAME, sort)) %>%
    #dplyr::show_query()
    tibble::as_tibble() %>%
    ggplot2::ggplot(ggplot2::aes(area = area_ha,
                                 fill = CLASSNAME,
                                 label = sprintf("%1.1f ha", area_ha),
                                 subgroup = UF,
                                 subgroup2 = year)) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_subgroup_border(colour = "white", size = 10) +
    treemapify::geom_treemap_subgroup2_border(colour = "white", size = 2) +
    treemapify::geom_treemap_text(colour = "white", place = "centre",
                                  size = 10, grow = FALSE) +
    treemapify::geom_treemap_subgroup_text(place = "topleft", grow = FALSE,
                                           alpha = 0.3, colour = "black",
                                           fontface = "bold", size = 96) +
    treemapify::geom_treemap_subgroup2_text(place = "centre", grow = FALSE,
                                            alpha = 0.4, colour = "black",
                                            fontface = "plain", size = 24) +
    ggplot2::theme(legend.title = ggplot2::element_blank())

if (interactive()) {
    plot_area_by_state_year_type_area +
        ggplot2::ggtitle(paste("Area of DETER warnings by state, year",
                               "(PRODES), and type"))
}else{
    ggplot2::ggsave(
        plot = plot_area_by_state_year_type_area,
        filename = file.path(out_dir, "plot_deter_area_by_state_pyear_type.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
}



#---- Point density: DETER subareas by state, year, warning type and area ----

# NOTE: This plot shows uses the CLASSNAME of the first DETER warning.
plot_density_area_ndays <-
    plot_data %>%
    dplyr::filter(area_ha > 3,
                  !is.na(area_ha),
                  diff_days > 0,
                  !is.na(diff_days),
                  !is.na(last_CLASSNAME)) %>%
    dplyr::mutate(UF = forcats::fct_relevel(UF, sort),
                  CLASSNAME = dplyr::recode(CLASSNAME, !!!deter_classes),
                  CLASSNAME = forcats::fct_relevel(CLASSNAME, sort)) %>%
    #dplyr::show_query()
    tibble::as_tibble() %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = diff_days,
                                     y = area_ha),
                        na.rm = TRUE) +
    ggplot2::geom_density_2d(ggplot2::aes(x = diff_days,
                                          y = area_ha,
                                          contour_var = "density"),
                             na.rm = TRUE) +
    ggplot2::scale_y_log10(labels = scales::comma) +
    ggplot2::facet_grid(rows = vars(UF),
                        cols = vars(CLASSNAME)) +
    ggplot2::geom_vline(xintercept = 365,  linetype = 3, color = "gray50") +
    ggplot2::geom_vline(xintercept = 730,  linetype = 3, color = "gray50") +
    ggplot2::geom_vline(xintercept = 1095, linetype = 3, color = "gray50") +
    ggplot2::geom_vline(xintercept = 1460, linetype = 3, color = "gray50") +
    ggplot2::geom_vline(xintercept = 1825, linetype = 3, color = "gray50") +
    ggplot2::xlab("Number of days between warnings") +
    ggplot2::ylab("Area (ha)")

if (interactive()) {
     plot_density_area_ndays +
        ggplot2::ggtitle(paste("DETER area by subarea, state, first type, and",
                               "number of days between warnings"))
}else{
    ggplot2::ggsave(
        plot = plot_density_area_ndays,
        filename = file.path(out_dir,
                     "plot_deter_subarea_density_by_state_first-type_nwarnings.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
}





#---- Plot DETER subareas by number of warnings ----

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
        ggplot2::ggtitle(paste("DETER area by subarea and number of",
                               "warnings in the Brazilian Amazon"))
}else{
    ggplot2::ggsave(
        plot = plot_area_by_warnings,
        filename = file.path(out_dir, "plot_deter_subarea_by_nwarnings.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
}

rm(plot_area_by_warnings)



#---- Plot DETER subareas by number of warnings by state ----

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
        ggplot2::ggtitle(paste("Area of DETER warnings by number of warnings",
                               "and state"))
}else{
    ggplot2::ggsave(
        plot = plot_area_by_warnings_state,
        filename = file.path(out_dir, "plot_area_by_warnings_state.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
}

rm(plot_area_by_warnings_state)



#---- Plot days between warnings by subarea ----

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

if (interactive()) {
    plot_days_first_to_last +
        ggplot2::ggtitle(paste("Number of days from the first to the last",
                               "DETER warnings",
                               "Amazon by Brazilian state"))
}else{
    ggplot2::ggsave(
        plot =  plot_days_first_to_last,
        filename = file.path(out_dir, "plot_days_first_to_last.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
}

rm(plot_days_first_to_last)



#---- Sankey: Trajectories of subareas ----

# Get unique DETER poligons.
unique_xy <-
    plot_data %>%
    dplyr::select(xy_id, subarea_id) %>%
    dplyr::group_by(xy_id) %>%
    dplyr::summarize(n_warnings = dplyr::n(),
                     subarea_id = dplyr::first(subarea_id)) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()
unique_xy  <-
    union_sf %>%
    dplyr::right_join(unique_xy, by = "subarea_id")

#' Compute the vector's mode.
#'
#' @param x A vector.
#' @return  A vector.
the_mode <- function(x) {
    x <- x[!is.na(x)]
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# Get the PRODES mode in each subarea.
prodes_r <- terra::rast(prodes_raster)
subarea_prodes <-
    terra::extract(x = prodes_r,
                   y = terra::vect(unique_xy["xy_id"]),
                   fun = the_mode,
                   ID = TRUE,
                   weights = FALSE,
                   exact = FALSE,
                   touches = FALSE,
                   bind = FALSE,
                   raw = FALSE)

# TODO: Recode subarea_prodes' PRODES codes to PRODES years.
# TODO: Join suabrea_prodes to plot_tb and compute again Sankey's.

plot_tb <-
    plot_data %>%
    dplyr::filter(area_ha > 3,
                  !is.na(area_ha),
                  diff_days > 0,
                  !is.na(diff_days),
                  !is.na(CLASSNAME),
                  !is.na(last_CLASSNAME)) %>%
    dplyr::mutate(CLASSNAME = dplyr::recode(CLASSNAME, !!!deter_classes),
                  last_CLASSNAME = dplyr::recode(last_CLASSNAME,
                                                 !!!deter_classes)) %>%
    dplyr::group_by(xy_id) %>%
    dplyr::mutate(subarea_pos = stringr::str_c("pos_",
                                               dplyr::row_number()),
                  n_warnings = dplyr::n()) %>%
    dplyr::select(xy_id, CLASSNAME, subarea_pos, area_ha, n_warnings) %>%
    tidyr::pivot_wider(names_from = subarea_pos, values_from = CLASSNAME) %>%
    dplyr::filter(n_warnings > 1) %>%
    tibble::as_tibble()

# TODO: Make sankey use the variable area_ha
for (i in unique(sort(plot_tb[["n_warnings"]]))) {
    my_plot <-
        plot_tb %>%
        dplyr::filter(n_warnings == i) %>%
        dplyr::select_if(~!all(is.na(.))) %>%
        ggsankey::make_long(tidyselect::starts_with("pos_")) %>%
        ggplot2::ggplot(ggplot2::aes(x = x,
                                     next_x = next_x,
                                     node = node,
                                     next_node = next_node,
                                     fill = factor(node),
                                     label = node)) +
        ggsankey::geom_sankey() +
        ggsankey::theme_sankey(base_size = 16) +
        ggsankey::geom_sankey(flow.alpha = .6, node.color = "gray30") +
        ggsankey::geom_sankey_label() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = NULL)
    if (interactive()) {
         my_plot +
            ggplot2::ggtitle("Trajectory of subareas")
    }else{
        ggplot2::ggsave(
            plot = my_plot,
            filename = file.path(out_dir,
                         paste0("plot_subarea_trajectory_", i, ".png")),
            height = 210,
            width = 297,
            units = "mm"
        )
    }
}

rm(my_plot)
rm(plot_tb)

