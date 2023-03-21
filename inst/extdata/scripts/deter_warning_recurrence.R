#!/usr/bin/env Rscript

###############################################################################
# Analysis of the recurrence of DETER warnings.
#------------------------------------------------------------------------------
# NOTE:
# - input data is a geopackage with DETER warnings after running
#   fix_geometries, multi to single part, union, and multi to single part
#    (again) operations.
# - ggsankey isn't availabe at CRAN. Install it from github
#   using this line of code devtools::install_github("davidsjoberg/ggsankey")
# - if PRODES is updated, also updated its classes codes (see file
#   PDigital2000_2021_AMZ_raster_v20220915_bioma.txt) in the prodes_classes
#   variable.
#
# TODO:
# - Do again the first DETER figure, use similar colors for deforestaion
#   (desmatamento com solo exposto, desmatamento com vegetacao, e mineracao)
#   and degradation classes. Also, use squares in all the data. Also, use only
#   data from the Amazon biome instead of BLA.
# - Create new trajectory figures using only DETER data.
# - Create trajectory figures but only of those trajectories that become
#   deforestation in PRODES.
# - Cross data with MODIS Aqua' focos de calos. Ask Guilherme.
# - Proposal:
#   Desmatamento = solo exposto, desmatamento com vegetacao, e mineracao.
#   Corte selectivo = ordenado e desordenado.
#   Degradacao = degradacao.
#   Cicatriz de queimada = cicatriz de queimada.
#
# - Update to GRASS geopackage. Use the data from ~/Documents/data/deter/amazonia_legal/deter_grass.gpkg
# - Run Sankey by state.
# - Download SHP fire calendar from ZENODO by Nathalia Carvalho.
# - Download  GWIS (ask Guilherme).
# - Compute table with areas by trajectory
# - Create map and add to slides.
# - Convert trajectories back to shapefile.
# - Select some weird trajectories and add them to the slides.
#
# DONE:
# - Time to PRODES
# - Add Amazonia Legal to maps. NOTE: PRODES mask added.








#---- Treemap: DETER warning area by state, year, warning type and area ----

plot_area_by_state_year_type <- get_plot_area_by_state_year_type(subarea_dt)

if (save_figs) {
    ggplot2::ggsave(
        plot = plot_area_by_state_year_type,
        filename = file.path(out_dir,
                             "plot_deter_area_by_state_pyear_type.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
} else {
    plot_area_by_state_year_type +
        ggplot2::ggtitle(paste("Area of DETER warnings by state, year",
                               "(PRODES), and type"))
}

rm(plot_area_by_state_year_type)



#---- Point density: DETER subareas by state, year, warning type and area ----

# NOTE: This plot shows uses the CLASSNAME of the first DETER warning.
plot_density_area_ndays <- get_plot_density_area_ndays(subarea_dt)

if (save_figs) {
    ggplot2::ggsave(
        plot = plot_density_area_ndays,
        filename = file.path(out_dir,
              "plot_deter_subarea_density_by_state_first-type_nwarnings.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
} else {
     plot_density_area_ndays +
        ggplot2::ggtitle(paste("DETER area by subarea, state, first type, and",
                               "number of days between warnings"))
}

rm(plot_density_area_ndays)



#---- Histogram DETER subareas by number of warnings ----

plot_area_by_warnings <- get_plot_area_by_warnings(subarea_dt)

if (save_figs) {
    ggplot2::ggsave(
        plot = plot_area_by_warnings,
        filename = file.path(out_dir, "plot_deter_subarea_by_nwarnings.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
} else {
    plot_area_by_warnings +
        ggplot2::ggtitle(paste("DETER area by subarea and number of",
                               "warnings in the Brazilian Amazon"))
}

rm(plot_area_by_warnings)



#---- Histogram DETER subareas by number of warnings by state ----

plot_area_by_warnings_state <- get_plot_area_by_warnings_state(subarea_dt)

if (save_figs) {
    ggplot2::ggsave(
        plot = plot_area_by_warnings_state,
        filename = file.path(out_dir, "plot_deter_subarea_by_warnings_state.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
} else {
    plot_area_by_warnings_state +
        ggplot2::ggtitle(paste("Area of DETER warnings by number of warnings",
                               "and state"))
}

rm(plot_area_by_warnings_state)



#---- Boxplot days between warnings by subarea ----

plot_days_first_to_last <- get_plot_days_first_to_last(subarea_dt)

if (save_figs) {
    ggplot2::ggsave(
        plot =  plot_days_first_to_last,
        filename = file.path(out_dir, "plot_deter_days_first_to_last.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
} else {
    plot_days_first_to_last +
        ggplot2::ggtitle(paste("Number of days from the first to the last",
                               "DETER warnings",
                               "Amazon by Brazilian state"))
}

rm(plot_days_first_to_last)



#---- Sankey: Trajectories of subareas ----

plot_tb <-
    subarea_dt %>%
    dplyr::filter(area_ha > 3 | is.na(area_ha),
                  !is.na(CLASSNAME)) %>%
    dplyr::arrange(xy_id, VIEW_DATE_est) %>%
    dplyr::group_by(xy_id) %>%
    dplyr::mutate(CLASSNAME = dplyr::if_else(
                      stringr::str_starts(CLASSNAME, "d"),
                      "P_deforestation",
                      CLASSNAME
                  ),
                  CLASSNAME = dplyr::if_else(
                      stringr::str_starts(CLASSNAME, "r"),
                      "P_residue",
                      CLASSNAME
                  ),
                  last_CLASSNAME = dplyr::lag(CLASSNAME),
                  last_VIEW_DATE_est = dplyr::lag(VIEW_DATE_est),
                  diff_days = as.vector(difftime(VIEW_DATE_est,
                                                 last_VIEW_DATE_est,
                                                 units = "days")),
                  n_warn_p = dplyr::n()) %>%
    tidyr::fill(area_ha, .direction = "downup") %>%
    dplyr::ungroup() %>%
    dplyr::select(area_ha, diff_days,
                  CLASSNAME, last_CLASSNAME,
                  xy_id, n_warn_p, data_source)  %>%
    #dplyr::filter(diff_days > 0 | is.na(diff_days)) %>%
    data.table::as.data.table()


# TODO: Make sankey use the variable area_ha
for (i in sort(unique(plot_tb$n_warn_p))) {
    if (i == 1)
        next

    my_plot <-
        plot_tb %>%
        dplyr::filter(n_warn_p == i) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(group_row = dplyr::row_number(),
                      subarea_step = stringr::str_c("step_", group_row)) %>%
        dplyr::ungroup() %>%
        dplyr::select(xy_id, CLASSNAME, subarea_step, area_ha, n_warn_p) %>%
        tidyr::pivot_wider(names_from = subarea_step,
                           values_from = CLASSNAME) %>%
        dplyr::select_if(~!all(is.na(.))) %>%
        tidyr::drop_na() %>%
        dplyr::as_tibble() %>%
        ggsankey::make_long(tidyselect::starts_with("step_")) %>%
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

    if (save_figs) {
        ggplot2::ggsave(
            plot = my_plot,
            filename = file.path(out_dir,
                         paste0("plot_deter_subarea_trajectory_", i, ".png")),
            height = 210,
            width = 297,
            units = "mm"
        )
    }else{
         my_plot +
            ggplot2::ggtitle("Trajectory of subareas")
    }
}


rm(my_plot)
rm(plot_tb)

