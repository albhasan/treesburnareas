


get_plot_area_by_state_year_type <- function(subarea_dt) {
    subarea_dt %>%
        dplyr::filter(data_source == "DETER") %>%
        dplyr::group_by(UF, CLASSNAME, year) %>%
        dplyr::summarize(area_ha = sum(area_ha)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(UF = forcats::fct_relevel(UF, sort),
                      year = forcats::fct_relevel(as.character(year),
                                                  sort)) %>%
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
                                                fontface = "plain",
                                                size = 24) +
        ggplot2::theme(legend.title = ggplot2::element_blank()) %>%
        return()
}



# NOTE: This plot shows uses the CLASSNAME of the first DETER warning.
get_plot_density_area_ndays <- function(subarea_dt) {
    subarea_dt %>%
        dplyr::filter(data_source == "DETER") %>%
        dplyr::group_by(xy_id) %>%
        dplyr::arrange(VIEW_DATE, .by_group = TRUE) %>%
        dplyr::mutate(last_CLASSNAME = dplyr::lag(CLASSNAME),
                      last_VIEW_DATE = dplyr::lag(VIEW_DATE),
                      diff_days = as.vector(difftime(VIEW_DATE,
                                                     last_VIEW_DATE,
                                                     units = "days"))) %>%
        dplyr::ungroup() %>%
        dplyr::filter(area_ha > 3,
                      diff_days > 0,
                      !is.na(diff_days),
                      !is.na(last_CLASSNAME)) %>%
        dplyr::mutate(UF = forcats::fct_relevel(UF, sort),
                      year = forcats::fct_relevel(as.character(year),
                                                  sort)) %>%
        tibble::as_tibble() %>%
        ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = diff_days,
                                         y = area_ha,
                                         color = year),
                            size = 0.1,
                            na.rm = TRUE) +
        ggplot2::geom_density_2d(ggplot2::aes(x = diff_days,
                                              y = area_ha),
                                 contour_var = "density",
                                 na.rm = TRUE) +
        ggplot2::scale_y_log10(labels = scales::comma) +
        ggplot2::facet_grid(rows = ggplot2::vars(UF),
                            cols = ggplot2::vars(CLASSNAME)) +
        ggplot2::geom_vline(xintercept = 365, linetype = 3, color = "gray50") +
        ggplot2::geom_vline(xintercept = 730, linetype = 3, color = "gray50") +
        ggplot2::geom_vline(xintercept = 1095,linetype = 3, color = "gray50") +
        ggplot2::geom_vline(xintercept = 1460,linetype = 3, color = "gray50") +
        ggplot2::geom_vline(xintercept = 1825,linetype = 3, color = "gray50") +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::xlab("Number of days between warnings") +
        ggplot2::ylab("Area (ha)") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) %>%
        return()
}


get_plot_area_by_warnings <- function(subarea_dt) {
    subarea_dt %>%
        dplyr::filter(data_source == "DETER") %>%
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
        tibble::as_tibble() %>%
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes(x = n_warnings,
                                       y = area_ha,
                                       fill = area_type),
                          position = "dodge",
                          stat = "identity") +
        ggplot2::xlab("Number of wanings.") +
        ggplot2::ylab("Area (ha)") +
        ggplot2::labs(fill = "Area less than") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_colour_viridis_d() %>%
        return()
}


get_plot_area_by_warnings_state <- function(subarea_dt){
    subarea_dt %>%
        dplyr::filter(data_source == "DETER") %>%
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
        tibble::as_tibble() %>%
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes(x = n_warnings,
                                       y = area_ha,
                                       fill = area_type),
                          position = "dodge",
                          stat = "identity") +
        ggplot2::xlab("Number of wanings.") +
        ggplot2::ylab("Area (ha)") +
        ggplot2::labs(fill = "Area less than") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_colour_viridis_d() +
        ggplot2::facet_wrap(~UF, scales = "free") %>%
        return()
}


get_plot_days_first_to_last <- function(subarea_dt){
    subarea_dt %>%
        dplyr::filter(data_source == "DETER") %>%
        dplyr::group_by(xy_id) %>%
        dplyr::arrange(VIEW_DATE, .by_group = TRUE) %>%
        dplyr::mutate(last_CLASSNAME = dplyr::lag(CLASSNAME),
                      last_VIEW_DATE = dplyr::lag(VIEW_DATE),
                      diff_days = as.vector(difftime(VIEW_DATE, last_VIEW_DATE,
                                                     units = "days"))) %>%
        dplyr::ungroup() %>%
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
                      area_ha > 3) %>%
        dplyr::mutate(area_type = cut(area_ha,
                                      breaks = area_breaks,
                                      labels = names(area_breaks)[-1])) %>%
        dplyr::arrange(UF, area_type, days_first_last) %>%
        tibble::as_tibble() %>%
        ggplot2::ggplot() +
        ggplot2::geom_boxplot(ggplot2::aes(x = area_type,
                                           y = days_first_last)) +
        ggplot2::facet_grid(rows = vars(n_warnings),
                            cols = vars(UF)) +
        ggplot2::xlab("") +
        ggplot2::ylab("Days from first to last DETER warning") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::geom_hline(yintercept = 365, linetype = 3, color = "gray50") +
        ggplot2::geom_hline(yintercept = 730, linetype = 3, color = "gray50") +
        ggplot2::geom_hline(yintercept = 1095,linetype = 3, color = "gray50") +
        ggplot2::geom_hline(yintercept = 1460,linetype = 3, color = "gray50") +
        ggplot2::geom_hline(yintercept = 1825,linetype = 3, color = "gray50") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                           vjust = 1,
                                                           hjust = 1)) %>%
        return()
}

