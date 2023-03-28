#' @title Build a treemap using of DETER's deforestation warnings.
#'
#' @name get_plot_area_by_state_year_type
#'
#' @description
#' This function creates a TreeMap figure using deforestation warnings in the
#' brazilian amazon. The figure uses DETER warnings area aggregated by state,
#' PRODES year, and warning's type and area.
#' @param subarea_dt A data.table object hosted in this package.
#' @return           A ggplot2 object.
#' @export
get_plot_area_by_state_year_type <- function(subarea_dt) {
    data_source <- in_prodes <- UF <- CLASSNAME <- year <- subarea_ha <- NULL
    subarea_km2 <- NULL

    subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE) %>%
        dplyr::group_by(UF, CLASSNAME, year) %>%
        dplyr::summarize(subarea_ha = sum(subarea_ha)) %>%
        dplyr::ungroup() %>%
        tibble::as_tibble() %>%
        dplyr::mutate(subarea_km2 = subarea_ha/100,
                      year = factor(as.character(year),
                                    levels = c("2017", "2018", "2019",
                                               "2020", "2021"),
                                    ordered = TRUE),
                      UF = factor(UF, levels = c("AC", "AM", "AP", "MA", "MT",
                                                 "PA", "RO", "RR", "TO"),
                                  ordered = TRUE),
                      CLASSNAME = factor(CLASSNAME,
                                         levels = c("Mining",
                                                    "Burn scar",
                                                    "Clear cut",
                                                    "Geometric slash",
                                                    "Slash with veg.",
                                                    "Untidy slash",
                                                    "Selective cut",
                                                    "Degradation"),
                                         ordered = TRUE)) %>%
        dplyr::arrange(UF, CLASSNAME, year, subarea_km2) %>%
    # NOTE: To use personlaized colors, use a named vector "cat" = "hexcolor"
    #       and pass it to ggplot2:scale_fill_manual(values = my_colors).
        ggplot2::ggplot(ggplot2::aes(area = subarea_km2,
                                     fill = CLASSNAME,
                                     label = sprintf("%1.1f km2", subarea_km2),
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

#' @title Build a dot cloud figure using DETER deforestation warnings.
#'
#' @name get_plot_area_ndays
#'
#' @description
#' This function creates a dot cloud and density figure using deforestation
#' warnings subareas in the brazilian amazon. The figure uses DETER warnings
#' organized by number of days between warnings, area, warning class (of the
#' first warning), state, and PRODES year. Only subareas greater then 3 ha are
#' included.
#' @param subarea_dt A data.table object hosted in this package.
#' @return           A ggplot2 object.
#' @export
get_plot_density_area_ndays <- function(subarea_dt) {
    CLASSNAME <- data_source <- diff_days <- in_prodes <- NULL
    last_CLASSNAME <- last_VIEW_DATE <- subarea_ha <- UF <- VIEW_DATE <- NULL
    xy_id <- year <- NULL

    subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::arrange(VIEW_DATE, .by_group = TRUE) %>%
        dplyr::mutate(last_CLASSNAME = dplyr::lag(CLASSNAME),
                      last_VIEW_DATE = dplyr::lag(VIEW_DATE),
                      diff_days = as.vector(difftime(VIEW_DATE,
                                                     last_VIEW_DATE,
                                                     units = "days"))) %>%
        dplyr::ungroup() %>%
        dplyr::filter(subarea_ha > 3,
                      diff_days > 0,
                      !is.na(diff_days),
                      !is.na(last_CLASSNAME)) %>%
        dplyr::mutate(UF = forcats::fct_relevel(UF, sort),
                      year = forcats::fct_relevel(as.character(year),
                                                  sort)) %>%
        tibble::as_tibble() %>%
        ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = diff_days,
                                         y = subarea_ha,
                                         color = year),
                            size = 0.1,
                            na.rm = TRUE) +
        ggplot2::geom_density_2d(ggplot2::aes(x = diff_days,
                                              y = subarea_ha),
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
        ggplot2::ylab("Subrea (ha)") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) %>%
        return()
}

#' @title Build a histogram using DETER deforestation warnings.
#'
#' @name get_plot_area_by_warnings
#'
#' @description
#' This function creates a histogram figure using deforestation warnings
#' subareas in the brazilian amazon. The figure shows the number of warnnings
#' by subarea and subarea bins.
#' @param subarea_dt  A data.table object hosted in this package.
#' @param area_breaks A named numeric vector with the breaks of subareas by
#' size.
#' @return            A ggplot2 object.
#' @export
get_plot_area_by_warnings <- function(subarea_dt, area_breaks) {
    area_type <- data_source <- in_prodes <- n_warnings <- subarea_ha <- NULL
    subarea_type <- UF <- xy_id <- NULL

    subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::summarize(n_warnings = dplyr::n(),
                         subarea_ha = dplyr::first(subarea_ha),
                         xy_id = dplyr::first(xy_id)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(subarea_type = cut(subarea_ha,
                                         breaks = area_breaks,
                                         labels = names(area_breaks)[-1])) %>%
        dplyr::group_by(n_warnings, subarea_type) %>%
        dplyr::summarize(subarea_ha = sum(subarea_ha)) %>%
        dplyr::ungroup() %>%
        tibble::as_tibble() %>%
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes(x = n_warnings,
                                       y = subarea_ha,
                                       fill = subarea_type),
                          position = "dodge",
                          stat = "identity") +
        ggplot2::xlab("Number of wanings.") +
        ggplot2::ylab("Subarea (ha)") +
        ggplot2::labs(fill = "Subarea less than") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_colour_viridis_d() %>%
        return()
}

#' @title Build histograms by state using DETER deforestation warnings.
#'
#' @name get_plot_area_by_warnings_state
#'
#' @description
#' This function creates a histogram figure using deforestation warnings
#' subareas for each state in the brazilian amazon. The figure shows the number
#' of warnnings by subarea, state,  and subarea bins.
#' @param subarea_dt  A data.table object hosted in this package.
#' @param area_breaks A named numeric vector with the breaks of subareas by
#' size.
#' @return            A ggplot2 object.
#' @export
get_plot_area_by_warnings_state <- function(subarea_dt, area_breaks) {
    area_type <- data_source <- in_prodes <- n_warnings <- subarea_ha <- NULL
    UF <- xy_id <- NULL

    subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::summarize(n_warnings = dplyr::n(),
                         subarea_ha = dplyr::first(subarea_ha),
                         xy_id = dplyr::first(xy_id),
                         UF = dplyr::first(UF)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(area_type = cut(subarea_ha,
                                      breaks = area_breaks,
                                      labels = names(area_breaks)[-1])) %>%
        dplyr::group_by(UF, n_warnings, area_type) %>%
        dplyr::summarize(subarea_ha = sum(subarea_ha)) %>%
        dplyr::ungroup() %>%
        tibble::as_tibble() %>%
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes(x = n_warnings,
                                       y = subarea_ha,
                                       fill = area_type),
                          position = "dodge",
                          stat = "identity") +
        ggplot2::xlab("Number of wanings.") +
        ggplot2::ylab("Subarea (ha)") +
        ggplot2::labs(fill = "Subarea less than") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_colour_viridis_d() +
        ggplot2::facet_wrap(~UF, scales = "free") %>%
        return()
}

#' @title Build boxplots of the days from first to last DETER warning.
#'
#' @name get_plot_days_first_to_last
#'
#' @description
#' This function creates figure of boxplots by state and number of warnings of
#' number of days between the first and last DETER deforestation warning
#' associated to each subarea.
#' @param subarea_dt  A data.table object hosted in this package.
#' @param area_breaks A named numeric vector with the breaks of subareas by
#' size.
#' @return            A ggplot2 object.
#' @export
get_plot_days_first_to_last <- function(subarea_dt, area_breaks){
    area_type <- CLASSNAME <- data_source <- NULL
    days_first_last <- diff_days <- in_prodes <- last_VIEW_DATE <- NULL
    n_warnings <- subarea_ha <- UF <- VIEW_DATE <- xy_id <- NULL

    subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::arrange(VIEW_DATE, .by_group = TRUE) %>%
        dplyr::mutate(last_CLASSNAME = dplyr::lag(CLASSNAME),
                      last_VIEW_DATE = dplyr::lag(VIEW_DATE),
                      diff_days = as.vector(difftime(VIEW_DATE, last_VIEW_DATE,
                                                     units = "days"))) %>%
        dplyr::ungroup() %>%
        dplyr::select(xy_id, diff_days, subarea_ha, UF) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::summarize(n_warnings = dplyr::n(),
                         subarea_ha = dplyr::first(subarea_ha),
                         xy_id = dplyr::first(xy_id),
                         UF = dplyr::first(UF),
                         days_first_last = sum(diff_days, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n_warnings > 1,
                      days_first_last > 0,
                      subarea_ha > 3) %>%
        dplyr::mutate(area_type = cut(subarea_ha,
                                      breaks = area_breaks,
                                      labels = names(area_breaks)[-1])) %>%
        dplyr::arrange(UF, area_type, days_first_last) %>%
        tibble::as_tibble() %>%
        ggplot2::ggplot() +
        ggplot2::geom_boxplot(ggplot2::aes(x = area_type,
                                           y = days_first_last)) +
        ggplot2::facet_grid(rows = ggplot2::vars(n_warnings),
                            cols = ggplot2::vars(UF)) +
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

#' @title Build a Sankey diagram of DETER warnings.
#'
#' @name get_plot_sankey
#'
#' @description
#' This function creates a Sankey diagram between DETER subareas for the given
#' number of warnings.
#' @param data_tb A data.table object specifically prepared for this function.
#' @param n_warnings A single integer. Number of warnings to use.
#' @return            A ggplot2 object.
#' @export
get_plot_sankey <- function(data_tb, n_warnings) {
    CLASSNAME <- group_row <- n_warn_p <- next_x <- next_node <- node <- NULL
    subarea_ha <- subarea_step <- x <- xy_id <- NULL

    data_tb %>%
        dplyr::filter(n_warn_p == n_warnings) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(group_row = dplyr::row_number(),
                      subarea_step = stringr::str_c("step_", group_row)) %>%
        dplyr::ungroup() %>%
        dplyr::select(xy_id, CLASSNAME, subarea_step, subarea_ha, n_warn_p) %>%
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
        ggplot2::labs(x = NULL) %>%
        return()
}
