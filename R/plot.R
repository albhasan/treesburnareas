#' @title Build a treemap using of DETER's deforestation warnings.
#'
#' @name get_plot_area_by_state_year_type
#'
#' @description
#' This function creates a TreeMap figure using deforestation warnings in the
#' brazilian amazon. The figure uses DETER warnings area aggregated by state,
#' PRODES year, and warning's type and area.
#' @param subarea_dt   A data.table object hosted in this package.
#' @param class_levels A character. Names used to recode DETER's CLASSNAME.
#' @return             A ggplot2 object.
#' @export
get_plot_area_by_state_year_type <- function(subarea_dt, class_levels) {
    #---- Column names ----
    area_col  <- "subarea_ha" #
    label_col <- "CLASSNAME"  #
    state_col <- "UF"         # Name of the state.
    year_col <- "year"        # PRODES year.
    #---- Prepapre data ----
    p_years <- sort(unique(subarea_dt[[year_col]]))
    b_states <- sort(unique(subarea_dt[[state_col]]))
    subarea_dt %>%
        dplyr::group_by(.data[[state_col]],
                        .data[[label_col]],
                        .data[[year_col]]) %>%
        dplyr::summarize(subarea_km2 = sum(.data[[area_col]])/100) %>%
        dplyr::ungroup() %>%
        tibble::as_tibble() %>%
        dplyr::mutate(year = factor(as.character(.data[[year_col]]),
                                    levels = as.character(p_years),
                                    ordered = TRUE),
                      UF = factor(.data[[state_col]],
                                  levels = b_states,
                                  ordered = TRUE),
                      CLASSNAME = factor(.data[[label_col]],
                                         levels = class_levels,
                                         ordered = TRUE)) %>%
        dplyr::arrange(.data[[state_col]],
                       .data[[label_col]],
                       .data[[year_col]],
                       .data[[area_col]]) %>%
    #---- Plot ----
    # NOTE: To use personalized colors, use a named vector "cat" = "hexcolor"
    #       and pass it to ggplot2:scale_fill_manual(values = my_colors).
        ggplot2::ggplot(ggplot2::aes_string(area = area_col,
                                            fill = label_col,
                                            label = sprintf("%1.1f km2",
                                                            area_col),
                                            subgroup = state_col,
                                            subgroup2 = year_col)) +
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
    #---- Column names ----
    area_col  <- "subarea_ha"    # Area.
    date_col  <- "VIEW_DATE"     # View date.
    label_col <- "CLASSNAME"     # Label column.
    said_col  <- "xy_id"         # Subarea ID column.
    state_col <- "UF"            # Name of the state.
    year_col  <- "year"          # PRODES year.
    #---- Prepare data ----
    subarea_dt %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::arrange(.data[[date_col]], .by_group = TRUE) %>%
        dplyr::mutate(last_CLASSNAME = dplyr::lag(.data[[label_col]]),
                      last_VIEW_DATE = dplyr::lag(.data[[date_col]]),
                      diff_days = as.vector(difftime(.data[[date_col]],
                                                     .data[["last_VIEW_DATE"]],
                                                     units = "days"))) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data[[area_col]] > 3,
                      .data[["diff_days"]] > 0,
                      !is.na(.data[["diff_days"]]),
                      !is.na(.data[["last_CLASSNAME"]])) %>%
        dplyr::mutate(
            UF = forcats::fct_relevel(.data[[state_col]], sort),
            year = forcats::fct_relevel(as.character(.data[[year_col]]),
                                        sort)
        ) %>%
        tibble::as_tibble() %>%
    #---- Plot ----
        ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes_string(x = "diff_days",
                                         y = area_col,
                                         color = year_col),
                            size = 0.1,
                            na.rm = TRUE) +
        ggplot2::geom_density_2d(ggplot2::aes_string(x = "diff_days",
                                                     y = area_col),
                                 contour_var = "density",
                                 na.rm = TRUE) +
        ggplot2::scale_y_log10(labels = scales::comma) +
        ggplot2::facet_grid(stats::reformulate(state_col, label_col)) +
        ggplot2::geom_vline(xintercept = 365, linetype = 3, color = "gray50") +
        ggplot2::geom_vline(xintercept = 730, linetype = 3, color = "gray50") +
        ggplot2::geom_vline(xintercept = 1095,linetype = 3, color = "gray50") +
        ggplot2::geom_vline(xintercept = 1460,linetype = 3, color = "gray50") +
        ggplot2::geom_vline(xintercept = 1825,linetype = 3, color = "gray50") +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::xlab("Number of days between warnings") +
        ggplot2::ylab("Subarea (ha)") +
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
    stopifnot("area_breaks must be a named vector" =
              !is.null(names(area_breaks)))
    #---- Column names ----
    area_col  <- "subarea_ha"    # Area.
    said_col  <- "xy_id"         # Subarea ID column.
    #---- Prepare data ----
    subarea_dt %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::summarize(n_warnings = dplyr::n(),
                         subarea_ha = dplyr::first(.data[[area_col]]),
                         xy_id = dplyr::first(.data[[said_col]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(subarea_type = cut(.data[[area_col]],
                                         breaks = area_breaks,
                                         labels = names(area_breaks)[-1])) %>%
        dplyr::group_by(.data[["n_warnings"]],
                        .data[["subarea_type"]]) %>%
        dplyr::summarize(subarea_ha = sum(.data[[area_col]])) %>%
        dplyr::ungroup() %>%
        tibble::as_tibble() %>%
        ggplot2::ggplot() +
        ggplot2::geom_col(ggplot2::aes_string(x = "n_warnings",
                                              y = area_col,
                                              fill = "subarea_type"),
                          position = "dodge") +
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
    #---- Column names ----
    area_col  <- "subarea_ha"    # Area.
    said_col  <- "xy_id"         # Subarea ID column.
    state_col <- "UF"            # Name of the state.
    #---- Prepare data ----
    subarea_dt %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::summarize(n_warnings = dplyr::n(),
                         subarea_ha = dplyr::first(.data[[area_col]]),
                         xy_id = dplyr::first(.data[[said_col]]),
                         UF = dplyr::first(.data[[state_col]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(area_type = cut(.data[[area_col]],
                                      breaks = area_breaks,
                                      labels = names(area_breaks)[-1])) %>%
        dplyr::group_by(.data[[state_col]],
                        .data[["n_warnings"]],
                        .data[["area_type"]]) %>%
        dplyr::summarize(subarea_ha = sum(.data[[area_col]])) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(prop = prop.table(.data[[area_col]]),
                      prop = dplyr::if_else(.data[["n_warnings"]] > 2, NA,
                                            .data[["prop"]])) %>%
        tibble::as_tibble() %>%
    #---- Plot ----
        ggplot2::ggplot() +
        ggplot2::geom_col(ggplot2::aes_string(x = "n_warnings",
                                              y = area_col,
                                              fill = "area_type"),
                          position = "dodge") +
        ggplot2::xlab("Number of wanings.") +
        ggplot2::ylab("Subarea (ha)") +
        ggplot2::labs(fill = "Subarea less than") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_colour_viridis_d() +
        ggplot2::facet_wrap(~UF, scales = "free") %>%
        return()
}


#' @title Build a histogram using the classes of DETER.
#'
#' @name get_plot_area_by_class
#'
#' @description
#' This function creates a histogram figure using the classes of the
#' deforestation warnings by subarea in the brazilian amazon. The figure shows
#' the area by DETER class, and year (PRODES).
#' @param subarea_dt  A data.table object hosted in this package.
#' @return            A ggplot2 object.
#' @export
get_plot_area_by_class <- function(subarea_dt) {
    #---- Column names ----
    area_col  <- "subarea_ha"    # Area.
    label_col <- "CLASSNAME"     # Label column.
    year_col  <- "year"          # PRODES year.
    #---- Prepare data ----
    subarea_dt %>%
        tibble::as_tibble() %>%
        dplyr::group_by(.data[[label_col]],
                        .data[[year_col]]) %>%
        dplyr::summarize(subarea_ha = sum(.data[[area_col]])) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes_string(x = year_col,
                                              y = area_col,
                                              fill = label_col),
                          position = "dodge",
                          stat = "identity") +
        ggplot2::xlab("Year (PRODES)") +
        ggplot2::ylab("Area (ha)") +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::scale_colour_viridis_d() %>%
        return()
}

#' @title Build a histogram using the classes of DETER by Brazilian state.
#'
#' @name get_plot_area_by_class_state
#'
#' @description
#' This function creates a histogram figure using the classes of the
#' deforestation warnings by subarea and state in the brazilian amazon. The
#' figure shows the area by DETER class, and year (PRODES).
#' @param subarea_dt  A data.table object hosted in this package.
#' @return            A ggplot2 object.
#' @export
get_plot_area_by_class_state <- function(subarea_dt) {
    #---- Column names ----
    area_col  <- "subarea_ha"    # Area.
    label_col <- "CLASSNAME"     # Label column.
    state_col <- "UF"            # Name of the state.
    year_col  <- "year"          # PRODES year.
    #---- Prepare data ----
    subarea_dt %>%
        tibble::as_tibble() %>%
        dplyr::group_by(.data[[label_col]],
                        .data[[year_col]],
                        .data[[state_col]]) %>%
        dplyr::summarize(subarea_ha = sum(.data[[area_col]])) %>%
        dplyr::ungroup() %>%
        #---- Plot ----
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes_string(x = year_col,
                                              y = area_col,
                                              fill = label_col),
                          position = "dodge",
                          stat = "identity") +
        ggplot2::xlab("Year (PRODES)") +
        ggplot2::ylab("Area (ha)") +
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
    #---- Column names ----
    area_col  <- "subarea_ha"    # Area.
    date_col  <- "VIEW_DATE"     # View date.
    label_col <- "CLASSNAME"     # Label column.
    said_col  <- "xy_id"         # Subarea ID column.
    state_col <- "UF"            # Name of the state.
    #---- Prepare data ----
    subarea_dt %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::arrange(.data[[date_col]],
                       .by_group = TRUE) %>%
        dplyr::mutate(last_CLASSNAME = dplyr::lag(.data[[label_col]]),
                      last_VIEW_DATE = dplyr::lag(.data[[date_col]]),
                      diff_days = as.vector(difftime(.data[[date_col]],
                                                     .data[["last_VIEW_DATE"]],
                                                     units = "days"))) %>%
        dplyr::ungroup() %>%
        dplyr::select(tidyselect::all_of(c(said_col, "diff_days",
                                           area_col, state_col))) %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::summarize(n_warnings = dplyr::n(),
                         "{area_col}"  := dplyr::first(.data[[area_col]]),
                         "{said_col}"  := dplyr::first(.data[[said_col]]),
                         "{state_col}" := dplyr::first(.data[[state_col]]),
                         days_first_last = sum(.data[["diff_days"]],
                                               na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data[["n_warnings"]] > 1,
                      .data[["days_first_last"]] > 0,
                      .data[[area_col]] > 3) %>%
        dplyr::mutate(
            area_type = cut(.data[[area_col]],
                            breaks = area_breaks,
                            labels = names(area_breaks)[-1])
        ) %>%
        dplyr::arrange(.data[[state_col]],
                       .data[["area_type"]],
                       .data[["days_first_last"]]) %>%
        #---- Plot ----
        tibble::as_tibble() %>%
        ggplot2::ggplot() +
        ggplot2::geom_boxplot(ggplot2::aes_string(x = "area_type",
                                                  y = "days_first_last")) +
        ggplot2::facet_grid(stats::reformulate("n_warnings", state_col)) +
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


#' @title Build a Sankey diagram of the given subareas
#'
#' @name get_plot_sankey
#'
#' @description
#' This function creates a Sankey diagram between DETER subareas for the given
#' number of warnings.
#' @param data_tb A tibble.
#' @return        A ggplot2 object.
#' @export
get_plot_sankey <- function(data_tb) {
    #---- Column names ----
    area_col  <- "subarea_ha"    # Area.
    label_col <- "CLASSNAME"     # Label column.
    said_col  <- "xy_id"         # Subarea ID column.
    #---- Prepare data ----
    data_tb %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::mutate(group_row = dplyr::row_number(),
                      subarea_step = stringr::str_c("step_",
                                                    .data[["group_row"]])) %>%
        dplyr::ungroup() %>%
        dplyr::select(tidyselect::all_of(c(said_col, label_col,
                                           "subarea_step", area_col))) %>%
        tidyr::pivot_wider(names_from = tidyselect::all_of("subarea_step"),
                           values_from = tidyselect::all_of(label_col)) %>%
        dplyr::select_if(~!all(is.na(.))) %>%
        tidyr::drop_na() %>%
        dplyr::as_tibble() %>%
        ggsankey::make_long(tidyselect::starts_with("step_")) %>%
        #---- Plot ----
        ggplot2::ggplot(ggplot2::aes_string(x = "x",
                                            next_x = "next_x",
                                            node = "node",
                                            next_node = "next_node",
                                            fill = factor("node"),
                                            label = "node")) +
        ggsankey::geom_sankey() +
        ggsankey::theme_sankey(base_size = 16) +
        ggsankey::geom_sankey(flow.alpha = .6, node.color = "gray30") +
        ggsankey::geom_sankey_label() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = NULL) %>%
        return()
}


#' @title Build a Sankey diagram of given subareas using their year
#'
#' @name get_plot_sankey_year
#'
#' @description
#' This function creates a Sankey diagram between DETER subareas for the given
#' number of warnings using their PRODES year.
#' @param data_tb A tibble.
#' @return        A ggplot2 object.
#' @export
get_plot_sankey_year <- function(data_tb) {
    #---- Column names ----
    area_col  <- "subarea_ha"    # Area.
    label_col <- "CLASSNAME"     # Label column.
    said_col  <- "xy_id"         # Subarea ID column.
    year_col  <- "year"          # PRODES year.
    #---- Prepapre data ----
     data_tb %>%
        dplyr::mutate(
            subarea_step = stringr::str_c("step_", .data[[year_col]])
        ) %>%
        dplyr::select(tidyselect::all_of(c(said_col, label_col,
                                           "subarea_step"))) %>%
        tidyr::pivot_wider(names_from  = tidyselect::all_of("subarea_step"),
                           values_from = tidyselect::all_of(area_col)) %>%
        dplyr::select(sort(colnames(.))) %>%
        ggsankey::make_long(tidyselect::starts_with("step_")) %>%
        #---- Plot ----
        ggplot2::ggplot(ggplot2::aes_string(x = "x",
                                            next_x = "next_x",
                                            node = "node",
                                            next_node = "next_node",
                                            fill = factor("node"),
                                            label = "node")) +
        ggsankey::geom_sankey() +
        ggsankey::theme_sankey(base_size = 16) +
        ggsankey::geom_sankey(flow.alpha = .6, node.color = "gray30") +
        ggsankey::geom_sankey_label() +
        ggplot2::theme(legend.position = "none") %>%
        return()
}

