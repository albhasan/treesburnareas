#' @title Create EDA plots and tables
#'
#' @name create_plots
#'
#' @description
#' This function creates the figures and tables of the Exploratory Data
#' Analysis of the Amazonia deforestation warnings issued by DETER.
#' @param out_dir A path to a directory.
#' @return out_dir (invisible).
#' @examples
#' \dontrun{
#'     create_plots(out_dir = "~/Documents/report")
#' }
#' @export
create_plots <- function(out_dir) {
    #TODO: make  parameters out of subarea_sf & subarea_dt
    CLASSNAME <- data_source <- diff_days <- in_prodes <- NULL
    last_CLASSNAME <- n_warnings <- subarea_ha <- NULL
    warning_pos <- VIEW_DATE <- xy_id <- NULL
    area_ha <- max_area <- mean_area <- median_area <- min_area <- NULL
    sd_area <- NULL

    stopifnot("Directory not found!" = dir.exists(out_dir))


    #---- Setup ----

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

    # Exclude subareas smaller than this threshold:
    min_subarea_ha <- 3

    # Only include subareas ocurring before this date:
    max_date <- as.Date("2021-08-01")

    # Size of the figures.
    height <- 210
    width <- 297
    units <- "mm"

    # Directories for writing figures and tables.
    out_fig <- file.path(out_dir, "figures")
    out_tab <- file.path(out_dir, "tables")
    stopifnot("Directory for storing figures not found!" = dir.exists(out_fig))
    stopifnot("Directory for storing tables not found!" = dir.exists(out_tab))


    #---- Treemap: DETER warning area by state, year, warning type and area----

    plot_area_by_state_year_type <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE,
                      VIEW_DATE < max_date) %>%
        get_plot_area_by_state_year_type()
    ggplot2::ggsave(
        plot = plot_area_by_state_year_type,
        filename = file.path(out_fig,
                             "plot_deter_area_by_state_pyear_type.png"),
        height = height, width = width, units = units
    )


    #---- Point density: DETER subareas by state, year, warning type and area

    # NOTE: This plot shows uses the CLASSNAME of the first DETER warning.
    plot_density_area_ndays <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE,
                      VIEW_DATE < max_date) %>%
        get_plot_density_area_ndays()
    ggplot2::ggsave(
        plot = plot_density_area_ndays,
        filename = file.path(out_fig,
           "plot_deter_subarea_density_by_state_first-type_nwarnings.png"),
        height = height, width = width, units = units
    )


    #---- Histogram DETER subareas by number of warnings ----

    plot_area_by_warnings <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE,
                      VIEW_DATE < max_date) %>%
        get_plot_area_by_warnings(area_breaks)
    ggplot2::ggsave(
        plot = plot_area_by_warnings,
        filename = file.path(out_fig,
                             "plot_deter_subarea_by_nwarnings.png"),
        height = height, width = width, units = units
    )


    #---- Histogram DETER subareas by number of warnings by state ----

    plot_area_by_warnings_state <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE,
                      VIEW_DATE < max_date) %>%
        get_plot_area_by_warnings_state(area_breaks)
    ggplot2::ggsave(
        plot = plot_area_by_warnings_state,
        filename = file.path(out_fig,
                             "plot_deter_subarea_by_warnings_state.png"),
        height = height, width = width, units = units
    )


    #---- Boxplot days between warnings by subarea ----

    plot_days_first_to_last <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE,
                      VIEW_DATE < max_date) %>%
        get_plot_days_first_to_last(area_breaks)
    ggplot2::ggsave(
        plot =  plot_days_first_to_last,
        filename = file.path(out_fig,
                             "plot_deter_days_first_to_last.png"),
        height = height, width = width, units = units
    )


    #---- Trajectories of subareas (DETER only) ----

    plot_tb <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE,
                      subarea_ha > min_subarea_ha | is.na(subarea_ha),
                      !is.na(CLASSNAME),
                      VIEW_DATE < max_date) %>%
        dplyr::arrange(xy_id, VIEW_DATE) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(n_warnings = dplyr::n(),
                      warning_pos = dplyr::row_number(),
                      CLASSNAME = stringr::str_replace_all(CLASSNAME,
                                                           pattern = "_",
                                                           replacement = " "),
                      CLASSNAME = stringr::str_to_sentence(CLASSNAME)) %>%
        tidyr::fill(subarea_ha, .direction = "downup") %>%
        dplyr::ungroup() %>%
        dplyr::select(subarea_ha, CLASSNAME, xy_id,
                      n_warnings, warning_pos) %>%
        data.table::as.data.table()

    # Create latex tables.
    table_ls <-
        plot_tb %>%
        dplyr::filter(n_warnings > 1) %>%
        dplyr::group_by(n_warnings) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(get_trajectory_stats)

    for (i in seq_along(table_ls)) {
        table_ls %>%
            magrittr::extract2(i) %>%
            dplyr::select(-min_area, -max_area, -mean_area,
                          -median_area, -sd_area ) %>%
            dplyr::arrange(dplyr::desc(area_ha)) %>%
            dplyr::slice(1:10) %>%
            janitor::adorn_totals() %>%
            kableExtra::kbl(format = "latex",
                            booktabs = TRUE,
                            longtable = TRUE,
                            linesep = "",
                            digits = 1,
                            label = paste0("tab:traj_deter_", i + 1),
                            caption = NA) %>%
            kableExtra::kable_styling(full_width = TRUE,
                                      font_size = 7,
                                      latex_options = "striped") %>%
            kableExtra::collapse_rows(columns = 1:i,
                                      latex_hline = "major",
                                      valign = "middle") %>%
            readr::write_file(file = file.path(out_tab,
                                         paste0("tb_deter_subarea_trajectory_",
                                                      i + 1, ".tex")),
                              append = FALSE)
    }

    # Create Sankey figures.
    # NOTE: I couldn't make ggsankey::geom_sankey use the variable subarea_ha.
    plot_ls <-
        plot_tb %>%
        dplyr::filter(n_warnings > 1) %>%
        dplyr::group_by(n_warnings) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(get_plot_sankey)
    for (i in seq_along(plot_ls)) {
        ggplot2::ggsave(
            plot = plot_ls[[i]],
            filename = file.path(
                out_fig,
                paste0("plot_deter_subarea_trajectory_", i + 1, ".png")
            ),
            height = height, width = width, units = units
        )
    }

    rm(plot_ls)
    rm(table_ls)
    rm(plot_tb)


    #---- Trajectories of subareas (DETER & PRODES) ----

    # NOTE: Use only DETER subareas which have a PRODES match.

    subareas_prodes_xyids <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "PRODES") %>%
        dplyr::pull(xy_id) %>%
        unique() %>%
        sort()

    plot_tb <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(subarea_ha > min_subarea_ha  | is.na(subarea_ha),
                      !is.na(CLASSNAME),
                      VIEW_DATE < max_date,
                      xy_id %in% subareas_prodes_xyids) %>%
        dplyr::arrange(xy_id, VIEW_DATE) %>%
        dplyr::select(subarea_ha, CLASSNAME, xy_id,
                      n_warnings, warning_pos) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(n_warnings = dplyr::n(),
                      warning_pos = dplyr::row_number()) %>%
        # Fill in the areas of PRODES using DETER subareas.
        tidyr::fill(subarea_ha, .direction = "downup") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(CLASSNAME = dplyr::case_match(CLASSNAME,
            c("d2007", "d2008", "d2009", "d2010", "d2011", "d2012",
              "d2013", "d2014", "d2015", "d2016", "d2017", "d2018", "d2019",
              "d2020", "d2021") ~ "P_desmatamento",
            c("r2010", "r2011", "r2012", "r2013", "r2014", "r2015", "r2016",
              "r2017", "r2018", "r2019", "r2020", "r2021") ~ "P_residual",
            "FOREST_2021" ~ "P_forest_2021",
            "HIDROGRAFIA" ~ "P_hidrografia",
            c("NAO_FLORESTA", "NAO_FLORESTA2") ~  "P_nao_floresta",
            "NUVEM_2021" ~ "P_nuvem_2021",
            .default = CLASSNAME),
                      CLASSNAME = stringr::str_replace_all(CLASSNAME,
                                                           pattern = "_",
                                                           replacement = " "),
                      CLASSNAME = stringr::str_to_sentence(CLASSNAME)) %>%
        data.table::as.data.table()

    # Create latex tables.
    table_ls <-
        plot_tb %>%
        dplyr::filter(n_warnings > 1) %>%
        dplyr::group_by(n_warnings) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(get_trajectory_stats)
    for (i in seq_along(table_ls)) {
        table_ls %>%
            magrittr::extract2(i) %>%
            dplyr::select(-min_area, -max_area, -mean_area,
                          -median_area, -sd_area ) %>%
            dplyr::arrange(dplyr::desc(area_ha)) %>%
            dplyr::slice(1:10) %>%
            janitor::adorn_totals() %>%
            kableExtra::kbl(format = "latex",
                            booktabs = TRUE,
                            longtable = TRUE,
                            linesep = "",
                            digits = 1,
                            label = paste0("tab:traj_deter_prodes", i + 1),
                            caption = NA) %>%
            kableExtra::kable_styling(full_width = TRUE,
                                      font_size = 7,
                                      latex_options = "striped") %>%
            kableExtra::collapse_rows(columns = 1:i,
                                      latex_hline = "major",
                                      valign = "middle") %>%
            readr::write_file(file = file.path(out_tab,
                                  paste0("tb_deter_prodes_subarea_trajectory_",
                                                      i + 1, ".tex")),
                              append = FALSE)
    }

    # Create Sankey figures.
    # NOTE: I couldn't make ggsankey::geom_sankey use the variable subarea_ha.
    plot_ls <-
        plot_tb %>%
        dplyr::filter(n_warnings > 1) %>%
        dplyr::group_by(n_warnings) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(get_plot_sankey)
    for (i in seq_along(plot_ls)) {
        ggplot2::ggsave(
            plot = plot_ls[[i]],
            filename = file.path(
                out_fig,
                paste0("plot_deter_prodes_subarea_trajectory_", i + 1, ".png")
            ),
            height = height, width = width, units = units
        )
    }
    rm(plot_ls)
    rm(plot_tb)
    rm(subareas_prodes_xyids)

    invisible(out_dir)
}


