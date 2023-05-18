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

    # Number of top rows (for the tables).
    top_rows <- 5

    # Directories for writing figures and tables.
    out_fig <- file.path(out_dir, "figures")
    out_tab <- file.path(out_dir, "tables")
    stopifnot("Directory for storing figures not found!" = dir.exists(out_fig))
    stopifnot("Directory for storing tables not found!" = dir.exists(out_tab))

    # NOTE: There shouldn't be DETER warnings before 2016-08-01, but there
    #       should be PRODES polygons before this date!
    stopifnot("DETER warnigs should start at 2016-08-01" =
        sum(treesburnareas::subarea_dt[data_source == "DETER",
        VIEW_DATE < as.Date("2016-08-01")]) == 0)


    #---- Preprocessing ----

    xyid_in_prodes <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(in_prodes == TRUE) %>%
        dplyr::pull(xy_id) %>%
        unique() %>%
        sort()

    # NOTE: Subareas (DETER & PRODES) that fall inside the PRODES mask and
    #       happened before max_date.
    subareas_deter_prodes <-
        treesburnareas::subarea_dt %>%
        # Fill in the area of the PRODES subareas.
        dplyr::arrange(xy_id, VIEW_DATE) %>%
        dplyr::group_by(xy_id) %>%
        tidyr::fill(subarea_ha, .direction = "downup") %>%
        dplyr::ungroup() %>%
        dplyr::filter(xy_id %in% xyid_in_prodes,
                      VIEW_DATE < max_date,
                      subarea_ha > min_subarea_ha)

    rm(xyid_in_prodes)



    #---- Treemap: DETER warning area by state, year, warning type and area----

    #NOTE: This figure isn't about subareas, is about DETER only!
    treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE,
                      VIEW_DATE < max_date) %>%
        # Save the figure.
        (function(x) {
            ggplot2::ggsave(plot = get_plot_area_by_state_year_type(x),
                    filename = file.path(out_fig,
                                    "plot_deter_area_by_state_pyear_type.png"),
                    height = height, width = width, units = units)
            return(x)
        }) %>%
        # Save the table.
        dplyr::group_by(CLASSNAME, UF, year) %>%
        dplyr::summarize(area_km2 = sum(subarea_ha) / 100) %>%
        dplyr::ungroup() %>%
        dplyr::select(UF, year, CLASSNAME, area_km2) %>%
        dplyr::arrange(UF, year, dplyr::desc(area_km2)) %>%
        dplyr::mutate(year = as.character(year)) %>%
        janitor::adorn_totals() %>%
        kableExtra::kbl(format = "latex",
                        booktabs = TRUE,
                        longtable = TRUE,
                        linesep = "",
                        digits = 1,
                        label = "tab:deter_area_by_state_pyear_type",
                        caption = NA) %>%
        kableExtra::kable_styling(full_width = TRUE,
                                  font_size = 7,
                                  latex_options = "striped") %>%
        kableExtra::collapse_rows(columns = 1:2,
                                  latex_hline = "major",
                                  valign = "middle") %>%
        readr::write_file(file = file.path(out_tab,
                                     "tb_deter_area_by_state_pyear_type.tex"),
                          append = FALSE)



    #---- Point density: DETER subareas by state, year, warning type and area

    # NOTE: This plot shows uses the CLASSNAME of the first DETER warning.
    subareas_deter_prodes %>%
        dplyr::filter(data_source == "DETER") %>%
        get_plot_density_area_ndays() %>%
        ggplot2::ggsave(filename = file.path(out_fig,
              "plot_deter_subarea_density_by_state_first-type_nwarnings.png"),
                        height = height, width = width, units = units)



    #---- Histogram DETER subareas by number of warnings ----

    subareas_deter_prodes %>%
        dplyr::filter(data_source == "DETER") %>%
        get_plot_area_by_warnings(area_breaks) %>%
        ggplot2::ggsave(filename = file.path(out_fig,
                                        "plot_deter_subarea_by_nwarnings.png"),
                        height = height, width = width, units = units)



    #---- Histogram DETER area by class ----

    subareas_deter_prodes %>%
        dplyr::filter(data_source == "DETER") %>%
        get_plot_area_by_class() %>%
        ggplot2::ggsave(filename = file.path(out_fig,
                                             "plot_deter_area_by_class.png"),
                        height = height, width = width, units = units)



    #---- Histogram DETER area by class & state ----

    subareas_deter_prodes %>%
        dplyr::filter(data_source == "DETER") %>%
        get_plot_area_by_class_state() %>%
        ggplot2::ggsave(filename = file.path(out_fig,
                                         "plot_deter_area_by_class_state.png"),
                        height = height, width = width, units = units)



    #---- Distribution of area of warning areas ----
    # TODO: This figure requires the ID of the original DETER warning.



    #---- Histogram DETER subareas by number of warnings by state ----

    subareas_deter_prodes %>%
        dplyr::filter(data_source == "DETER") %>%
        get_plot_area_by_warnings_state(area_breaks) %>%
        ggplot2::ggsave(filename = file.path(out_fig,
                                   "plot_deter_subarea_by_warnings_state.png"),
                        height = height, width = width, units = units)



    #---- Boxplot days between warnings by subarea ----

    subareas_deter_prodes %>%
        dplyr::filter(data_source == "DETER") %>%
        get_plot_days_first_to_last(area_breaks) %>%
        ggplot2::ggsave(filename = file.path(out_fig,
                                          "plot_deter_days_first_to_last.png"),
                        height = height, width = width, units = units)



    #---- Trajectories of subareas (DETER only) ----

    plot_tb <-
        subareas_deter_prodes %>%
        dplyr::filter(data_source == "DETER") %>%
        dplyr::arrange(xy_id, VIEW_DATE) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(n_warnings = dplyr::n(),
                      warning_pos = dplyr::row_number(),
                      CLASSNAME = stringr::str_replace_all(CLASSNAME,
                                                           pattern = "_",
                                                           replacement = " "),
                      CLASSNAME = stringr::str_to_sentence(CLASSNAME)) %>%
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
            dplyr::slice_max(order_by = area_ha, n = top_rows) %>%
            janitor::adorn_totals() %>%
            kableExtra::kbl(format = "latex", booktabs = TRUE,
                            longtable = TRUE, linesep = "", digits = 1,
                            label = paste0("tab:traj_deter_", i + 1),
                            caption = NA) %>%
            kableExtra::kable_styling(full_width = TRUE, font_size = 7,
                                      latex_options = "striped") %>%
            kableExtra::collapse_rows(columns = 1:i, latex_hline = "major",
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
            filename = file.path(out_fig,
                      paste0("plot_deter_subarea_trajectory_", i + 1, ".png")),
                        height = height, width = width, units = units)
    }

    rm(plot_ls)
    rm(table_ls)
    rm(plot_tb)
    rm(i)



    #---- Trajectories of subareas (DETER & PRODES) ----

    # NOTE: Use only trajectories that include at least one PRODES event.
    subareas_prodes_xyids <-
        subareas_deter_prodes %>%
        dplyr::filter(data_source == "PRODES") %>%
        dplyr::pull(xy_id) %>%
        unique() %>%
        sort()

    prodes_common_names <-
        get_prodes_names() %>%
        (function(x) {
            y <- paste0("P ", x)
            names(y) <- names(x)
            return(y)
        })

    plot_tb <-
        subareas_deter_prodes %>%
        dplyr::filter(xy_id %in% subareas_prodes_xyids) %>%
        dplyr::arrange(xy_id, VIEW_DATE) %>%
        dplyr::select(subarea_ha, CLASSNAME, xy_id, VIEW_DATE, data_source) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(n_warnings = dplyr::n(),
                      warning_pos = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(CLASSNAME = dplyr::recode(CLASSNAME,
                                                !!!prodes_common_names),
                      CLASSNAME = stringr::str_replace_all(CLASSNAME,
                                                           pattern = "_",
                                                           replacement = " "),
                      CLASSNAME = stringr::str_to_sentence(CLASSNAME)) %>%
        data.table::as.data.table()

    # Compute the proximity (in time) of PRODES and DETER classes.
    plot_tb %>%
        dplyr::arrange(xy_id, VIEW_DATE) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(prev_class = dplyr::lag(CLASSNAME),
                      next_class = dplyr::lead(CLASSNAME),
                      prev_date = dplyr::lag(VIEW_DATE),
                      next_date = dplyr::lead(VIEW_DATE)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n_warnings > 1, data_source == "PRODES") %>%
        dplyr::mutate(
            prev_date = tidyr::replace_na(prev_date, as.Date(-Inf)),
            next_date = tidyr::replace_na(next_date, as.Date(Inf)),
            closest_class = dplyr::if_else(
                abs(VIEW_DATE - prev_date) <= abs(VIEW_DATE - next_date),
                prev_class,
                next_class),
            closest_date = dplyr::if_else(
                abs(VIEW_DATE - prev_date) <= abs(VIEW_DATE - next_date),
                prev_date,
                next_date)
            ) %>%
        dplyr::select(-n_warnings, -data_source, -prev_class,
                      -next_class, -prev_date, -next_date) %>%
        dplyr::mutate(diff_days = difftime(VIEW_DATE, closest_date,
                                           units = "days")) %>%
        dplyr::group_by(CLASSNAME, closest_class) %>%
        dplyr::summarize(total_ha = sum(subarea_ha),
                         n = dplyr::n(),
                         median_days = median(as.double(diff_days)),
                         median_days_abs = median(abs(as.double(diff_days))),
                         sd_days = sd(as.double(diff_days)),
                         sd_abs = sd(abs(as.double(diff_days)))) %>%
        dplyr::ungroup() %>%
        dplyr::slice_max(order_by = total_ha, n = top_rows) %>%
        kableExtra::kbl(format = "latex",
                        booktabs = TRUE,
                        longtable = TRUE,
                        linesep = "",
                        digits = 1,
                        label = "tab:prodes_deter_time_proximity",
                        caption = NA) %>%
        kableExtra::kable_styling(full_width = TRUE,
                                  font_size = 7,
                                  latex_options = "striped") %>%
        readr::write_file(
            file = file.path(out_tab, "tb_prodes_deter_time_proximity.tex"),
            append = FALSE
        )

    # Create trajectory top-5 tables (latex).
    table_ls <-
        plot_tb %>%
        dplyr::filter(n_warnings > 1) %>%
        dplyr::select(-VIEW_DATE, -data_source) %>%
        dplyr::group_by(n_warnings) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(get_trajectory_stats)
    for (i in seq_along(table_ls)) {
        table_ls %>%
            magrittr::extract2(i) %>%
            dplyr::select(-min_area, -max_area, -mean_area,
                          -median_area, -sd_area) %>%
            dplyr::slice_max(order_by = area_ha, n = top_rows) %>%
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
        dplyr::select(-VIEW_DATE, data_source) %>%
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
    rm(i)



    #---- Analysis 1: Trajectories with DETER and PRODES ----

    # NOTE: There are 70/517059 with more than one DETER-PRODES event in the
    #       same PRODES year.
    subareas_deter_prodes %>%
        dplyr::arrange(xy_id, VIEW_DATE) %>%
        dplyr::group_by(xy_id, year) %>%
        dplyr::summarize(n = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n > 1) %>%
        nrow()

    # Allow only one event by PRODES year.
    subareas_deter_prodes <-
        subareas_deter_prodes %>%
        dplyr::arrange(xy_id, VIEW_DATE) %>%
        dplyr::group_by(xy_id, year) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()

    # Exclude trajectories involving mining.
    xyid_mining <-
        subareas_deter_prodes %>%
        dplyr::filter(CLASSNAME == "MINERACAO") %>%
        dplyr::pull(xy_id) %>%
        unique()
    subareas_deter_prodes <-
        subareas_deter_prodes %>%
        dplyr::filter(!(xy_id %in% xyid_mining))
    rm(xyid_mining)

    # End trajectories as soon as they reach deforestation
    deforestation_classes <- c("P Deforestation", "P Residual",
                               "DESMATAMENTO_CR", "DESMATAMENTO_VEG")
    subareas_analysis_1 <-
        subareas_deter_prodes %>%
        dplyr::mutate(common_name = dplyr::recode(CLASSNAME,
                                                  !!!prodes_common_names)) %>%
        trim_trajectories(class_column = common_name,
                          end_class = deforestation_classes) %>%
        dplyr::select(-common_name)

    # NOTE: Use only trajectories that include at least one PRODES event.
    subareas_prodes_xyids <-
        subareas_analysis_1 %>%
        dplyr::filter(data_source == "PRODES") %>%
        dplyr::pull(xy_id) %>%
        unique() %>%
        sort()

    prodes_common_names <-
        get_prodes_names() %>%
        (function(x) {
            y <- paste0("P ", x)
            names(y) <- names(x)
            return(y)
        })

    subareas_analysis_1 <-
        subareas_analysis_1 %>%
        dplyr::filter(xy_id %in% subareas_prodes_xyids)

    plot_tb <-
        subareas_analysis_1 %>%
        dplyr::arrange(xy_id, VIEW_DATE) %>%
        dplyr::select(subarea_ha, CLASSNAME, xy_id, VIEW_DATE, data_source) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(n_warnings = dplyr::n(),
                      warning_pos = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(CLASSNAME = dplyr::recode(CLASSNAME,
                                                !!!prodes_common_names),
                      CLASSNAME = stringr::str_replace_all(CLASSNAME,
                                                           pattern = "_",
                                                           replacement = " "),
                      CLASSNAME = stringr::str_to_sentence(CLASSNAME)) %>%
        data.table::as.data.table()

    # Create Sankey figures.
    # NOTE: I couldn't make ggsankey::geom_sankey use the variable subarea_ha.
    plot_ls <-
        plot_tb %>%
        dplyr::filter(n_warnings > 1) %>%
        dplyr::select(-VIEW_DATE, data_source) %>%
        dplyr::group_by(n_warnings) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(get_plot_sankey)
    for (i in seq_along(plot_ls)) {
        ggplot2::ggsave(
            plot = plot_ls[[i]],
            filename = file.path(
                out_fig,
                paste0("an1_plot_deter_prodes_subarea_trajectory_", i + 1, ".png")
            ),
            height = height, width = width, units = units
        )
    }
    rm(plot_ls)
    rm(plot_tb)
    rm(subareas_prodes_xyids)
    rm(i)


    #---- Analysis 2 ----

    xyid_burn_scar <-
        subareas_analysis_1 %>%
        dplyr::filter(CLASSNAME == "CICATRIZ_DE_QUEIMADA") %>%
        dplyr::pull(xy_id) %>%
        unique()
    subareas_analysis_2 <-
        subareas_analysis_1 %>%
        dplyr::filter(xy_id %in% xyid_burn_scar)

    plot_tb <-
        subareas_analysis_2 %>%
        dplyr::arrange(xy_id, VIEW_DATE) %>%
        dplyr::select(subarea_ha, CLASSNAME, xy_id, VIEW_DATE, data_source) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(n_warnings = dplyr::n(),
                      warning_pos = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(CLASSNAME = dplyr::recode(CLASSNAME,
                                                !!!prodes_common_names),
                      CLASSNAME = stringr::str_replace_all(CLASSNAME,
                                                           pattern = "_",
                                                           replacement = " "),
                      CLASSNAME = stringr::str_to_sentence(CLASSNAME)) %>%
        data.table::as.data.table()

    # Create Sankey figures.
    # NOTE: I couldn't make ggsankey::geom_sankey use the variable subarea_ha.
    plot_ls <-
        plot_tb %>%
        dplyr::filter(n_warnings > 1) %>%
        dplyr::select(-VIEW_DATE, data_source) %>%
        dplyr::group_by(n_warnings) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(get_plot_sankey)
    for (i in seq_along(plot_ls)) {
        ggplot2::ggsave(
            plot = plot_ls[[i]],
            filename = file.path(
                out_fig,
                paste0("an2_plot_deter_prodes_subarea_trajectory_", i + 1, ".png")
            ),
            height = height, width = width, units = units
        )
    }
    rm(plot_ls)
    rm(plot_tb)
    rm(i)




    #---- End ----
    invisible(out_dir)

}


