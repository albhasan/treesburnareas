#' @title Create EDA plots and tables
#'
#' @name create_plots
#'
#' @description
#' This function creates the figures and tables of the Exploratory Data
#' Analysis of the Amazonia deforestation warnings issued by DETER.
#' @param out_dir A path to a directory.
#' @param subarea_tb A tibble with data about DETER subareas.
#' @param subarea_sf A sf object with the spatial data associated to 
#' subarea_tb.
#' @param fire_sf A sf object (point). Fire spots.
#' @param area_col Name of the area column.
#' @param date_col Name of the column with the date of the warning.
#' @param inprodes_col Name of the column indicating if the warning is present
#'                     in PRODES.
#' @param label_col Name of the label column.
#' @param month_col Name of the column with the month of the warning.
#' @param ntraj_col Name of the column with the number of trajectories.
#' @param nwarn_col Name of the column with the number of warnings.
#' @param nwarn_pos Name of the column with the position of the warning in the
#'                  trajectory.
#' @param said_col Name of the column with the SubArea ID.
#' @param source_col Name of the column with the origin of the warning (e.g. 
#'                   DETER or PRODES).
#' @param state_col Name of the column with the name of the state.
#' @param year_col Name of the column with the PRODES year.
#' @param area_breaks Named vector with break values of for the area column.
#' @param top_rows Nuber of rows to export as LateX tables. 
#' @param height Size of the output figures.
#' @param width Size of the output figures.
#' @param units Units of height and width.
#' @param max_date Only include warnings before this date.
#' @param min_subarea_ha Mininum area of a subarea.
#' @return out_dir (invisible).
#'
#' @export
#'
create_plots <- function(out_dir, subarea_tb, subarea_sf, fire_sf, 
                         area_col = "subarea_ha", date_col = "VIEW_DATE", 
                         inprodes_col = "in_prodes", label_col = "CLASSNAME", 
                         month_col = "month", ntraj_col = "n_traj", 
                         nwarn_col = "n_warnings", nwarn_pos = "warning_pos", 
                         said_col = "xy_id", source_col = "data_source", 
                         state_col = "UF", year_col = "year", 
                         area_breaks = c("0" = 0, "6,25 ha"= 6.25, 
                         "10 ha" = 10, "25 ha" = 25, "50 ha" = 50, 
                         "100 ha" = 100, "250 ha" = 250, "500 ha" = 500, 
                         "1000 ha" = 1000, "> 1000 ha" = Inf), top_rows = 5, 
                         height = 210, width = 297, units = "mm", 
                         max_date = as.Date("2021-08-01"), min_subarea_ha = 3) {

    # Temporal statistics columns
    max_area <- mean_area <- median_area <- min_area <- sd_area <- NULL


    # Columns for Sankey figures.
    closest_class <- closest_date <- next_class <- next_date <- prev_class <-
    prev_date <- NULL

    # Columns added to brazilian states downloaded from internet
    area_bla_km2 <- name_state <- NULL

    # Internal columns
    common_name <- diff_days <- filename <- filepath  <- fire_spots <- 
    fspot_km2 <- n <- total_ha <- NULL

    # TODO:
    # - Check why the named changed from subarea_ha to area_ha
    area_ha <- NULL
    # - estado remain because I don't know how to use a varible in a join.
    estado <- NULL
    # - ggplot2 variable
    year <- NULL




    #---- Setup ----

    stopifnot("Directory not found!" = dir.exists(out_dir))

    # Directories for writing figures and tables.
    out_fig <- file.path(out_dir, "figures")
    out_tab <- file.path(out_dir, "tables")

    if (!dir.exists(out_fig))
        dir.create(out_fig)
    if (!dir.exists(out_tab))
        dir.create(out_tab)

    #---- Validation ----

    # NOTE: There shouldn't be DETER warnings before 2016-08-01, but there
    #       should be PRODES polygons before this date!
    stopifnot("DETER warnings are outside time interval" =
        subarea_tb %>%
        dplyr::filter(.data[[source_col]] == "DETER",
                      .data[[date_col]] < as.Date("2016-08-01")) %>%
        nrow() == 0)


    #---- Utilitary ----
    # Add percentages column and totals row to tibble.
    # @param data_tb  A tibble.
    # @param top_rows An integer. Number of top rows to keep.
    # @return         A tibble.
    .adorn_table <- function(data_tb, top_rows) {
        data_tb %>%
            dplyr::arrange(dplyr::desc(.data[[area_col]])) %>%
            dplyr::mutate(
              p_area = 100 * .data[[area_col]] / sum(.data[[area_col]]),
              p_traj = 100 * .data[[ntraj_col]] / sum(.data[[ntraj_col]])) %>%
            janitor::adorn_totals(where = "row", fill = "-",
                                  c(tidyselect::all_of(c(area_col,
                                                         ntraj_col)))) %>%
            (function(x) {
                dplyr::bind_rows(
                    dplyr::slice_head(x, n = top_rows),
                    dplyr::slice_tail(x, n = 1)
                )
            }) %>%
            return()
    }

    # Get the subarea ids of DETER subareas falling inside PRODES mask.
    # @param data_tb A tibble.
    # @return        A character.
    .get_xyid_in_prodes <- function(data_tb) {
        data_tb %>%
            dplyr::filter(.data[[inprodes_col]] == TRUE) %>%
            dplyr::pull(tidyselect::all_of(said_col)) %>%
            unique() %>%
            sort() %>%
            return()
    }

    # Format latex tables. Sets defaults for exporting tables to LateX.
    # @param data_tb A tibble.
    # @param label Latex's label.
    # @param columns Integer. Columns in which rows should be collapsed.
    .table_helper <- function(data_tb, label, columns, format = "latex", 
                              booktabs = TRUE, longtable = TRUE, linesep = "", 
                              digits = 1, caption = NA, full_width = TRUE, 
                              font_size = 7, latex_options = "striped", 
                              latex_hline = "major", valign = "middle") {
        data_tb %>%
            kableExtra::kbl(format = format, booktabs = booktabs, 
                longtable = longtable, linesep = linesep, digits = digits, 
                caption = caption, label = label) %>%
            kableExtra::kable_styling(full_width = full_width, 
                font_size = font_size, latex_options = latex_options) %>%
            kableExtra::collapse_rows(columns = columns, 
                latex_hline = latex_hline, valign = valign) %>%
            return()
    }


    #---- Treemap: DETER warning area by state, year, warning type and area----

    #NOTE: This figure isn't about subareas, is about DETER only!
    subarea_tb %>%
        dplyr::filter(.data[[source_col]] == "DETER",
                      .data[[inprodes_col]] == TRUE,
                      .data[[date_col]] < max_date) %>%
        # Save the figure.
        (function(x) {
            ggplot2::ggsave(plot = get_plot_area_by_state_year_type(
                x,
                # NOTE: In this figure we need this specific order of classes.
                #       The classes provided by get_deter_classes()$class don't
                #       match the required order.
                class_levels = c("MINERACAO", "CICATRIZ_DE_QUEIMADA",
                                 "DESMATAMENTO_CR", "CS_GEOMETRICO",
                                 "DESMATAMENTO_VEG", "CS_DESORDENADO",
                                 "CORTE_SELETIVO", "DEGRADACAO")), 
                filename = file.path(out_fig, 
                    "plot_deter_area_by_state_pyear_type.png"), 
                height = height, width = width, units = units)
            return(x)
        }) %>%
        # Save the table.
        dplyr::summarize("{label_col}" := dplyr::first(.data[[label_col]]),
                         "{state_col}" := dplyr::first(.data[[state_col]]),
                         "{year_col}"  := dplyr::first(.data[[year_col]]),
                         "area_km2" = sum(.data[[area_col]]) / 100,
                         .by = tidyselect::all_of(c(state_col, year_col,
                                                    label_col))) %>%
        dplyr::arrange(.data[[state_col, year_col,
                       dplyr::desc("area_km2")]]) %>%
        dplyr::mutate("{year_col}" := as.character(.data[[year_col]])) %>%
        janitor::adorn_totals() %>%
        .table_helper(label = "tab:deter_area_by_state_pyear_type", 
                      columns = 1:2) %>%
        readr::write_file(append = FALSE, 
            file = file.path(out_tab, "tb_deter_area_by_state_pyear_type.tex"))


    #---- Preprocessing ----

    # NOTE: Subareas (DETER & PRODES) that fall inside the PRODES mask,
    #       happened before max_date, and have a minimum size.
    subareas_deter_prodes <-
        subarea_tb %>%
        dplyr::arrange(.data[[said_col]], .data[[date_col]]) %>%
        # Fill in the missing areas of PRODES.
        dplyr::group_by(.data[[said_col]]) %>%
        tidyr::fill(tidyselect::all_of(area_col), .direction = "downup") %>%
        dplyr::ungroup() %>%
        # NOTE: Keep all of PRODES data by not using a min_date.
        dplyr::filter(
            .data[[said_col]] %in% .get_xyid_in_prodes(subarea_tb),
            .data[[date_col]] < max_date,
            .data[[area_col]] > min_subarea_ha
        )

    

    #---- Histogram DETER area by class ----

    subareas_deter_prodes %>%
        dplyr::filter(.data[[source_col]] == "DETER") %>%
        get_plot_area_by_class() %>%
        ggplot2::ggsave(filename = file.path(out_fig,
                                             "plot_deter_area_by_class.png"),
                        height = height, width = width, units = units)



    #---- Histogram DETER area by class & state ----

    subareas_deter_prodes %>%
        dplyr::filter(.data[[source_col]] == "DETER") %>%
        get_plot_area_by_class_state() %>%
        ggplot2::ggsave(filename = file.path(out_fig,
                                         "plot_deter_area_by_class_state.png"),
                        height = height, width = width, units = units)


    #---- Point density: DETER subareas by state, year, warning type and area

    # NOTE: This plot shows uses the CLASSNAME of the first DETER warning.
    subareas_deter_prodes %>%
        dplyr::filter(.data[[source_col]]== "DETER") %>%
        get_plot_density_area_ndays() %>%
        ggplot2::ggsave(filename = file.path(out_fig,
              "plot_deter_subarea_density_by_state_first-type_nwarnings.png"),
                        height = height, width = width, units = units)



    #---- Histogram DETER subareas by number of warnings ----

    subareas_deter_prodes %>%
        dplyr::filter(.data[[source_col]] == "DETER") %>%
        get_plot_area_by_warnings(area_breaks) %>%
        ggplot2::ggsave(filename = file.path(out_fig,
                                        "plot_deter_subarea_by_nwarnings.png"),
                        height = height, width = width, units = units)






    #---- Distribution of area of warning areas ----

    # TODO: This figure requires the ID of the original DETER warning.
        # check data scripts for deter ids.



    #---- Histogram DETER subareas by number of warnings by state ----

    subareas_deter_prodes %>%
        dplyr::filter(.data[[source_col]] == "DETER") %>%
        get_plot_area_by_warnings_state(area_breaks) %>%
        ggplot2::ggsave(filename = file.path(out_fig,
                                   "plot_deter_subarea_by_warnings_state.png"),
                        height = height, width = width, units = units)



    #---- Boxplot days between warnings by subarea ----

    subareas_deter_prodes %>%
        dplyr::filter(.data[[source_col]] == "DETER") %>%
        get_plot_days_first_to_last(area_breaks) %>%
        ggplot2::ggsave(filename = file.path(out_fig,
                                          "plot_deter_days_first_to_last.png"),
                        height = height, width = width, units = units)



    #---- Trajectories of subareas (DETER only) ----

    plot_tb <-
        subareas_deter_prodes %>%
        dplyr::filter(.data[[source_col]] == "DETER") %>%
        dplyr::arrange(.data[[said_col]], .data[[date_col]]) %>%
        dplyr::arrange(.data[[said_col]], .data[[date_col]]) %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::mutate(
            "{nwarn_col}" := dplyr::n(),
            "{nwarn_pos}" := dplyr::row_number(),
            {{label_col}} := stringr::str_replace_all(.data[[label_col]],
                                                      pattern = "_", 
                                                      replacement = " "),
            {{label_col}} := stringr::str_to_sentence(.data[[label_col]])
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(tidyselect::all_of(c(area_col, label_col, said_col,
                                         nwarn_col, nwarn_pos, year_col)))

    # Create Sankey figure (all years).
    # oplan <- future::plan(multicore, workers = 2)
    # on.exit(plan(oplan), add = TRUE)
    sankey_year <-
        plot_tb %>%
        get_plot_sankey_year()
    ggplot2::ggsave(plot = sankey_year,
                    filename = file.path(out_fig,
                             paste0("plot_deter_subarea_trajectory_year.png")),
                    height = height, width = width, units = units)
    rm(sankey_year)


    # Create latex tables (by number of warnings).
    table_ls <-
        plot_tb %>%
        dplyr::select(-tidyselect::any_of(year_col)) %>%
        dplyr::filter(.data[[nwarn_col]]> 1) %>%
        dplyr::group_by(.data[[nwarn_col]]) %>%
        dplyr::group_split(.keep = TRUE) %>%
        furrr::future_map(get_trajectory_stats)

    for (i in seq_along(table_ls)) {
# TODO: This throws an erro on iteration 1!
print(paste(".adorn_table 326:", i))
        table_ls %>%
            magrittr::extract2(i) %>%
            dplyr::select(-min_area, -max_area, -mean_area,
                          -median_area, -sd_area) %>%
            # TODO: We renamed area column. Check get_trajectory_stats.
            dplyr::select(subarea_ha = area_ha) %>%
            .adorn_table(top_rows = top_rows) %>%
            .table_helper(label = paste0("tab:traj_deter_", i + 1), 
                          columns = 1:i) %>%
            readr::write_file(file = file.path(out_tab,
                        paste0("tb_deter_subarea_trajectory_", i + 1, ".tex")),
                              append = FALSE)
    }

    # Create Sankey figure (by number of warnings).
    plot_ls <-
        plot_tb %>%
        dplyr::filter(.data[[nwarn_col]]> 1) %>%
        dplyr::group_by(.data[[nwarn_col]]) %>%
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

    prodes_common_names <-
        get_prodes_names() %>%
        (function(x) {
            y <- paste0("P ", x)
            names(y) <- names(x)
            return(y)
        })

    plot_tb <-
        subareas_deter_prodes %>%
        # NOTE: Use only trajectories that include at least one PRODES event.
        filter_trajectory(filter_col = source_col, filter_val = "PRODES") %>%
        dplyr::arrange(.data[[said_col]], .data[[date_col]]) %>%
        dplyr::select(tidyselect::all_of(area_col, label_col, said_col, 
                                         date_col, source_col)) %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::mutate("{{nwarn_col}}" := dplyr::n(),
                      "{{nwarn_pos}}" := dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(CLASSNAME = dplyr::recode(.data[[label_col]],
                                                !!!prodes_common_names),
                      CLASSNAME = stringr::str_replace_all(.data[[label_col]],
                                                           pattern = "_",
                                                           replacement = " "),
                      CLASSNAME = stringr::str_to_sentence(.data[[label_col]]))

    # Compute the proximity (in time) of PRODES and DETER classes.
    plot_tb %>%
        dplyr::arrange(.data[[said_col]], .data[[date_col]]) %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::mutate(prev_class = dplyr::lag(.data[[label_col]]),
                      next_class = dplyr::lead(.data[[label_col]]),
                      prev_date = dplyr::lag(.data[[date_col]]),
                      next_date = dplyr::lead(.data[[date_col]])) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data[[nwarn_col]] > 1, 
                      .data[[source_col]] == "PRODES") %>%
        dplyr::mutate(
            prev_date = tidyr::replace_na(prev_date, as.Date(-Inf)),
            next_date = tidyr::replace_na(next_date, as.Date(Inf)),
# TODO: Is this right? Is the date the clossest class?
            closest_class = dplyr::if_else(
                abs(.data[[date_col]] - prev_date) <= 
                    abs(.data[[date_col]]- next_date),
                prev_class,
                next_class),
            closest_date = dplyr::if_else(
                abs(.data[[date_col]] - prev_date) <= 
                    abs(.data[[date_col]] - next_date),
                prev_date,
                next_date)
            ) %>%
        #######################################################################
        # TODO: Check if this is equivalent!
        #dplyr::select(-n_warnings, -data_source, -prev_class,
        #              -next_class, -prev_date, -next_date) %>%
        dplyr::select( -tidyselect::any_of(nwarn_col, source_col, "prev_class", 
                                "next_class", "prev_date", "next_date")) %>%
        #######################################################################
        dplyr::mutate(diff_days = difftime(.data[[date_col]], closest_date,
                                           units = "days")) %>%
        dplyr::group_by(.data[[label_col]], closest_class) %>%
        dplyr::summarize(total_ha = sum(.data[[area_col]]),
                         n = dplyr::n(),
                         median_days = stats::median(as.double(diff_days)),
                         median_days_abs =
                             stats::median(abs(as.double(diff_days))),
                         sd_days = stats::sd(as.double(diff_days)),
                         sd_abs = stats::sd(abs(as.double(diff_days)))) %>%
        dplyr::ungroup() %>%
        dplyr::slice_max(order_by = total_ha, n = top_rows) %>%
        .table_helper(label = "tab:prodes_deter_time_proximity", 
                      columns = NULL) %>%
        readr::write_file(append = FALSE,
            file = file.path(out_tab, "tb_prodes_deter_time_proximity.tex"))

    # Create trajectory top tables (latex).
    table_ls <-
        plot_tb %>%
        dplyr::filter(.data[[nwarn_col]] > 1) %>%
        dplyr::select(-tidyselect::any_of(date_col, source_col)) %>%
        dplyr::group_by(.data[[nwarn_col]]) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(get_trajectory_stats)
    for (i in seq_along(table_ls)) {
print(paste(".adorn_table 450 ", i))
        table_ls %>%
            magrittr::extract2(i) %>%
            dplyr::select(-min_area, -max_area, -mean_area,
                          -median_area, -sd_area,
            # NOTE: We renamed area column. Check get_trajectory_stats.
                          subarea_ha = area_ha) %>%
            .adorn_table(top_rows = top_rows) %>%
            .table_helper(label = paste0("tab:traj_deter_prodes", i + 1), 
                          columns = 1:i) %>%
            readr::write_file(file = file.path(out_tab,
                                  paste0("tb_deter_prodes_subarea_trajectory_",
                                                      i + 1, ".tex")),
                              append = FALSE)
    }

    # TODO: Create Sankey figure (all years).
    # NOTE: ggsankey doesn't work well with large amounts of data.

    # Create Sankey figure (by number of recurrent warnings).
    plot_ls <-
        plot_tb %>%
        dplyr::filter(.data[[nwarn_col]] > 1) %>%
        dplyr::select(-tidyselect::any_of(date_col)) %>%
# TODO: Is this right? Leave only one column?
        dplyr::select(tidyselect::all_of(source_col)) %>%
        dplyr::group_by(.data[[nwarn_col]]) %>%
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
    rm(i)


    #---- Analysis 1: Trajectories with DETER and PRODES ----

    # NOTE: There are subareas with more than one DETER-PRODES event in the
    #       same PRODES year (101587/530464).
    subareas_deter_prodes %>%
        dplyr::arrange(.data[[said_col]], .data[[date_col]]) %>%
        dplyr::group_by(.data[[said_col]], .data[[year_col]]) %>%
        dplyr::summarize(n = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n > 1) %>%
        nrow() %>%
        magrittr::set_names(paste("INFO: subareas with more than one event in",
                            "a PRODES year"))

    # Labels considered as deforestation are used to trim trajectories.
    deforestation_classes <- c("P Deforestation", "P Residual",
                               "DESMATAMENTO_CR", "DESMATAMENTO_VEG")

    # Prepare data
    subareas_analysis_1 <-
        subareas_deter_prodes %>%
        dplyr::arrange(.data[[said_col]], .data[[date_col]]) %>%
    # Exclude trajectories involving mining.
        filter_trajectory(filter_col = label_col,
                          filter_val = "MINERACAO",
                          invert = TRUE) %>%
    # Allow only one (first) event by PRODES year.
        dplyr::slice_head(n = 1,
                          by = tidyselect::all_of(c(said_col, year_col))) %>%
        (function(x) {
            stopifnot("Unknown end labels!" =
                      any(unique(x[[label_col]]) %in%
                          names(prodes_common_names)))
            return(x)
        }) %>%
        dplyr::mutate(common_name = dplyr::recode(.data[[label_col]],
                                                  !!!prodes_common_names)) %>%
    # End trajectories as soon as they reach deforestation
        trim_trajectories(end_class = deforestation_classes) %>%
        dplyr::select(-common_name) %>%
    # Use only trajectories that include at least one PRODES event.
        filter_trajectory(filter_col = source_col,
                          filter_val = "PRODES",
                          invert = FALSE)

    plot_tb <-
        subareas_analysis_1 %>%
        dplyr::arrange(.data[[said_col]], .data[[date_col]]) %>%
        dplyr::select(tidyselect::all_of(c(area_col, label_col, said_col,
                                         date_col, source_col))) %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::mutate("{{nwarn_col}}" := dplyr::n(),
                      "{{nwarn_pos}}" := dplyr::row_number()) %>%
        dplyr::ungroup() %>%
# TODO: make sure this works!
        dplyr::mutate(
            "{{label_col}}" := dplyr::recode(.data[[label_col]], 
                                             !!!prodes_common_names),
            "{{label_col}}" := stringr::str_replace_all(.data[[label_col]], 
                                                        pattern = "_", 
                                                        replacement = " "),
            "{{label_col}}" := stringr::str_to_sentence(.data[[label_col]]))

    plot_ls <-
        plot_tb %>%
        dplyr::filter(.data[[nwarn_col]] > 1) %>%
        #######################################################################
        # TODO: Check if this is equivalent!
        #dplyr::select(-VIEW_DATE, data_source) %>%
        dplyr::select(-tidyselect::any_of(date_col)) %>%
        dplyr::select(tidyselect::all_of(source_col)) %>%
        #######################################################################
        dplyr::group_by(.data[[nwarn_col]]) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(get_plot_sankey)

    for (i in seq_along(plot_ls)) {
        ggplot2::ggsave(
            plot = plot_ls[[i]],
            filename = file.path(
                out_fig,
                paste0("an1_plot_deter_prodes_subarea_trajectory_", i + 1,
                       ".png")
            ),
            height = height, width = width, units = units
        )
    }

    rm(plot_ls)
    #rm(plot_tb)
    rm(i)



    #---- Analysis 2 ----

    xyid_burn_scar <-
        subareas_analysis_1 %>%
        dplyr::filter(.data[[label_col]] == "CICATRIZ_DE_QUEIMADA") %>%
        dplyr::pull(tidyselect::all_of(said_col)) %>%
        unique()

    subareas_analysis_2 <-
        subareas_analysis_1 %>%
        dplyr::filter(.data[[said_col]] %in% xyid_burn_scar)

    plot_tb <-
        subareas_analysis_2 %>%
        dplyr::arrange(.data[[said_col]], .data[[date_col]]) %>%
        dplyr::select(tidyselect::all_of(area_col, label_col, said_col, 
                                         date_col, source_col)) %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::mutate(n_warnings = dplyr::n(),
                      warning_pos = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(CLASSNAME = dplyr::recode(.data[[label_col]],
                                                !!!prodes_common_names),
                      CLASSNAME = stringr::str_replace_all(.data[[label_col]],
                                                           pattern = "_",
                                                           replacement = " "),
                      CLASSNAME = stringr::str_to_sentence(.data[[label_col]]))

    # Create Sankey figures.
    # NOTE: I couldn't make ggsankey::geom_sankey use the variable subarea_ha.
    plot_ls <-
        plot_tb %>%
        dplyr::filter(.data[[nwarn_col]] > 1) %>%
        #######################################################################
        # TODO: Check if this is equivalent!
        #dplyr::select(-VIEW_DATE, data_source) %>%
        dplyr::select(-tidyselect::any_of(date_col)) %>%
        dplyr::select(tidyselect::all_of(source_col)) %>%
        #######################################################################
        dplyr::group_by(.data[[nwarn_col]]) %>%
        dplyr::group_split(.keep = TRUE) %>%
        purrr::map(get_plot_sankey)

    for (i in seq_along(plot_ls)) {
        ggplot2::ggsave(
            plot = plot_ls[[i]],
            filename = file.path(
                out_fig,
                paste0("an2_plot_deter_prodes_subarea_trajectory_", i + 1,
                       ".png")
            ),
            height = height, width = width, units = units
        )
    }
    rm(plot_ls)
    rm(plot_tb)
    rm(i)



    # TODO: Create map by trajetory.

    #---- Fire spots ----

    # Column names
    date_col <- "datahora"
    nfspot_col <- "n_fspot"
    state_col <- "estado"

    # Proportion of fires spots inside DETER
    # NOTE: The fire spots associated to a xy_id fall inside a DETER warning
    #       at some point in time. Those outside could our could not fall
    #       inside PRODES forest mask.
    # sum(!is.na(fire_sf$xy_id))/nrow(fire_sf)

    # Number of fire spots by month.
    plot_fire_spots_by_month <-
        fire_sf %>%
        sf::st_drop_geometry() %>%
        get_plot_fire_by_month()
    ggplot2::ggsave(plot_fire_spots_by_month,
                    filename = file.path(out_fig,
                                         "plot_fire_spots_by_month.png"),
                    height = height, width = width, units = units)

    # Number of fire spots by month and state.
    plot_fire_spots_by_month_state <-
        fire_sf %>%
        sf::st_drop_geometry() %>%
        get_plot_fire_by_month_state()
    ggplot2::ggsave(plot_fire_spots_by_month_state,
                    filename = file.path(out_fig,
                                         "plot_fire_spots_by_month_state.png"),
                    height = height, width = width, units = units)

###############################################################################
# TODO

    # Get forest masks.
    forest_mask_tb <-
        "/home/alber/Documents/data/prodes/amazonia_legal" %>%
        list.files(pattern = "^forest_[0-9]{4}.shp$",
                   full.names = TRUE) %>%
        tibble::as_tibble() %>%
        dplyr::rename(filepath = "value") %>%
        dplyr::mutate(
            filename = tools::file_path_sans_ext(basename(filepath))
        ) %>%
        tidyr::separate(col = filename, into = c(NA, "year"), sep = "_") %>%
        dplyr::mutate(forest_mask = purrr::map(filepath, sf::read_sf))

    #TODO: read forest'mask, intersect with states, compute state area in each mask
    #      re-calculate fire spot density using the mask.

    # # Get matadata from the Brazilian states.
    # br_state <-
    #     geobr::read_state(year = 2010) %>%
    #     (function(x, forest_mask_tb) {
    #          x %>%
    #              dplyr::mutate(
    #                 intersect_forest = purrr::map(., forest_mask_tb$forest_mask[[1]]
    #                 )


    #     })


###############################################################################

    br_state <-
        geobr::read_state(year = 2010) %>%
        # NOTE: Include only the state area covered by the Brazilian Legal
        #       Amazon.
        sf::st_intersection(y = geobr::read_amazon(year = 2012)) %>%
        dplyr::mutate(area_bla_km2 = sf::st_area(.),
                      area_bla_km2 = units::drop_units(area_bla_km2),
                      area_bla_km2 = area_bla_km2 / 1e6,
                      name_state = toupper(name_state),
                      name_state = iconv(name_state,
                                         to = "ASCII//TRANSLIT")) %>%
        sf::st_drop_geometry() %>%
        dplyr::select(name_state, area_bla_km2) %>%
        tibble::as_tibble()

    # Compute number of fire spots by year by subarea.
    fire_sa_year_tb <-
        fire_sf %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(.data[[said_col]], .data[[year_col]],
                        .data[[state_col]]) %>%
        dplyr::summarize("{nfspot_col}" := dplyr::n()) %>%
        dplyr::arrange(.by_group = TRUE) %>%
        dplyr::ungroup()

    fire_state_year_tb <-
        fire_sa_year_tb %>%
        dplyr::summarize(fire_spots = sum(.data[[nfspot_col]]),
                         .by = tidyselect::all_of(c(state_col, year_col))) %>%
        dplyr::arrange(.data[[state_col]], .data[[year_col]]) %>%
        dplyr::left_join(br_state,
                         by = dplyr::join_by(estado == name_state)) %>%
        dplyr::mutate(fspot_km2 = fire_spots / area_bla_km2)



    last_year <-
        fire_state_year_tb %>%
        dplyr::slice_max(.data[[year_col]],
                         by = tidyselect::all_of(state_col))

    # TODO: Normalize using PRODES forest mask instead of states' area.
    # TODO: Split fire splots using PRODES year.
    density_factor <- 2.5 * 1e6 # Transform density to match number of f spots.
    fire_state_year_tb %>%
        ggplot2::ggplot(ggplot2::aes(x = year,
                                     y = fspot_km2,
                                     group = estado)) +
        ggplot2::geom_col(ggplot2::aes(y = fire_spots,
                                       fill = estado)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = fspot_km2 * density_factor,
                                        group = estado,
                                        colour = estado),
                           linewidth = 1.6,
                           color = "black") +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = fspot_km2 * density_factor,
                                        group = estado,
                                        colour = estado),
                           linewidth = 1.0) +
        ggplot2::geom_label(data = last_year,
                            ggplot2::aes(x = year,
                                         y = fspot_km2 * density_factor,
                                         label = estado,
                                         vjust = 0,
                                         hjust = 0)) +
        ggplot2::scale_y_continuous(
            labels = scales::comma,
            "Number of fire spots",
            sec.axis = ggplot2::sec_axis(~ . / density_factor,
                                         name = "Fire density (spots/km2)")
        ) +
        ggplot2::xlab("Year (PRODES)") +
        ggplot2::theme(legend.title = ggplot2::element_blank())
    ggplot2::ggsave(filename = file.path(out_fig,
          "plot_fire_spots_density_by_state.png"),
                    height = height, width = width, units = units)
    rm(density_factor)




    # TODO: Compare yearly fire between fire spots and deter.
    #       Use fire_sa_year_tb and subareas_deter_prodes.

    subareas_flat <-
        subarea_sf %>%
        dplyr::filter(.data[[said_col]] %in%
                      dplyr::pull(subareas_deter_prodes, var = said_col)) %>%
        get_flat_subarea()




    subareas_fire_tb <-
        subareas_deter_prodes %>%
        dplyr::filter(.data[[source_col]] == "DETER") %>%
        dplyr::group_by(.data[[said_col]], .data[[year_col]]) %>%
        dplyr::mutate("{nwarn_col}" := dplyr::n()) %>%
        dplyr::select(tidyselect::all_of(c(said_col, year_col, nwarn_col))) %>%
        dplyr::full_join(y = fire_sa_year_tb,
                         by = c(said_col, year_col),
                         keep = TRUE,
                         na_matches = "na")

    subareas_fire_tb %>%
        tidyr::drop_na() %>%
        dplyr::pull(.data[[nfspot_col]]) %>%
        log() %>%
        graphics::hist()



    #---- End ----
    invisible(out_dir)

}


