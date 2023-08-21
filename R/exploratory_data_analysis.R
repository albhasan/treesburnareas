#' @title Create EDA plots and tables
#'
#' @name create_plots
#'
#' @description
#' This function creates the figures and tables of the Exploratory Data
#' Analysis of the Amazonia deforestation warnings issued by DETER.
#' @param out_dir A path to a directory.
#' @param subarea_tb A tibble with data about DETER subareas.
#' @param fire_sf A sf object (point). Fire spots.
#' @return out_dir (invisible).
#' @examples
#' \dontrun{
#'     create_plots(out_dir = "~/Documents/report")
#' }
#' @export
create_plots <- function(out_dir, subarea_tb, fire_sf) {

    #TODO: make  parameters out of subarea_sf & subarea_tb
    area_ha <- area_km2 <- CLASSNAME <- closest_class <- closest_date <- NULL
    common_name <- data_source <- diff_days <- in_prodes <- NULL
    last_CLASSNAME <- max_area <- mean_area <- median_area <- min_area <- NULL
    n <- n_warnings <- next_class <- next_date <- prev_class <- NULL
    prev_date <- sd_area <- subarea_ha <- subarea_step <- subarea_tb <- NULL
    total_ha <- warning_pos <- UF <- VIEW_DATE <- xy_id <- year <- NULL

    stopifnot("Directory not found!" = dir.exists(out_dir))


    #---- Setup ----

    # TODO: Remove.
    subarea_tb <- treesburnareas::subarea_tb
    fire_sf    <- treesburnareas::fire_sf

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


    #---- Column names ----

    area_col     <- "subarea_ha"  # Area.
    date_col     <- "VIEW_DATE"   # View date.
    inprodes_col <- "in_prodes"   # Does the subarea fall inside PRODES' mask?
    label_col    <- "CLASSNAME"   # Label column.
    month_col    <- "month"       # Month column.
    ntraj_col    <- "n_traj"      # Number of trajectories.
    nwarn_col    <- "n_warnings"  # Number of warnings by subarea.
    nwarn_pos    <- "warning_pos" # Position of the warning in a subarea traj.
    said_col     <- "xy_id"       # Subarea ID column.
    source_col   <- "data_source" # Does the data comes from DETER or PRODES?
    state_col    <- "UF"          # Name of the state.
    year_col     <- "year"        # PRODES year.

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
        dplyr::mutate(year = as.character({{year_col}})) %>%
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


    #---- Preprocessing ----

    # NOTE: Subareas (DETER & PRODES) that fall inside the PRODES mask,
    #       happened before max_date, and have a minimum size.
    subareas_deter_prodes <-
        subarea_tb %>%
        dplyr::arrange(.data[[said_col]],
                       .data[[date_col]]) %>%
        # Fill in the missing areas of PRODES.
        dplyr::group_by(.data[[said_col]]) %>%
        tidyr::fill(tidyselect::all_of(area_col),
                    .direction = "downup") %>%
        dplyr::ungroup() %>%
        # NOTE: Keep all of PRODES data by not using a min_date.
        dplyr::filter(
            .data[[said_col]] %in%
                .get_xyid_in_prodes(subarea_tb),
            .data[[date_col]] < max_date,
            .data[[area_col]] > min_subarea_ha
        )



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
        # check data scripts for deter ids.



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
        dplyr::mutate("{nwarn_col}" := dplyr::n(), #n_warnings = dplyr::n(),
                      warning_pos = dplyr::row_number(),
                      CLASSNAME = stringr::str_replace_all(CLASSNAME,
                                                           pattern = "_",
                                                           replacement = " "),
                      CLASSNAME = stringr::str_to_sentence(CLASSNAME)) %>%
        dplyr::ungroup() %>%
        dplyr::select(tidyselect::all_of(area_col, label_col, said_col,
                                         nwarn_col, nwarn_pos, year_col))

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
        dplyr::select(-year) %>%
        dplyr::filter(n_warnings > 1) %>%
        dplyr::group_by(n_warnings) %>%
        dplyr::group_split(.keep = TRUE) %>%
        furrr::future_map(get_trajectory_stats)

    for (i in seq_along(table_ls)) {
        table_ls %>%
            magrittr::extract2(i) %>%
            # NOTE: We renamed area column. Check get_trajectory_stats.
            dplyr::select(-min_area, -max_area, -mean_area,
                          -median_area, -sd_area,
                          subarea_ha = area_ha) %>%
            .adorn_table(top_rows = top_rows) %>%
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

    # Create Sankey figure (by number of warnings).
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
                      CLASSNAME = stringr::str_to_sentence(CLASSNAME))

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
                         median_days = stats::median(as.double(diff_days)),
                         median_days_abs =
                             stats::median(abs(as.double(diff_days))),
                         sd_days = stats::sd(as.double(diff_days)),
                         sd_abs = stats::sd(abs(as.double(diff_days)))) %>%
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
                          -median_area, -sd_area,
            # NOTE: We renamed area column. Check get_trajectory_stats.
                          subarea_ha = area_ha) %>%
            .adorn_table(top_rows = top_rows) %>%
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

    # TODO: Create Sankey figure (all years).
    # NOTE: ggsankey doesn't work well with large amounts of data.

    # Create Sankey figure (by number of recurrent warnings).
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

    # Labeld considered as deforestation used to trim trajectories.
    deforestation_classes <- c("P Deforestation", "P Residual",
                               "DESMATAMENTO_CR", "DESMATAMENTO_VEG")

################################################################################
## TODO: convert into tests!
#    res <-
#        subareas_deter_prodes %>%
#        dplyr::arrange(xy_id, VIEW_DATE)
#    res_fil <-
#        res %>%
#        filter_trajectory(filter_col = "CLASSNAME", filter_val = "MINERACAO",
#                          invert = FALSE)
#    stopifnot(nrow(res) >= nrow(res_fil))
#    stopifnot(ncol(res) == ncol(res_fil))
#    stopifnot(length(unique(res[res$CLASSNAME == "MINERACAO",][["xy_id"]])) ==
#length(unique(res_fil$xy_id)))
#    stopifnot(all(unique(res_fil$xy_id) %in% unique(res$xy_id)))
#    stopifnot(length(unique(res_fil$xy_id)) <= length(unique(res$xy_id)))
#    res_fil_inv <-
#        res %>%
#        filter_trajectory(filter_col = "CLASSNAME", filter_val = "MINERACAO",
#                          invert = TRUE)
#    stopifnot(nrow(dplyr::filter(res_fil_inv, CLASSNAME == "MINERACAO")) == 0)
################################################################################

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
        dplyr::mutate(n_warnings = dplyr::n(),
                      warning_pos = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        # TODO: replace with name injection.
        dplyr::mutate(CLASSNAME = dplyr::recode(CLASSNAME,
                                                !!!prodes_common_names),
                      CLASSNAME = stringr::str_replace_all(CLASSNAME,
                                                           pattern = "_",
                                                           replacement = " "),
                      CLASSNAME = stringr::str_to_sentence(CLASSNAME))

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
                      CLASSNAME = stringr::str_to_sentence(CLASSNAME))

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


    # Proportion of fires spots inside DETER
    # NOTE: The fire spots associated to a xy_id fall inside a DETER warning
    #       at some point in time. Those outside could our could not fall
    #       inside PRODES forest mask.
    # sum(!is.na(fire_sf$xy_id))/nrow(fire_sf)

    # Number of fire spots by month and state.
    plot_fire_spots_by_month <-
        fire_sf %>%
        sf::st_drop_geometry() %>%
        get_plot_fire_by_month()
    ggplot2::ggsave(plot_fire_spots_by_month,
                    filename = file.path(out_fig,
                                         "plot_fire_spots_by_month.png"),
                    height = height, width = width, units = units)


    #---- End ----
    invisible(out_dir)

}

