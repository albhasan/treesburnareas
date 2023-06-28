#' @title Compute trajectory statistics
#'
#' @name get_trajectory_stats
#'
#' @description
#' This function computes statistics about trajectories with the same number
#' of steps.
#' @param data_tb A tibble with trajectory data.
#' @return        A tibble.
#' @export
get_trajectory_stats <- function(data_tb) {
    n_warnings <- "n_warnings"
    warning_pos <- "warning_pos"
    label <- "CLASSNAME"
    position <- "position"
    area <- "subarea_ha"
    stopifnot("Column 'n_warnings' is missing" =
              n_warnings %in% colnames(data_tb))
    stopifnot("Trajectories of different lengths aren't supported!" =
              length(unique(data_tb[[n_warnings]])) == 1)
    data_tb %>%
        tidyr::pivot_wider(names_from = tidyselect::all_of(warning_pos),
                           names_prefix = "pos",
                           values_from = tidyselect::all_of(label)) %>%
        tidyr::unite(col = position,
                     tidyselect::starts_with("pos"),
                     sep = ";",
                     remove = TRUE) %>%
        dplyr::group_by(.data[[position]]) %>%
        dplyr::summarize(
            min_area = min(.data[[area,]], na.rm = TRUE),
            max_area = max(.data[[area]], na.rm = TRUE),
            mean_area  = mean(.data[[area]], na.rm = TRUE),
            median_area = stats::median(.data[[area]], na.rm = TRUE),
            sd_area = stats::sd(.data[[area]], na.rm = TRUE),
            area_ha = sum(.data[[area]], na.rm = TRUE),
            n_traj = dplyr::n(),
        ) %>%
        tidyr::separate_wider_delim(cols = tidyselect::all_of(position),
                                    delim = ";",
                                    names_sep = "_") %>%
        return()
}


#' @title Find potential inconsistent trajectories
#'
#' @name report_suspicious_trajectories
#'
#' @description
#' This function reports trajectories with potential inconsistencies.
#' of steps.
#' @param out_dir A path to a directory.
#' @param subarea_sf An sf object with data about trajectories. See the data
#'                   in this package.
#' @param subarea_dt A data.table object with data about subtrajectories. See
#'                   the data in this package.
#' @return        A character with paths to files storing trajectories which
#'                start with the class deforestation or end with the class
#'                forest.
#' @export
report_suspicious_trajectories <- function(subarea_sf, subarea_dt, out_dir) {
    data_source <- NULL
    stopifnot("Directory not found!" = dir.exists(out_dir))
    trajs_start_with_def_file <- file.path(out_dir, "trajs_start_with_def.shp")
    trajs_end_with_for_file   <- file.path(out_dir, "trajs_end_with_for.shp")
    subarea_id <- "xy_id"
    view_date <- "VIEW_DATE"
    label <- "CLASSNAME"
    year <- "year"
    subarea_flat_sf <-
        subarea_sf %>%
        get_flat_subarea()
   # Trajectory starts with PRODES deforestation.
   subarea_dt %>%
        dplyr::group_by(.data[[subarea_id]]) %>%
        dplyr::arrange(.data[[subarea_id]],
                       .data[[view_date]]) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::filter(data_source == "PRODES",
                      stringr::str_starts(.data[[label]],
                                          pattern = "d")) %>%
        dplyr::select(tidyselect::all_of(c(label, view_date,
                                           subarea_id, year))) %>%
        merge(subarea_flat_sf, .,
              by = subarea_id) %>%
        sf::write_sf(trajs_start_with_def_file)
    # Trajectory ends with PRODES forest.
   subarea_dt %>%
        dplyr::group_by(.data[[subarea_id]]) %>%
        dplyr::arrange(.data[[subarea_id]],
                       .data[[view_date]]) %>%
        dplyr::slice_tail(n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::filter(data_source == "PRODES",
                      stringr::str_starts(.data[[label]],
                                          pattern = "FOREST_2021")) %>%
        dplyr::select(tidyselect::all_of(c(label, view_date,
                                           subarea_id, year))) %>%
        merge(subarea_flat_sf, .,
              by = subarea_id) %>%
        sf::write_sf(trajs_end_with_for_file)
    return(c("trajs_start_with_def" = trajs_start_with_def_file,
             "trajs_end_with_for" = trajs_end_with_for_file))
}


#' @title End the trajectories at certain class.
#'
#' @name trim_trajectories
#'
#' @description
#' This function computes removes events from trajectories after certain class
#' is found.
#' @param data_tb   A tibble with trajectory data.
#' @param end_class A character. Classes on which trajectories should end.
#' @return          A tibble.
#' @export
trim_trajectories <- function(data_tb, end_class) {
    .n_warnings <- .warning_pos <- NULL
    said_col <- "xy_id"      # Subarea ID column.
    label_col <- "CLASSNAME" # Label column.
    date_col <- "VIEW_DATE" # View date column.
    stopifnot(
        #sprintf("Missing expected columns: %s, %s, %s", said_col, label_col, date_col) =
            c(said_col, label_col, date_col) %in% colnames(data_tb))
    data_tb <-
        data_tb %>%
        dplyr::arrange(.data[[said_col]],
                       .data[[date_col]]) %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::mutate(.n_warnings = dplyr::n(),
                      .warning_pos = dplyr::row_number()) %>%
        dplyr::ungroup()
    max_pos_tb <-
        data_tb %>%
        dplyr::mutate(
            .max_pos = dplyr::if_else(.data[[label_col]] %in% end_class,
            .data[[.warning_pos]], .data[[.n_warnings]])
        ) %>%
        dplyr::group_by(.data[[said_col]]) %>%
        dplyr::slice_min(.data[[".max_pos"]], n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data[[said_col]],
                        .data[[".max_pos"]])
    data_tb %>%
        dplyr::left_join(max_pos_tb, by = "xy_id") %>%
        dplyr::filter(.data[[".warning_pos"]] <= .data[[".max_pos"]]) %>%
        dplyr::select(-tidyselect::all_of(c(".max_pos",
                                            ".warning_pos",
                                            ".n_warnings"))) %>%
        return()
}

