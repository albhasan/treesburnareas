#' @title Compute trajectory statistics
#'
#' @name get_trajectory_stats
#'
#' @description
#' This function computes statistics about trajectories with the same number
#' of steps.
#' @param data_tb A tibble with trajectory data.
#' @param nwarn_col A character. The name of the column with the number of
#' warnings.
#' @param nwarn_pos A character. The name of the column with the position of
#' an event in the trajectory of a subarea.
#' @param label_col A character. The name of the column with labels.
#' @param area_col  A character. The name of the column with area.
#' @return        A tibble.
#' @export
get_trajectory_stats <- function(data_tb, nwarn_col = "n_warnings",
                                 nwarn_pos = "warning_pos",
                                 label_col = "CLASSNAME",
                                 area_col =  "subarea_ha") {
    position <- "position"
    stopifnot("Column 'n_warnings' is missing" =
              nwarn_col %in% colnames(data_tb))
    stopifnot("Trajectories of different lengths aren't supported!" =
              length(unique(data_tb[[nwarn_col]])) == 1)
    data_tb %>%
        tidyr::pivot_wider(names_from = tidyselect::all_of(nwarn_pos),
                           names_prefix = "pos",
                           values_from = tidyselect::all_of(label_col)) %>%
        tidyr::unite(col = position,
                     tidyselect::starts_with("pos"),
                     sep = ";",
                     remove = TRUE) %>%
        dplyr::group_by(.data[[position]]) %>%
        dplyr::summarize(
            min_area = min(.data[[area_col,]], na.rm = TRUE),
            max_area = max(.data[[area_col]], na.rm = TRUE),
            mean_area  = mean(.data[[area_col]], na.rm = TRUE),
            median_area = stats::median(.data[[area_col]], na.rm = TRUE),
            sd_area = stats::sd(.data[[area_col]], na.rm = TRUE),
            area_ha = sum(.data[[area_col]], na.rm = TRUE),
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
#' @param subarea_tb A tibble with data about subtrajectories. See the data in
#'                   this package.
#' @return        A character with paths to files storing trajectories which
#'                start with the class deforestation or end with the class
#'                forest.
#' @export
report_suspicious_trajectories <- function(subarea_sf, subarea_tb, out_dir) {
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
   subarea_tb %>%
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
   subarea_tb %>%
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
#' @param said_col  A character. Name of the column with
#' @param label_col A character. Name of the column with labels.
#' @param date_col  A character. Name of the view date column.
#' @return          A tibble.
#' @export
trim_trajectories <- function(data_tb, end_class, said_col = "xy_id",
                              label_col = "CLASSNAME",
                              date_col = "VIEW_DATE") {
    stopifnot(c(said_col, label_col, date_col) %in% colnames(data_tb))
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
            .data[[".warning_pos"]], .data[[".n_warnings"]])
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


#' @title Filter trajectories
#'
#' @name filter_trajectory
#'
#' @description
#' This function filters whole trajectories based on attributes of a single
#' trajectory step.
#' @param data_tb    A tibble with trajectory data.
#' @param filter_col A character. Name of a column.
#' @param filter_val Value to filter.
#' @param said_col   A character. Name of the column with subarea ids.
#' @param invert     A logical. Should the filter results be inverted?
#' @return           A tibble.
#' @export
filter_trajectory <- function(data_tb, filter_col, filter_val,
                              said_col = "xy_id", invert = FALSE) {
    ids <-
        data_tb %>%
        dplyr::filter(.data[[filter_col]] == filter_val) %>%
        dplyr::pull(tidyselect::all_of(said_col)) %>%
        unique()
    if (invert) {
        data_tb %>%
            dplyr::filter(!(.data[[said_col]] %in% ids)) %>%
            return()
    } else {
        data_tb %>%
            dplyr::filter(.data[[said_col]] %in% ids) %>%
            return()
    }
}


#' @title Flat trajectories
#'
#' @name flat_trajectory
#'
#' @description
#' This function ensures there is only one event in each year.
#' @param data_tb    A tibble with trajectory data.
#' @param keep       A character. Obervation to keep. Either "first" or "last".
#' @param said_col   A character. Name of the column with subarea ids.
#' @param year_col   A character. Name of the column with the observations'
#' year.
#' @param date_col   A character. Name of the column with the observations'
#' date.
#' @param other_col  A character. Name of an additional grouping column.
#' @return           A tibble.
#' @export
flat_trajectory <- function(data_tb, keep = "first",
                            said_col = "xy_id", year_col = "year",
                            date_col = "VIEW_DATE",
                            other_col = NA
                            ) {
    stop("DEPRECATED!")
    stopifnot(keep %in% c("first", "last"))
    stopifnot("Only one column supported" = length(other_col) == 1)
    .my_fun <- dplyr::slice_head
    if (keep == "last")
        .my_fun <- dplyr::slice_tail

    data_tb %>%
        dplyr::arrange(.data[[said_col, date_col]]) %>%
        (function(x) {
            if (!is.na(other_col)) {
                return(dplyr::group_by(x, .data[[said_col, year_col,
                                       other_col]]))
            } else {
                return(dplyr::group_by(x,
                                       .data[[said_col]],
                                       .data[[year_col]]))
            }
        }) %>%
        .my_fun(n = 1) %>%
        dplyr::ungroup() %>%
        return()
}

