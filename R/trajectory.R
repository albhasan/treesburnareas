#' @title Compute trajectory statistics
#'
#' @name get_trajectory_stats
#'
#' @description
#' This function computes statistics about trajectories with the same number
#' of steps.
#' @param data_tb A tibble.
#' @return        A tibble.
#' @export
get_trajectory_stats <- function(data_tb) {
    CLASSNAME <- n_traj <- position <- subarea_ha <- warning_pos <- NULL
    stopifnot("Trajectories of different lengths aren't supported!" =
              length(unique(data_tb[["n_warnings"]])) == 1)
    data_tb %>%
        tidyr::pivot_wider(names_from = warning_pos,
                           names_prefix = "pos",
                           values_from = CLASSNAME) %>%
        tidyr::unite("position",
                     tidyselect::starts_with("pos"),
                     remove = TRUE,
                     sep = ";") %>%
        dplyr::group_by(position) %>%
        dplyr::summarize(
            min_area = min(subarea_ha, na.rm = TRUE),
            max_area = max(subarea_ha, na.rm = TRUE),
            mean_area  = mean(subarea_ha, na.rm = TRUE),
            median_area = stats::median(subarea_ha, na.rm = TRUE),
            sd_area = stats::sd(subarea_ha, na.rm = TRUE),
            area_ha = sum(subarea_ha, na.rm = TRUE),
            n_traj = dplyr::n(),
        ) %>%
        tidyr::separate_wider_delim(cols = position,
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
    CLASSNAME <- data_source <- traj_position <- VIEW_DATE <- xy_id <- NULL
    year <- NULL

    stopifnot("Directory not found!" = dir.exists(out_dir))

    trajs_start_with_def_file <- file.path(out_dir, "trajs_start_with_def.shp")
    trajs_end_with_for_file   <- file.path(out_dir, "trajs_end_with_for.shp")

    subarea_flat_sf <-
        subarea_sf %>%
        get_flat_subarea()

   # Trajectory starts with PRODES deforestation.
   subarea_dt %>%
        dplyr::group_by(xy_id) %>%
        dplyr::arrange(VIEW_DATE) %>%
        dplyr::first() %>%
        dplyr::ungroup() %>%
        dplyr::filter(data_source == "PRODES",
                      stringr::str_starts(CLASSNAME, pattern = "d")) %>%
        dplyr::select(CLASSNAME, VIEW_DATE, xy_id, traj_position, year) %>%
        merge(subarea_flat_sf, .,
              by = "xy_id") %>%
        sf::write_sf(trajs_start_with_def_file)

    # Trajectory ends with PRODES forest.
   subarea_dt %>%
        dplyr::group_by(xy_id) %>%
        dplyr::arrange(dplyr::desc(VIEW_DATE)) %>%
        dplyr::first() %>%
        dplyr::ungroup() %>%
        dplyr::filter(data_source == "PRODES",
                      stringr::str_starts(CLASSNAME,
                                          pattern = "FOREST_2021")) %>%
        dplyr::select(CLASSNAME, VIEW_DATE, xy_id, traj_position, year) %>%
        merge(subarea_flat_sf, .,
              by = "xy_id") %>%
        sf::write_sf(trajs_end_with_for_file)

    return(c("trajs_start_with_def" = trajs_start_with_def_file,
             "trajs_end_with_for" = trajs_end_with_for_file))
}

