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
        dplyr::summarize(n_traj = dplyr::n(),
                         subarea_ha = sum(subarea_ha)) %>%
        tidyr::separate_wider_delim(cols = position,
                                    delim = ";",
                                    names_sep = "_") %>%
        dplyr::arrange(dplyr::pick(tidyselect::starts_with("position")),
                       dplyr::desc(n_traj)) %>%
        return()
}

