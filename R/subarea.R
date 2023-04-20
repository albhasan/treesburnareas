#' @title Return an flat subarea SF object.
#'
#' @name get_flat_subarea
#'
#' @description
#' This function removes overlaped subareas.
#' @param subarea_sf An sf object.
#' @return An sf object.
#' @export
get_flat_subarea <- function(subarea_sf) {
    xy_id <- NULL
    stopifnot("SF object expected!" = inherits(subarea_sf, what = "sf"))
    stopifnot("xy_id column not found!" = "xy_id" %in% colnames(subarea_sf))
    treesburnareas::subarea_sf %>%
        dplyr::select(xy_id) %>%
        dplyr::arrange(xy_id) %>%
        dplyr::distinct(xy_id, .keep_all = TRUE) %>%
        return()
}
