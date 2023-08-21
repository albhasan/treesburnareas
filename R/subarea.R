#' @title Return an flat subarea SF object
#'
#' @name get_flat_subarea
#'
#' @description
#' This function removes overlaped subareas.
#' @param subarea_sf An sf object.
#' @return An sf object.
#' @export
get_flat_subarea <- function(subarea_sf) {
    id_col <- "xy_id"
    stopifnot("sf object expected!" = inherits(subarea_sf, what = "sf"))
    stopifnot("DETER subarea id column not found!" =
              id_col %in% colnames(subarea_sf))
    subarea_sf %>%
        dplyr::select(tidyselect::all_of(id_col)) %>%
        dplyr::arrange(.data[[id_col]]) %>%
        dplyr::distinct(.data[[id_col]],
                        .keep_all = TRUE) %>%
        return()
}


#' @title Compare subareas' id to their centroids
#'
#' @name test_centroids
#'
#' @description
#' This function compares the subarea id, which is made of the centroids of
#' each subarea, to the actual centroids computed using the sf package.
#' @param data_sf  An sf object.
#' @param sample_n An integer.
#' @param tol      A numeric. The tolerance used for the comparison.
#' @return         A logical.
#' @export
test_centroids <- function(data_sf, sample_n = 50, tol = 0.00001) {
    X <- x_id <- Y <- y_id <- NULL
    said_col <- "xy_id"
    # Get a random xy id.
    ids <-
        data_sf %>%
        sf::st_drop_geometry() %>%
        dplyr::select(.data[[said_col]]) %>%
        dplyr::distinct() %>%
        dplyr::slice_sample(n = sample_n) %>%
        dplyr::pull(.data[[said_col]])
    # Test if the xy_id and the centroid are close.
    data_sf %>%
        dplyr::select(tidyselect::all_of(said_col)) %>%
        dplyr::filter(.data[[said_col]] %in% ids) %>%
        (function(x) {
             cbind(x, as.data.frame(sf::st_coordinates(sf::st_centroid(x))))
        }) %>%
        sf::st_drop_geometry() %>%
        tibble::as_tibble() %>%
        tidyr::separate_wider_delim(cols = tidyselect::all_of(said_col),
                                    delim = ";",
                                    names = c("x_id", "y_id")) %>%
        dplyr::mutate(x_test = abs(X - as.double(x_id)) <= tol,
                      y_test = abs(Y - as.double(y_id)) <= tol) %>%
        dplyr::select(tidyselect::all_of(c("x_test", "y_test"))) %>%
        as.vector() %>%
        unlist() %>%
        all() %>%
        return()
}

