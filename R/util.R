#' @title Compute the mode of a vector.
#'
#' @name the_mode
#'
#' @description
#' This function computes the mode of the given vector, ignoring NA elements.
#' @param x A vector.
#' @return A vector.
#' @examples
#' the_mode(sample(1:10, size = 100, replace = TRUE))
#' @export
the_mode <- function(x) {
    x <- x[!is.na(x)]
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

