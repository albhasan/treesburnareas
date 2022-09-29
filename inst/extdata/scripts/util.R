
#' Estimate the PRODES year of the given date.
#'
#' @param x A date.
#' @param start_month The month when the year starts.
#' @param start_day   The day when the year starts.
#'
#' @return            An integer. The PRODES's year.
#'
to_year_prodes <- function(x, start_month = "08", start_day = "01") {
    stopifnot("At least a date is expected!" = length(x) != 0)
    stopifnot("Date object expected!" = lubridate::is.Date(x))
    if (length(x) > 1) {
        return(vapply(x,  FUN = to_year_prodes,  FUN.VALUE = integer(1)))
    }
    y <- as.integer(format(as.Date(x, format = "%d/%m/%Y"),"%Y"))
    start_year <- paste(y, start_month, start_day, sep = "-")
    if (x >= start_year)
        return(as.integer(y + 1))
    return(y)
}

#' Estimate the number of days since the start of the PRODES year.
#'
#' @param x A date.
#' @param start_month The month when the year starts.
#' @param start_day   The day when the year starts.
#'
#' @return            An integer.
#'
to_doy_prodes <- function(x, start_month = "08", start_day = "01") {
    stopifnot("At least a date is expected!" = length(x) != 0)
    stopifnot("Date object expected!" = lubridate::is.Date(x))
    if (length(x) > 1) {
        return(vapply(x,  FUN = to_doy_prodes,  FUN.VALUE = integer(1)))
    }
    y <- as.integer(format(as.Date(x, format = "%d/%m/%Y"),"%Y"))
    m <- as.integer(format(as.Date(x, format = "%d/%m/%Y"),"%m"))
    start_year <- paste(y, start_month, start_day, sep = "-")
    if (m < as.integer(start_month))
        start_year <- paste(y - 1, start_month, start_day, sep = "-")
    return(as.integer(difftime(x, start_year, units = "days")))
}

fix_geom_s2 <- function(data_sf) {
    stopifnot("sf object expected" = inherits(data_sf, "sf"))
    data_sf %>%
slice(1:10000) %>%
        dplyr::mutate(is_valid_s2 = s2::s2_is_valid(.))
}
