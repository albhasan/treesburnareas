#' @title Get the DETER classes
#'
#' @name get_deter_classes
#'
#' @description
#' This function returns a tibble with the classes in the DETER data.
#' @return A tibble.
#' @export
get_deter_classes <- function() {
    tibble::tribble(
        ~class, ~class_en,
        "CICATRIZ_DE_QUEIMADA",  "Wildfire scar",
        "CORTE_SELETIVO",        "Selective cut",
        "CS_DESORDENADO",        "Selective cut (disordered)",
        "CS_GEOMETRICO",         "Selective cut (geometric)",
        "DEGRADACAO",            "Degradation",
        "DESMATAMENTO_CR",       "Deforestation with exposed soil",
        "DESMATAMENTO_VEG",      "Deforestation with vegetation",
        "MINERACAO",             "Mining"
    )
}

