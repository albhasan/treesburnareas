# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

#' @importFrom dplyr .data
NULL

#' @importFrom rlang :=
NULL

if(getRversion() >= "3.1.0")
    utils::globalVariables(c(
        # ".x", ":=" # dplyr
        "."       # magrittr
    ))

