# Count the downloaded files.

library(dplyr)
library(terra)

mcd_tb <-
    "/home/alber/Documents/data/mcd64a1/MCD64A1.061" %>%
    list.files(full.names = TRUE,
               recursive = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(path = value) %>%
    dplyr::mutate(file_name = basename(path)) %>%
    tidyr::separate(file_name,
                    into = c("prod", "date_obs", "tile",
                             "col", "date_proc", NA))

mcd_tb %>%
    dplyr::count(tile)

r <-
    mcd_tb %>%
    dplyr::slice(1) %>%
    dplyr::pull(path) %>%
    magrittr::extract2(1) %>%
    terra::rast()
plot(r)

fire_tb <-
    "/home/alber/Documents/data/fire_cci/burned_area/MODIS/pixel/v5.1/compressed" %>%
    list.files(full.names = TRUE,
               recursive = TRUE,
               pattern = "*.tar.gz") %>%
    tibble::as_tibble() %>%
    dplyr::rename(path = value) %>%
    dplyr::mutate(file_name = basename(path)) %>%
    tidyr::separate(file_name,
                    sep = "-",
                    into = c("date_obs", "prod", "level", "x1", "sat", "region", "x2"))





# end

