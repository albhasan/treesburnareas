library(dplyr)
library(sf)
library(ggplot2)

data_sf <-
    "~/Documents/trees_lab/data/tmp/deter_ba_overlaped.shp" %>%
    sf::read_sf()

gtype <-
    data_sf %>%
    dplyr::mutate(geom_type = sf::st_geometry_type(.)) %>%
    pull(geom_type) %>%
    unique()




#---- Test the self-intersection of polygons ----

# NOTE: st_union is not the same QGIS union!
test_union <-
    data_sf %>%
    sf::st_union(by_feature = TRUE, is_coverage = TRUE)
plot(sf::st_geometry(test_union))
plot(sf::st_geometry(data_sf))
nrow(test_union) == nrow(data_sf)
rm(test_union)



# NOTE: This one creates a matrix-like object holding intersecting ids.
res1 <- sf::st_intersects(data_sf)
nrow(data_sf) == nrow(res1)

# NOTE:
# This one self-intersect the polygons and recycle their attributes.
# The resulting number of polygons is likely to be different from the original.
# It also adds an "origins" list-column holding vectors of ids.
# The self-intersection operation could result in different geometry types.
res2 <- data_sf %>%
    sf::st_intersection()
nrow(data_sf) <  nrow(res2)



# NOTE: The problem aren't the precision parameters!
## Check the precision parameters!
#    data_sf %>%
#    # One meter in degrees at the equator on a WGS84 ellipsoid (approx.).
#    #sf::st_set_precision(1) %>%
#    #sf::st_set_precision(1 / 6378137) %>%
#    sf::st_set_precision(0) %>%
#    sf::st_intersection() %>%
#    dplyr::select(UF) %>%
#    nrow()




data_sf  %>%
    select("UF") %>%
    plot(main = "original")

# NOTE: The different geometries produced!
res2 %>%
    dplyr::mutate(geom_type = sf::st_geometry_type(.)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = geom_type)) +
    ggplot2::facet_wrap(~geom_type) +
    ggplot2::ggtitle("st_intersection results")

# Take the features' Geometry Collection and extract the polygons!
res2 %>%
    sf::st_collection_extract(type = "POLYGON") %>%
    dplyr::filter(!sf::st_is_empty(.)) %>%
    dplyr::mutate(geom_type = sf::st_geometry_type(.)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = geom_type)) +
    ggplot2::facet_wrap(~geom_type) +
    ggplot2::ggtitle("st_collection_extract results casted to polygons")

res2 %>%
    # One meter in degrees at the equator on a WGS84 ellipsoid (approx.).
    #sf::st_set_precision(0) %>%
    #sf::st_set_precision(1) %>%
    #sf::st_set_precision(1 / 6378137) %>%
    sf::st_intersection() %>%
    sf::st_collection_extract(type = "POLYGON") %>%
    dplyr::select(-origins) %>%
    dplyr::filter(!sf::st_is_empty(.)) %>%
    sf::write_sf("~/Documents/trees_lab/data/tmp/overlap_polygons.shp")

res3 %>%
    sf::st_collection_extract(type = "POLYGON") %>%
    dplyr::filter(!sf::st_is_empty(.))
