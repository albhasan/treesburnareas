#!/bin/bash
###############################################################################
# EXPORT DETER SHAPEFILE TO GEOPACKAGE
###############################################################################

# Path to the DETER file downloaded from TERRABRASILIS
DETER_SHP="/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp"
OUT_GPKG="/home/alber/Documents/data/deter/amazonia_legal/deter_ogr2ogr.gpkg"
OUT_LAYER=deter_public


#---- Utilitary functions ----

is_file_valid () {
    if [ -f "$1" ]; then
        echo "INFO: File found: $1"
    else
        echo "ERROR: Missing file: $1"
        exit 1
    fi
}


#---- Validation ----

if command -v ogr2ogr &> /dev/null; then
        echo "INFO: ogr2ogr found!"
    else
        echo "ERROR: ogr2ogr could not be found. Please install it."
        exit 1
fi

is_file_valid $DETER_SHP


#---- Export ----

ogr2ogr -nlt MULTIPOLYGON -nln ${OUT_LAYER} -f GPKG ${OUT_GPKG} ${DETER_SHP}

exit 0
