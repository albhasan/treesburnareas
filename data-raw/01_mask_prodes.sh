#!/bin/bash
###############################################################################
# CREATE A MASK OF THE PRODES RASTER. 
###############################################################################

TIF_FILE="/home/alber/data/prodes/amazonia/PDigital2000_2021_AMZ_raster_v20220915_bioma.tif"
OUT_TIF="/home/alber/data/prodes/prodes_mask.tif"
OUT_VEC="/home/alber/data/prodes/prodes_mask.gpkg"
VEC_NAME=prodes_mask



# Validation.
if [ -f "$TIF_FILE" ]; then
        echo "INFO: prodes raster found at $TIF_FILE"
    else
        echo "ERROR: prodes raster is missing. Expected location: $TIF_FILE"
        exit 1
fi
if command -v gdal_calc.py &> /dev/null; then
        echo "INFO: gdal_calc.py found!"
    else
        echo "ERROR: gdal_calc.py could not be found. Please install it."
        exit 1
fi
if command -v gdal_polygonize.py &> /dev/null; then
        echo "INFO: gdal_polygonize.py found!"
    else
        echo "ERROR: gdal_polygonize.py could not be found. Please install it."
        exit 1
fi

gdal_calc.py -A "${TIF_FILE}" --calc="A > 0" --outfile "${OUT_TIF}" --NoDataValue=0 --type=Byte --format=GTiff --co="COMPRESS=LZW" --co="BIGTIFF=YES" --quiet

gdal_polygonize.py "${OUT_TIF}" -f GPKG "${OUT_VEC}" "${VEC_NAME}" -q

exit 0
