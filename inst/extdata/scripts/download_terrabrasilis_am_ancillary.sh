#!/bin/bash
#
###############################################################################
# Download ancillary data from terrabrailis.
#------------------------------------------------------------------------------
###############################################################################

OUT_DIR="${HOME}/data/terrabrasilis"
[ -d "${OUT_DIR}" ] || { echo "ERROR: Directory ${OUT_DIR} does not exist!" >&2; exit 1; }

wget http://terrabrasilis.dpi.inpe.br/download/dataset/amz-aux/vector/amazon_biome_border.zip -P "${OUT_DIR}" -t 5
wget http://terrabrasilis.dpi.inpe.br/download/dataset/amz-aux/vector/conservation_units_amazon_biome.zip -P "${OUT_DIR}" -t 5
wget http://terrabrasilis.dpi.inpe.br/download/dataset/amz-aux/vector/indigenous_area_amazon_biome.zip -P "${OUT_DIR}" -t 5
wget http://terrabrasilis.dpi.inpe.br/download/dataset/amz-aux/vector/municipalities_amazon_biome.zip -P "${OUT_DIR}" -t 5
wget http://terrabrasilis.dpi.inpe.br/download/dataset/amz-aux/vector/states_amazon_biome.zip -P "${OUT_DIR}" -t 5
wget http://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/accumulated_deforestation_2007_biome.zip -P "${OUT_DIR}" -t 5
wget http://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/forest_biome.zip -P "${OUT_DIR}" -t 5

exit 0
