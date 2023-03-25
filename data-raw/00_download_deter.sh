#!/bin/bash
###############################################################################
# Download DETER data from terrabrailis.
###############################################################################

OUT_DIR="${HOME}/data/deter/amazonia_legal"

[ -d "${OUT_DIR}" ] || { echo "ERROR: Directory ${OUT_DIR} does not exist!" >&2; exit 1; }

# Download files.
wget --content-disposition -nc -t 5 http://terrabrasilis.dpi.inpe.br/file-delivery/download/deter-amz/shape -P "${OUT_DIR}" 

# Extract files.
find "${OUT_DIR}" -type f -iname "*.zip" -exec unzip -o {} -d "${OUT_DIR}" \;

# Remove zip files.
find "${OUT_DIR}" -type f -iname "*.zip" -exec rm {} \;

exit 0
