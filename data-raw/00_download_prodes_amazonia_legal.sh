#!/bin/bash
###############################################################################
# Download PRODES data from terrabrailis.
###############################################################################

OUT_DIR="${HOME}/data/prodes/amazonia_legal"

[ -d "${OUT_DIR}" ] || { echo "ERROR: Directory ${OUT_DIR} does not exist!" >&2; exit 1; }

# Download files.
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
wget -nc -t 5 -i "${SCRIPT_DIR}"/urls_prodes_amazonia_legal.txt -P "${OUT_DIR}" 
# Extract files.
find "${OUT_DIR}" -type f -iname "*.zip" -exec unzip -o {} -d "${OUT_DIR}" \;

# Remove zip files.
find "${OUT_DIR}" -type f -iname "*.zip" -exec rm {} \;

exit 0
