#!/bin/bash
###############################################################################
# Download MCD64A1 
###############################################################################

# Directory for storing the downloaded files.
OUT_DIR="${HOME}/data/mcd64a1"

# File with Earh Data credentials.
FILE=${HOME}/earthdata_credentials.sh

# Load Earth Data credentials from another file.
if [ -f "$FILE" ]; then
    source ${FILE}
else
    echo "ERROR: File with credentials not found: ${FILE}"
fi
[ -d "${OUT_DIR}" ] || echo "ERROR: Directory ${OUT_DIR} does not exist!"

# Load ED_TOKEN
source "${FILE}"

# NOTE: download only 2020!
URL="https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/6/MCD64A1/2020/"

# Download data
wget -e robots=off -m -np -R .html,.tmp -nH --cut-dirs=3 "${URL}" --header \"Authorization: Bearer $ED_TOKEN\" -P "${OUT_DIR}"

# NOTE: The accept list isn't working! 
# wget -e robots=off -m -np -R .html,.tmp -A '*h10v09*' -nH --cut-dirs=3 "${URL}" --header "Authorization: Bearer $ED_TOKEN" -P "${OUT_DIR}"
# wget -e robots=off -m -np -R .html,.tmp -A '*h11v08*.hdf' -nH --cut-dirs=3 "${URL}" --header "Authorization: Bearer $ED_TOKEN" -P "${OUT_DIR}"
# wget -e robots=off -m -np -R .html,.tmp -A '*h11v09*.hdf' -nH --cut-dirs=3 "${URL}" --header "Authorization: Bearer $ED_TOKEN" -P "${OUT_DIR}"
# wget -e robots=off -m -np -R .html,.tmp -A '*h11v10*.hdf' -nH --cut-dirs=3 "${URL}" --header "Authorization: Bearer $ED_TOKEN" -P "${OUT_DIR}"
# wget -e robots=off -m -np -R .html,.tmp -A '*h12v08*.hdf' -nH --cut-dirs=3 "${URL}" --header "Authorization: Bearer $ED_TOKEN" -P "${OUT_DIR}"
# wget -e robots=off -m -np -R .html,.tmp -A '*h12v09*.hdf' -nH --cut-dirs=3 "${URL}" --header "Authorization: Bearer $ED_TOKEN" -P "${OUT_DIR}"
# wget -e robots=off -m -np -R .html,.tmp -A '*h12v10*.hdf' -nH --cut-dirs=3 "${URL}" --header "Authorization: Bearer $ED_TOKEN" -P "${OUT_DIR}"
# wget -e robots=off -m -np -R .html,.tmp -A '*h13v09*.hdf' -nH --cut-dirs=3 "${URL}" --header "Authorization: Bearer $ED_TOKEN" -P "${OUT_DIR}"

exit 0
