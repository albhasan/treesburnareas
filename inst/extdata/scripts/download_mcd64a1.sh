#!/bin/bash
###############################################################################
# Download MCD64A1 collection 61 data from USGS site.
#------------------------------------------------------------------------------
# NOTE: This script downloads collection 61. For dowloding collection 6 use
#       instead script 'download_mcd61a.lftp'.
# TODO: This script isn't downloaind all the tiles. Is collection 61 ready?
###############################################################################
echo "DEPRECATED: Use instead 'download_mcd61a.lftp'"
exit 1

 Bash script with Earh Data credentials.
FILE=${HOME}/earthdata_credentials.sh

# Directory for storing the downloaded files.
OUT_DIR="${HOME}/data/mcd64a1"

# URL for downloading data. 
URL='https://e4ftl01.cr.usgs.gov//DP131/MOTA/MCD64A1.061/'

# Additional parameters for WGET.
PARAMS='--no-clobber --no-host-directories --cut-dirs=3 --wait=1 --random-wait --waitretry=5 --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --no-check-certificate --auth-no-challenge=on -r --reject "index.html*" --no-parent -e robots=off'

# Load Earth Data credentials from another file.
if [ -f "$FILE" ]; then
    source ${HOME}/earthdata_credentials.sh
else
    echo "ERROR: File with credentials not found: ${HOME}"
fi
[ -d "${OUT_DIR}" ] || echo "ERROR: Directory ${OUT_DIR} does not exist!"

# Download the data (Brazilian Amazon).
wget --http-user="${ED_USER}" --http-password="${ED_PASSWD}" -P "${OUT_DIR}" -A '*h10v09*.hdf' -t 5 ${PARAMS} ${URL}
wget --http-user="${ED_USER}" --http-password="${ED_PASSWD}" -P "${OUT_DIR}" -A '*h11v08*.hdf' -t 5 ${PARAMS} ${URL}
wget --http-user="${ED_USER}" --http-password="${ED_PASSWD}" -P "${OUT_DIR}" -A '*h11v09*.hdf' -t 5 ${PARAMS} ${URL}
wget --http-user="${ED_USER}" --http-password="${ED_PASSWD}" -P "${OUT_DIR}" -A '*h11v10*.hdf' -t 5 ${PARAMS} ${URL}
wget --http-user="${ED_USER}" --http-password="${ED_PASSWD}" -P "${OUT_DIR}" -A '*h12v08*.hdf' -t 5 ${PARAMS} ${URL}
wget --http-user="${ED_USER}" --http-password="${ED_PASSWD}" -P "${OUT_DIR}" -A '*h12v09*.hdf' -t 5 ${PARAMS} ${URL}
wget --http-user="${ED_USER}" --http-password="${ED_PASSWD}" -P "${OUT_DIR}" -A '*h12v10*.hdf' -t 5 ${PARAMS} ${URL}
wget --http-user="${ED_USER}" --http-password="${ED_PASSWD}" -P "${OUT_DIR}" -A '*h13v09*.hdf' -t 5 ${PARAMS} ${URL}

exit 0
