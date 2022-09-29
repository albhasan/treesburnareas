#!/usr/bin/env bash

# Download ESA MODIS Fire_cci Burned Area data - Pixel product (only South 
# America)

out_dir="${HOME}/data/fire_cci"
[ -d "${out_dir}" ] || echo "ERROR: Directory ${out_dir} does not exist!"

#url="https://dap.ceda.ac.uk/neodc/esacci/fire/data/burned_area/MODIS/pixel/v5.1/"

# NOTE: download only 2020!
url="https://dap.ceda.ac.uk/neodc/esacci/fire/data/burned_area/MODIS/pixel/v5.1/compressed/2020/"

wget -r -e robots=off -U mozilla -nH --cut-dirs=4 -P "${out_dir}" -t 5 -A '*AREA_2*.tar.gz' ${url}
wget -r -e robots=off -U mozilla -nH --cut-dirs=4 -P "${out_dir}" -t 5 -A 'README*'         ${url}
wget -r -e robots=off -U mozilla -nH --cut-dirs=4 -P "${out_dir}" -t 5 -A '*.pdf'           ${url}

exit 0
