#!/bin/bash

# Download GFW Global Forest Loss due to Fire

out_dir="${HOME}/data/gfw_forest_loss_fire"
[ -d "${out_dir}" ] || echo "ERROR: Directory ${out_dir} does not exist!"

url="https://glad.umd.edu/users/Alexandra/Fire_GFL_data/2001-21/"

wget -r -e robots=off -U mozilla -nH --cut-dirs=4 -P "${out_dir}" -t 5 -A 'LAM_fire_forest_loss_*.tif' ${url}

exit 0
