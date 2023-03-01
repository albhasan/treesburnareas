#!/bin/bash
###############################################################################
# PRE-PROCESS DETER DATA USING GRASS GIS 8.2.0.
#------------------------------------------------------------------------------
# NOTE: 
#------------------------------------------------------------------------------
# TODO: 
# - add missing columns: 
#   deter_id: Enumeration of the original DETER polygons. NOTE: not needed.
#   biome:    Biome's name. NOTE: It isn't needed.
###############################################################################


#---- Setup ----

# Path to the DETER file downloaded from TERRABRASILIS
DETER_SHP="/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp"

# Path to GRASS GIS database.
GRASS_DATA="/home/alber/Documents/grassdata"

# Path to PRODES' vector mask.
PRODES_GPKG="/home/alber/data/prodes/prodes_mask.gpkg"
PRODES_MASK="prodes_mask"


#---- Utilitary functions ----

is_file_valid () {
    if [ -f "$1" ]; then
        echo "INFO: File found: $1"
    else
        echo "ERROR: Missing file: $1"
        exit 1
    fi
}

is_dir_valid () {
    if [ -d "$1" ]; then
        echo "INFO: Directory found: $GRASS_DATA" 
    else
        echo "ERROR: Missing directory: $GRASS_DATA"
        exit 1
    fi
}


#---- Validation ----

if command -v grass &> /dev/null; then
        echo "INFO: grass gis found!"
    else
        echo "ERROR: grass gis could not be found. Please install it."
        exit 1
fi

is_file_valid $DETER_SHP
is_file_valid $PRODES_GPKG
is_dir_valid $GRASS_DATA


#---- Import data to GRASS GIS ----

# Create a GRASS location using DETER properties.
grass -e -c ${DETER_SHP} ${GRASS_DATA}/deter

# Import DETER data to GRASS GIS. GRASS cleans and intersects the polygons.
# NOTE: The snap argument was taken from GRASS suggestion during the first 
#       import. See below.
grass ${GRASS_DATA}/deter/PERMANENT --exec v.import input=${DETER_SHP} output=deter_public snap=1e-06

# Import PRODES mask.
grass ${GRASS_DATA}/deter/PERMANENT --exec v.import input=${PRODES_GPKG} layer=${PRODES_MASK} output=prodes_mask


#---- Compute additonal fields ----

# Add subarea_id: Enumerate each polygon. 
grass ${GRASS_DATA}/deter/PERMANENT --exec v.db.addcolumn deter_public columns="subarea_id int"
grass ${GRASS_DATA}/deter/PERMANENT --exec v.db.update deter_public col=subarea_id qcol=cat

# Add xy_id. xy_id is created by combining into a string the centroids' 
# coordinates up to 6 decimal places.
grass ${GRASS_DATA}/deter/PERMANENT --exec v.to.db map=deter_public type=centroid option=coor columns="x,y"
grass ${GRASS_DATA}/deter/PERMANENT --exec v.db.update deter_public col=x qcol="round(x, 6)"
grass ${GRASS_DATA}/deter/PERMANENT --exec v.db.update deter_public col=y qcol="round(y, 6)"
grass ${GRASS_DATA}/deter/PERMANENT --exec v.db.addcolumn deter_public columns="xy_id varchar"
grass ${GRASS_DATA}/deter/PERMANENT --exec v.db.update deter_public col=xy_id qcol="x || ';' || y"
grass ${GRASS_DATA}/deter/PERMANENT --exec v.db.dropcolumn deter_public column=x
grass ${GRASS_DATA}/deter/PERMANENT --exec v.db.dropcolumn deter_public column=y

# Add area column
grass ${GRASS_DATA}/deter/PERMANENT --exec v.to.db map=deter_public option=area type=boundary columns=subarea_ha units=hectares

# Identify DETER polygons whose centroids fall in the Amazon Biome.
grass ${GRASS_DATA}/deter/PERMANENT --exec v.extract input=deter_public type=centroid output=deter_cent
grass ${GRASS_DATA}/deter/PERMANENT --exec v.type input=deter_cent from_type=centroid output=deter_point to_type=point
grass ${GRASS_DATA}/deter/PERMANENT --exec g.remove -f type=vector name=deter_cent
grass ${GRASS_DATA}/deter/PERMANENT --exec v.db.addcolumn map=deter_point column="biome_amazon INT"
grass ${GRASS_DATA}/deter/PERMANENT --exec v.distance from=deter_point to=prodes_mask to_column=DN upload=to_attr column=biome_amazon dmax=0
grass ${GRASS_DATA}/deter/PERMANENT --exec v.db.join map=deter_public column=xy_id other_table=deter_point other_column=xy_id subset_columns=xy_id,biome_amazon

# Export the results.
grass ${GRASS_DATA}/deter/PERMANENT --exec v.out.ogr -a input=deter_public type=area format=GPKG output=~/Documents/data/deter/amazonia_legal/deter_grass.gpkg output_layer=deter_public

exit 0

###############################################################################
# First  GRASS import, no snap
###############################################################################
# WARNING: The output contains topological errors:
#          Unable to calculate a centroid for 28413 areas
#                   Number of incorrect boundaries: 1335
#                            Number of duplicate centroids: 1
#                            The input could be cleaned by snapping vertices to each other.
#                            Estimated range of snapping threshold: [1e-13, 1e-05]
#                            Try to import again, snapping with 1e-09: 'snap=1e-09'
#                            Input </home/alber/Documents/data/deter/amazonia_legal/deter_public.shp>
#                            successfully imported without reprojection
#                            Execution of <v.import input=/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp output=deter_public> finished.
#                            Cleaning up default sqlite database ...
#                            Cleaning up temporary files...
#
###############################################################################
# Second GRASS import, snap=1e-09 (~ 1/10 of a milimeter).
###############################################################################
# 213390 areas represent multiple (overlapping) features, because polygons
# overlap in input layer(s). Such areas are linked to more than 1 row in
# attribute table. The number of features for those areas is stored as
# category in layer 2
# -----------------------------------------------------
# If overlapping is not desired, the input data can be cleaned by snapping
# vertices to each other.
# Estimated range of snapping threshold: [1e-13, 1e-05]
# Try to import again, snapping with 1e-08: 'snap=1e-08'
# Input </home/alber/Documents/data/deter/amazonia_legal/deter_public.shp>
# successfully imported without reprojection
# Execution of <v.import input=/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp output=deter_public snap=1e-09 --overwrite> finished.
# Cleaning up default sqlite database ...
# Cleaning up temporary files...
#
###############################################################################
# Third GRASS import, snap=1e-08 (~ 1 milimeter).
###############################################################################
# 204181 areas represent multiple (overlapping) features, because polygons
# overlap in input layer(s). Such areas are linked to more than 1 row in
# attribute table. The number of features for those areas is stored as
# category in layer 2
# -----------------------------------------------------
# If overlapping is not desired, the input data can be cleaned by snapping
# vertices to each other.
# Estimated range of snapping threshold: [1e-13, 1e-05]
# Try to import again, snapping with 1e-07: 'snap=1e-07'
# Input </home/alber/Documents/data/deter/amazonia_legal/deter_public.shp>
# successfully imported without reprojection
# Execution of <v.import input=/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp output=deter_public snap=1e-08 --overwrite> finished.
# Cleaning up default sqlite database ...
# Cleaning up temporary files...
#
###############################################################################
# Fourth GRASS import, snap=1e-07 (~ 1 centimeter).
############################################################################### 
# 204089 areas represent multiple (overlapping) features, because polygons
# overlap in input layer(s). Such areas are linked to more than 1 row in
# attribute table. The number of features for those areas is stored as
# category in layer 2
# -----------------------------------------------------
# If overlapping is not desired, the input data can be cleaned by snapping
# vertices to each other.
# Estimated range of snapping threshold: [1e-13, 1e-05]
# Try to import again, snapping with 1e-06: 'snap=1e-06'
# Input </home/alber/Documents/data/deter/amazonia_legal/deter_public.shp>
# successfully imported without reprojection
# Execution of <v.import input=/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp output=deter_public snap=1e-07 --overwrite> finished.
# Cleaning up default sqlite database ...
# Cleaning up temporary files...
#
###############################################################################
# Fifth GRASS import, snap=1e-06 (~ 10 centimeters).
############################################################################### 
# 203662 areas represent multiple (overlapping) features, because polygons
# overlap in input layer(s). Such areas are linked to more than 1 row in
# attribute table. The number of features for those areas is stored as
# category in layer 2
# -----------------------------------------------------
# If overlapping is not desired, the input data can be cleaned by snapping
# vertices to each other.
# Estimated range of snapping threshold: [1e-13, 1e-05]
# Try to import again, snapping with 1e-05: 'snap=1e-05'
# Input </home/alber/Documents/data/deter/amazonia_legal/deter_public.shp>
# successfully imported without reprojection
# Execution of <v.import input=/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp output=deter_public snap=1e-06 --overwrite> finished.
# Cleaning up default sqlite database ...
# Cleaning up temporary files...
#
###############################################################################
# Sixth GRASS import, snap=1e-05 (~ 1 meter).
############################################################################### 
# 203122 areas represent multiple (overlapping) features, because polygons
# overlap in input layer(s). Such areas are linked to more than 1 row in
# attribute table. The number of features for those areas is stored as
# category in layer 2
# -----------------------------------------------------
# If overlapping is not desired, the input data can be cleaned by snapping
# vertices to each other.
# Manual cleaning may be needed.
# Input </home/alber/Documents/data/deter/amazonia_legal/deter_public.shp>
# successfully imported without reprojection
# Execution of <v.import input=/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp output=deter_public snap=1e-05 --overwrite> finished.
# Cleaning up default sqlite database ...
# Cleaning up temporary files...
#
###############################################################################
# Seventh GRASS import, snap=1e-04 (~ 10 meters).
############################################################################### 
# 197545 areas represent multiple (overlapping) features, because polygons
# overlap in input layer(s). Such areas are linked to more than 1 row in
# attribute table. The number of features for those areas is stored as
# category in layer 2
# -----------------------------------------------------
# If overlapping is not desired, the input data can be cleaned by snapping
# vertices to each other.
# Manual cleaning may be needed.
# Input </home/alber/Documents/data/deter/amazonia_legal/deter_public.shp>
# successfully imported without reprojection
# Execution of <v.import input=/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp output=deter_public snap=1e-04 --overwrite> finished.
# Cleaning up default sqlite database ...
# Cleaning up temporary files...
