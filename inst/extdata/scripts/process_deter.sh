#!/bin/bash

###############################################################################
# PRE-PROCESS DETER DATA USING GRASS GIS 8.2.0.
#------------------------------------------------------------------------------
# NOTE: 
# - Create the GPKG deter_grass.gpkg before calling this script.
# - Run this script before running deter_warning_recurrence.R
#------------------------------------------------------------------------------
# TODO: 
# - Create the missing fields in the table.
# - Update other scripts using the old GPKG created with QGIS with the call to
#   the GPKG created in this script.
###############################################################################


export GRASS_DATA="/home/alber/Documents/grassdata"
export DETER_SHP="/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp"

# Create a GRASS location using DETER properties.
grass -e -c ${DETER_SHP} ${GRASS_DATA}/deter

# Import DETER data to GRASS GIS. GRASS cleans and intersects the polygons.
grass ${GRASS_DATA}/deter/PERMANENT --exec v.import input=${DETER_SHP} output=deter_public snap=1e-09


# NOTE: The snap argument was taken from GRASS suggestion during the first 
#       import.

###############################################################################
# second import, snap=1e-09
###############################################################################
# 151139 areas represent multiple (overlapping) features, because polygons
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
# Execution of <v.import input=/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp output=deter_public snap=1e-09> finished.
# Cleaning up default sqlite database ...
# Cleaning up temporary files...

###############################################################################
# First import, no snap
###############################################################################
# WARNING: Number of incorrect boundaries: 1040
# WARNING: Number of duplicate centroids: 2
# -----------------------------------------------------
# WARNING: The output contains topological errors:
#          Unable to calculate a centroid for 26071 areas
#                   Number of incorrect boundaries: 1040
#                            Number of duplicate centroids: 2
#                            The input could be cleaned by snapping vertices to each other.
#                            Estimated range of snapping threshold: [1e-13, 1e-05]
#                            Try to import again, snapping with 1e-09: 'snap=1e-09'
#                            Input </home/alber/Documents/data/deter/amazonia_legal/deter_public.shp>
#                            successfully imported without reprojection
#                            Execution of <v.import input=/home/alber/Documents/data/deter/amazonia_legal/deter_public.shp output=deter_public> finished.
#                            Cleaning up default sqlite database ...
#                            Cleaning up temporary files...
#                            a

# Add subarea_id: Enumerate each polygon. 
grass ${GRASS_DATA}/deter/PERMANENT --exec db.execute sql="ALTER TABLE deter_public ADD COLUMN subarea_id integer"
grass ${GRASS_DATA}/deter/PERMANENT --exec db.execute sql="UPDATE deter_public SET subarea_id=cat"

# TODO:
# - add the missing columns: 
#   x, y: Centroid coordinates
#   xy_id: Unique id created by combining x and y up to 10 decimal values.
#   legal_amazon: Does the polygon intersect the legal amazon? Use PRODES's
#                 raster mask.
#
#   deter_id: Enumeration of the original DETER polygons. NOTE: It isn't needed
#   subarea_area: Area of each polygon.  NOTE: It isn't needed.
#   biome: Biome's name. NOTE: It isn't needed.

# Export the results.
grass ${GRASS_DATA}/deter/PERMANENT --exec v.out.ogr -a input=deter_public type=area format=GPKG output=~/Documents/data/deter/amazonia_legal/deter_grass.gpkg output_layer=deter_public


exit 0
