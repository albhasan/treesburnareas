#' @title Data Frame of the subareas of deforestation alerts from 2008 to 2021
#'
#' @description A dataset containing a data.table object with no-spatial data
#'   of subareas of DETER's deforestation alerts and PRODES's deforestation
#'   areas. DETER subareas are segments of DETER alerts which have continuity
#'   over time, that is, their shape do not change over time. Capitalized names
#'   are the DETER attributes corresponding to the warnings.
#' @name subarea_dt
#' @docType data
#' @keywords datasets
#' @usage data(subarea_dt)
#' @format A data.table object with 1083335 rows and 23 variables:
#'   cat: Identifier inherited from processing in GRASS GIS,
#'   CLASSNAME: Name of warning or deforestation,
#'   QUADRANT: Out of use. CBERS AWFI quadrant,
#'   PATH_ROW: Path and row of the used satellite images,
#'   VIEW_DATE: Date of the images used to identify a warning,
#'   SENSOR: Name of the sensor which take the images,
#'   SATELLITE: Name of the satellite which took the images,
#'   AREAUCKM: Warning area intesecting a conservation unit,
#'   UC: Name of conservation unit,
#'   AREAMUNKM: Warning area intersecting a municipality,
#'   MUNICIPALI:Name of municipality intersecting a warning,
#'   GEOCODIBGE: IBGE's code for the municipality,
#'   UF: Brazilian state name,
#'   subarea_id: ID of the subarea. It varies along time,
#'   xy_id: Subarea ID based on its centroid coordinates,
#'   subarea_ha: Subarea extent in hectares,
#'   fid_2: Out of use. Feature ID of PRODES mask assigned by QGIS,
#'   DN: Out of use. Attribute name of PRODES mask,
#'   in_prodes: Does the subarea intersect the PRODES mask,
#'   year: PRODES year,
#'   data_source: Source of the data; either DETER or PRODES,
#'   n_warnings_deter: Number of DETER warnings in a subarea,
#'   VIEW_DATE_est: Estimated date of PRODES detection (August of PRODES year).
NULL

#' @title sf object with the subareas of deforestation alerts from 2008 to 2021
#'
#' @description An sf object containing the polygons corresponding to the data
#'   set subarea_dt.
#' @name subarea_sf
#' @docType data
#' @keywords datasets
#' @usage data(subarea_sf)
#' @format A tibble with 807069 rows and 2 variables:
#'   subarea_id: ID of the subarea,
#'   geom: geometry filed.
NULL
