###############################################################################
# Analysis of the recurrence of DETER warnings.
#------------------------------------------------------------------------------
# NOTE:
# - input data is a QGIS geopackage with DETER warnings after running
#   fix_geometries, multi to single part, union, and multi to single part
#    (again) operations.
# TODO:
# - Time to PRODES
# - Run Sankey by state.
# - Convert trajectories back to shapefile.
# - Add Amazonia Legal to maps
# - Download SHP fire calendar from ZENODO by Nathalia Carvalho.
# - Download  GWIS (ask Guilherme).

library(data.table)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggsankey)
library(magrittr)
library(stringr)
library(terra)
library(tibble)
library(treemapify)
library(tidyr)
library(units)



#---- Configuration ----

base_dir <- "~/Documents/trees_lab/deter_warning_recurrence"
out_dir <- file.path(base_dir, "img")
save_figs <- TRUE

deter_lyr <- "deter_public_fix_m2s_union_m2s"
deter_gpk       <- "~/Documents/data/deter/amazonia_legal/deter.gpkg"
prodes_raster   <- "~/Documents/data/prodes/amazonia/prodes_raster.tif"
prodes_viewdate <- "~/Documents/data/prodes/amazonia/prodes_viewdate.tif"

stopifnot("Base directory not found!" = dir.exists(base_dir))
stopifnot("Out directory not found!" = dir.exists(out_dir))
stopifnot("GeoPackage not found!" = file.exists(deter_gpk))
stopifnot("PRODES raster not found! Use script process_prodes.R" =
          file.exists(prodes_raster))
stopifnot("PRODES view dates raster not found! Use script procss_prodes.R" =
          file.exists(prodes_viewdate))

# Categorize the warnings' area.
area_breaks <- c(
    "0"        = 0,
    "6,25 ha"  = 6.25,
    "10 ha"    = 10,
    "25 ha"    = 25,
    "50 ha"    = 50,
    "100 ha"   = 100,
    "250 ha"   = 250,
    "500 ha"   = 500,
    "1000 ha"   = 1000,
    "> 1000 ha" = Inf
)

deter_classes <- c(
    "CICATRIZ_DE_QUEIMADA" = "Burn scar",
    "CORTE_SELETIVO"       = "Selective cut",
    "CS_DESORDENADO"       = "Untidy slash",
    "CS_GEOMETRICO"        = "Geometric slash",
    "DEGRADACAO"           = "Degradation",
    "DESMATAMENTO_CR"      = "Clear cut",
    "DESMATAMENTO_VEG"     = "Slash with veg.",
    "MINERACAO"            = "Mining"
)

# NOTE: Obtained from the text file
#       PDigital2000_2021_AMZ_raster_v20220915_bioma.txt
prodes_classes <- c("54" =  "r2014",
                    "52" =  "r2012",
                    "61" =  "r2021",
                    "53" =  "r2013",
                    "58" =  "r2018",
                    "57" =  "r2017",
                    "51" =  "r2011",
                    "60" =  "r2020",
                    "56" =  "r2016",
                    "50" =  "r2010",
                    "59" =  "r2019",
                    "55" =  "r2015",
                    "12" =  "d2012",
                    "14" =  "d2014",
                    "13" =  "d2013",
                    "11" =  "d2011",
                    "10" =  "d2010",
                     "9" =  "d2009",
                    "15" =  "d2015",
                    "17" =  "d2017",
                    "18" =  "d2018",
                    "16" =  "d2016",
                    "20" =  "d2020",
                    "19" =  "d2019",
                    "21" =  "d2021",
                     "8" =  "d2008",
                    "91" =  "P_Water",        # Hidrografia
                    "32" =  "P_Clouds",       # Nuvem
                   "101" =  "P_Non-forest",   # NaoFloresta
                     "7" =  "d2007 (mask)", # d2007 (mascara)
                   "100" =  "P_Forest")       # Floresta

# NOTE: These are temporal files.
subarea_prodes_file <- file.path(out_dir, "subarea_prodes.rds")
subarea_prodes_date_file <- file.path(out_dir, "subarea_prodes_date.rds")

source("./deter_warnings_recurrence_util.R")

sf::sf_use_s2(FALSE)



#---- Load data ----

# NOTE:
# - deter_id identifies the original DETER features.
# - subarea is what we call each of the subareas of each DETER warning which
#   overlaps with some other subarea of other warnings.
# - subarea_id identifies features afer applying QGIS' union operation and
#   pre-processing.

# Keep the geometry separated form the data handle it as a data.table.
subarea_sf <-
    deter_gpk %>%
    sf::read_sf(layer = deter_lyr) %>%
    dplyr::mutate( area_ha = units::drop_units(sf::st_area(.) * 0.0001)) %>%
    dplyr::filter(!is.na(area_ha),
                  area_ha > 0) %>%
    sf::st_cast(to = "POLYGON")

subarea_dt <-
    subarea_sf %>%
    sf::st_drop_geometry() %>%
    data.table::setDT(key = c("subarea_id", "VIEW_DATE"))

subarea_sf <- subarea_sf %>%
    dplyr::select(subarea_id)

rm(deter_lyr)
rm(deter_gpk)

# Build an ID based on coordinates.
# Recode DETER classes.
# Compute PRODES year.
# Filter out DETER warnigs before 2022.
# Sort by subarea and date.
subarea_dt <-
    subarea_dt %>%
    dplyr::mutate(
        x = round(x, digits = 10),
        y = round(y, digits = 10),
        xy_id = stringr::str_c(x, y, sep = ";"),
        CLASSNAME = dplyr::recode(CLASSNAME, !!!deter_classes,
                                  .default = NA_character_,
                                  .missing = NA_character_),
        CLASSNAME = forcats::fct_relevel(CLASSNAME, sort),
        start_date = as.Date(paste(year(VIEW_DATE), "08", "01", sep = "-")),
        year = dplyr::if_else(VIEW_DATE > start_date,
                              as.double(year(start_date)) + 1,
                              as.double(year(start_date))),
        year = as.integer(year)) %>%
    dplyr::select(-start_date, -x, -y) %>%
    dplyr::filter(year < 2022) %>%
    dplyr::arrange(xy_id, VIEW_DATE)

# TODO: check
subarea_dt %>% filter(xy_id == "-51.7596975442;-11.9129786069")


#---- Prepare plot data ----

# Get unique DETER subareas ids (ids of poligons without overlap).
subarea_unique_tb <-
    subarea_dt %>%
    dplyr::select(xy_id, subarea_id) %>%
    dplyr::group_by(xy_id) %>%
    dplyr::summarize(n_warnings_deter = dplyr::n(),
                     subarea_id = dplyr::first(subarea_id)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    tibble::as_tibble()

subarea_unique_sf  <- merge(subarea_sf,
                            subarea_unique_tb,
                            by = "subarea_id",
                            all.y = TRUE)

stopifnot("NAs in the number of deter warnings found" =
          sum(is.na(subarea_unique_sf$n_warnings_deter)) == 0)
stopifnot("Expected more subareas than unique subareas" =
          nrow(subarea_sf) >= nrow(subarea_unique_tb))

# Get the PRODES' class mode in each subarea.
if (file.exists(subarea_prodes_file)) {
    subarea_prodes <- readRDS(subarea_prodes_file)
} else {
    # NOTE: This takes long!
    subarea_prodes <-
        terra::extract(x = terra::rast(prodes_raster),
                       y = terra::vect(subarea_unique_sf["xy_id"]),
                       fun = the_mode,
                       ID = TRUE,
                       weights = FALSE,
                       exact = FALSE,
                       touches = FALSE,
                       bind = FALSE,
                       raw = FALSE)
    saveRDS(subarea_prodes, file = subarea_prodes_file)
}

stopifnot("Some PRODES data is missing" =
          nrow(subarea_prodes) == nrow(subarea_unique_sf))

# Get the PRODES' view date mode in each subarea.
if (file.exists(subarea_prodes_date_file)) {
    subarea_prodes_date <- readRDS(subarea_prodes_date_file)
} else {
    # NOTE: This takes long!
    subarea_prodes_date <-
        terra::extract(x = terra::rast(prodes_viewdate),
                       y = terra::vect(subarea_unique_sf["xy_id"]),
                       fun = the_mode,
                       ID = TRUE,
                       weights = FALSE,
                       exact = FALSE,
                       touches = FALSE,
                       bind = FALSE,
                       raw = FALSE)
    saveRDS(subarea_prodes_date, file = subarea_prodes_date_file)
}

stopifnot("Some PRODES VIEWDATE data is missing" =
          nrow(subarea_prodes_date) == nrow(subarea_unique_sf))

sf::write_sf(subarea_unique_sf,
             dsn = file.path(out_dir, "deter_warning_recurrence.gpkg"),
             layer = "subarea_unique_sf")

rm(subarea_unique_sf)
rm(subarea_prodes_file)
rm(subarea_prodes_date_file)
rm(prodes_raster)
rm(prodes_viewdate)

# Recode subarea_prodes' PRODES codes to PRODES years.
subarea_prodes <-
    subarea_prodes %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(c("row_id", "prodes_code")) %>%
    dplyr::mutate(row_id = as.integer(row_id),
                  prodes_code = as.integer(prodes_code),
                  prodes_name = dplyr::recode(prodes_code,
                                              !!!prodes_classes, # )) %>%
                                              .default = NA_character_,
                                              .missing = NA_character_))

# Recode subarea_prodes_date
subarea_prodes_date <-
    subarea_prodes_date %>%
    tibble::as_tibble() %>%
    magrittr::set_colnames(c("row_id", "prodes_date")) %>%
    dplyr::mutate(row_id = as.integer(row_id),
                  prodes_date = as.Date(prodes_date,
                                        origin = "1970-01-01"))

stopifnot(nrow(subarea_prodes) == nrow(subarea_unique_tb))
stopifnot(nrow(subarea_prodes_date) == nrow(subarea_prodes))

# Join PRODES class and viewdate.
# Add the PRODES date estimated from CLASSNAME.
subarea_prodes <-
    subarea_prodes %>%
    dplyr::left_join(subarea_prodes_date, by = "row_id") %>%
    dplyr::right_join(subarea_unique_tb, by = "row_id") %>%
    dplyr::select(-row_id, -subarea_id, -prodes_code) %>%
    dplyr::rename(CLASSNAME = prodes_name,
                  VIEW_DATE = prodes_date) %>%
    dplyr::mutate(data_source = "PRODES",
                  VIEW_DATE_est = dplyr::if_else(
                      stringr::str_starts(CLASSNAME, "d"),
                      stringr::str_c(stringr::str_sub(CLASSNAME, 2, 5),
                                     "-08-01"),
                      NA_character_),
                  VIEW_DATE_est = lubridate::as_date(VIEW_DATE_est)) %>%
    dplyr::filter(!is.na(CLASSNAME),
                  !is.na(VIEW_DATE))

stopifnot("Unique xy_ids expected" =
          length(unique(subarea_prodes[["xy_id"]])) == nrow(subarea_prodes))

rm(subarea_unique_tb)
rm(subarea_prodes_date)


# Difference between theoretical and actual VIEW_DATE of deforestation.
# NOTE: The median is at most 14 days off from the theoretical PRODES VIEW_DATE.
#       However the SD is up to 241 days, the minimum is 4759 days, and the
#       maximum up to 732 days.
# NOTE: Don't use the VIEW_DATE. Use the theoretical PRODES data of
#       deforestation.
subarea_prodes %>%
    dplyr::filter(stringr::str_starts(CLASSNAME, "d")) %>%
    dplyr::mutate(
        view_diff = difftime(VIEW_DATE_est, VIEW_DATE, units = "days")
    ) %>%
    group_by(CLASSNAME) %>%
    summarize(min = min(view_diff),
              mean = mean(view_diff),
              median = stats::median(view_diff),
              max = max(view_diff),
              sd = sd(view_diff)) %>%
    knitr::kable()


# Join subarea_prodes to DETER subareas.

subarea_prodes <-
    data.table::setDT(subarea_prodes,
                      key = "xy_id")

prodes_ids <-
    subarea_prodes %>%
    pull(xy_id)

subarea_dt <-
    subarea_dt %>%
    dplyr::mutate(data_source = "DETER") %>%
    tibble::as_tibble() %>%
    dplyr::bind_rows(subarea_prodes) %>%
    dplyr::filter(xy_id %in% prodes_ids)

rm(prodes_ids)
rm(subarea_prodes)

subarea_dt <-
    data.table::setDT(subarea_dt,
                      key = "xy_id")



#---- Treemap: DETER warning area by state, year, warning type and area ----

plot_area_by_state_year_type <- get_plot_area_by_state_year_type(subarea_dt)

if (save_figs) {
    ggplot2::ggsave(
        plot = plot_area_by_state_year_type,
        filename = file.path(out_dir,
                             "plot_deter_area_by_state_pyear_type.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
} else {
    plot_area_by_state_year_type +
        ggplot2::ggtitle(paste("Area of DETER warnings by state, year",
                               "(PRODES), and type"))
}

rm(plot_area_by_state_year_type)



#---- Point density: DETER subareas by state, year, warning type and area ----

# NOTE: This plot shows uses the CLASSNAME of the first DETER warning.
plot_density_area_ndays <- get_plot_density_area_ndays(subarea_dt)

if (save_figs) {
    ggplot2::ggsave(
        plot = plot_density_area_ndays,
        filename = file.path(out_dir,
              "plot_deter_subarea_density_by_state_first-type_nwarnings.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
} else {
     plot_density_area_ndays +
        ggplot2::ggtitle(paste("DETER area by subarea, state, first type, and",
                               "number of days between warnings"))
}

rm(plot_density_area_ndays)



#---- Histogram DETER subareas by number of warnings ----

plot_area_by_warnings <- get_plot_area_by_warnings(subarea_dt)

if (save_figs) {
    ggplot2::ggsave(
        plot = plot_area_by_warnings,
        filename = file.path(out_dir, "plot_deter_subarea_by_nwarnings.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
} else {
    plot_area_by_warnings +
        ggplot2::ggtitle(paste("DETER area by subarea and number of",
                               "warnings in the Brazilian Amazon"))
}

rm(plot_area_by_warnings)



#---- Histogram DETER subareas by number of warnings by state ----

plot_area_by_warnings_state <- get_plot_area_by_warnings_state(subarea_dt)

if (save_figs) {
    ggplot2::ggsave(
        plot = plot_area_by_warnings_state,
        filename = file.path(out_dir, "plot_area_by_warnings_state.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
} else {
    plot_area_by_warnings_state +
        ggplot2::ggtitle(paste("Area of DETER warnings by number of warnings",
                               "and state"))
}

rm(plot_area_by_warnings_state)



#---- Boxplot days between warnings by subarea ----

plot_days_first_to_last <- get_plot_days_first_to_last(subarea_dt)

if (save_figs) {
    ggplot2::ggsave(
        plot =  plot_days_first_to_last,
        filename = file.path(out_dir, "plot_days_first_to_last.png"),
        height = 210,
        width = 297,
        units = "mm"
    )
} else {
    plot_days_first_to_last +
        ggplot2::ggtitle(paste("Number of days from the first to the last",
                               "DETER warnings",
                               "Amazon by Brazilian state"))
}

rm(plot_days_first_to_last)



#---- Sankey: Trajectories of subareas ----

plot_tb <-
    subarea_dt %>%
    dplyr::filter(area_ha > 3 | is.na(area_ha),
                  !is.na(CLASSNAME)) %>%
    dplyr::arrange(xy_id, VIEW_DATE_est) %>%
    dplyr::group_by(xy_id) %>%
    dplyr::mutate(CLASSNAME = dplyr::if_else(
                      stringr::str_starts(CLASSNAME, "d"),
                      "P_deforestation",
                      CLASSNAME
                  ),
                  CLASSNAME = dplyr::if_else(
                      stringr::str_starts(CLASSNAME, "r"),
                      "P_residue",
                      CLASSNAME
                  ),
                  last_CLASSNAME = dplyr::lag(CLASSNAME),
                  last_VIEW_DATE_est = dplyr::lag(VIEW_DATE_est),
                  diff_days = as.vector(difftime(VIEW_DATE_est,
                                                 last_VIEW_DATE_est,
                                                 units = "days")),
                  n_warn_p = dplyr::n()) %>%
    tidyr::fill(area_ha, .direction = "downup") %>%
    dplyr::ungroup() %>%
    dplyr::select(area_ha, diff_days,
                  CLASSNAME, last_CLASSNAME,
                  xy_id, n_warn_p, data_source)  %>%
    #dplyr::filter(diff_days > 0 | is.na(diff_days)) %>%
    data.table::as.data.table()


# TODO: Make sankey use the variable area_ha
for (i in sort(unique(plot_tb$n_warn_p))) {
    if (i == 1)
        next

    my_plot <-
        plot_tb %>%
        dplyr::filter(n_warn_p == i) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(group_row = dplyr::row_number(),
                      subarea_step = stringr::str_c("step_", group_row)) %>%
        dplyr::ungroup() %>%
        dplyr::select(xy_id, CLASSNAME, subarea_step, area_ha, n_warn_p) %>%
        tidyr::pivot_wider(names_from = subarea_step,
                           values_from = CLASSNAME) %>%
        dplyr::select_if(~!all(is.na(.))) %>%
        tidyr::drop_na() %>%
        dplyr::as_tibble() %>%
        ggsankey::make_long(tidyselect::starts_with("step_")) %>%
        ggplot2::ggplot(ggplot2::aes(x = x,
                                     next_x = next_x,
                                     node = node,
                                     next_node = next_node,
                                     fill = factor(node),
                                     label = node)) +
        ggsankey::geom_sankey() +
        ggsankey::theme_sankey(base_size = 16) +
        ggsankey::geom_sankey(flow.alpha = .6, node.color = "gray30") +
        ggsankey::geom_sankey_label() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(x = NULL)

    if (save_figs) {
        ggplot2::ggsave(
            plot = my_plot,
            filename = file.path(out_dir,
                         paste0("plot_subarea_trajectory_", i, ".png")),
            height = 210,
            width = 297,
            units = "mm"
        )
    }else{
         my_plot +
            ggplot2::ggtitle("Trajectory of subareas")
    }
}


rm(my_plot)
rm(plot_tb)

