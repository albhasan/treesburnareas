###############################################################################
# Analysis of the recurrence of DETER warnings.
#------------------------------------------------------------------------------

create_plots <- function(out_dir, save_figs = TRUE) {

    # create_plots(out_dir = "~/Documents/trees_lab/deter_warning_recurrence/img",
    #              save_figs = TRUE)

    CLASSNAME <- data_source <- diff_days <- in_prodes <- NULL
    last_CLASSNAME <- last_VIEW_DATE_est <- n_warn_p <- subarea_ha <- NULL
    VIEW_DATE_est <- xy_id <- NULL

    if (save_figs)
        stopifnot("Directory not found!" = dir.exists(out_dir))

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


    #---- Treemap: DETER warning area by state, year, warning type and area----

    plot_area_by_state_year_type <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE) %>%
        get_plot_area_by_state_year_type()
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


    #---- Point density: DETER subareas by state, year, warning type and area

    # NOTE: This plot shows uses the CLASSNAME of the first DETER warning.
    plot_density_area_ndays <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE) %>%
        get_plot_density_area_ndays()
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
            ggplot2::ggtitle(
                paste("DETER area by subarea, state, first type,",
                      "and number of days between warnings"))
    }


    #---- Histogram DETER subareas by number of warnings ----

    plot_area_by_warnings <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE) %>%
        get_plot_area_by_warnings(area_breaks)
    if (save_figs) {
        ggplot2::ggsave(
            plot = plot_area_by_warnings,
            filename = file.path(out_dir,
                                 "plot_deter_subarea_by_nwarnings.png"),
            height = 210,
            width = 297,
            units = "mm"
        )
    } else {
        plot_area_by_warnings +
            ggplot2::ggtitle(paste("DETER area by subarea and number of",
                                   "warnings in the Brazilian Amazon"))
    }


    #---- Histogram DETER subareas by number of warnings by state ----

    plot_area_by_warnings_state <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE) %>%
        get_plot_area_by_warnings_state(area_breaks)
    if (save_figs) {
        ggplot2::ggsave(
            plot = plot_area_by_warnings_state,
            filename = file.path(out_dir,
                                 "plot_deter_subarea_by_warnings_state.png"),
            height = 210,
            width = 297,
            units = "mm"
        )
    } else {
        plot_area_by_warnings_state +
            ggplot2::ggtitle(
                paste("Area of DETER warnings by",
                      "number of warnings and state"))
    }

    #---- Boxplot days between warnings by subarea ----
    plot_days_first_to_last <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE) %>%
        get_plot_days_first_to_last(area_breaks)
    if (save_figs) {
        ggplot2::ggsave(
            plot =  plot_days_first_to_last,
            filename = file.path(out_dir, "plot_deter_days_first_to_last.png"),
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

    #---- Sankey: Trajectories of subareas (only DETER) ----

    plot_tb <-
        treesburnareas::subarea_dt %>%
        dplyr::filter(data_source == "DETER",
                      in_prodes == TRUE,
                      subarea_ha > 3 | is.na(subarea_ha),
                      !is.na(CLASSNAME)) %>%
        dplyr::arrange(xy_id, VIEW_DATE_est) %>%
        dplyr::group_by(xy_id) %>%
        dplyr::mutate(last_CLASSNAME = dplyr::lag(CLASSNAME),
                      last_VIEW_DATE_est = dplyr::lag(VIEW_DATE_est),
                      diff_days = as.vector(difftime(VIEW_DATE_est,
                                                     last_VIEW_DATE_est,
                                                     units = "days")),
                      n_warn_p = dplyr::n()) %>%
        tidyr::fill(subarea_ha, .direction = "downup") %>%
        dplyr::ungroup() %>%
        dplyr::select(subarea_ha, diff_days,
                      CLASSNAME, last_CLASSNAME,
                      xy_id, n_warn_p, data_source)  %>%
        #dplyr::filter(diff_days > 0 | is.na(diff_days)) %>%
        data.table::as.data.table()

    # NOTE: I couldn't make ggsankey::geom_sankey use the variable subarea_ha.
    for (i in sort(unique(plot_tb$n_warn_p))) {
        if (i == 1)
            next
        my_plot <- get_plot_sankey(data_tb = plot_tb, n_warnings = i)
        if (save_figs) {
            ggplot2::ggsave(
                plot = my_plot,
                filename = file.path(
                    out_dir,
                    paste0("plot_deter_subarea_trajectory_", i, ".png")
                ),
                height = 210,
                width = 297,
                units = "mm"
            )
        } else {
             my_plot +
                ggplot2::ggtitle("Trajectory of subareas")
        }
    }


    rm(my_plot)
    rm(plot_tb)



# TODO:
    #---- Sankey: Trajectories of subareas (DETER & PRODES) ----



}
