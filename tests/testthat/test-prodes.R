test_that("compute_prodes_year works", {
    test_dates <- c("2008-08-01", "2008-08-02", "2008-12-31", "2008-01-01",
                    "2008-07-31")
    exp_years <- c(2009, 2009, 2009, 2008, 2008)
    expect_equal(
        compute_prodes_year(as.Date(test_dates)),
        exp_years
    )
})

test_that("get_prodes_codes works", {
    data_tb <- get_prodes_codes()
    expect_equal(dim(data_tb), expected = c(32, 4))
    expect_equal(colnames(data_tb),
                 expected = c("prodes_code", "common_name",
                              "class_tif_2021",  "class_shp_2021"))
    last_prodes_year <- compute_prodes_year(Sys.Date()) - 1
    if(!(paste0("d", last_prodes_year) %in% data_tb[[3]]))
       warning("The codes in get_prodes_codes (raster) could be outdated!")
    if(!(paste0("d", last_prodes_year) %in% data_tb[[4]]))
       warning("The codes in get_prodes_codes (vector) could be outdated!")
})

test_that("get_prodes_names works", {
    expect_true(length(get_prodes_names()) > 36)
    expect_true(is.character(get_prodes_names()))
})
