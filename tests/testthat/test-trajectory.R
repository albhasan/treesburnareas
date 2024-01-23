test_that("filter_trajectory works", {

    data_tb <- treesburnareasdata::subarea_tb[1:1000,]
    test_tb <- 
        filter_trajectory(
            data_tb = data_tb,
            filter_col = "CLASSNAME",
            filter_val = "MINERACAO",
            invert = FALSE
        )

    expect_true(ncol(data_tb) == ncol(test_tb))
    expect_true(nrow(data_tb) >= nrow(test_tb))
    expect_true(
        all(unique(test_tb[["xy_id"]] %in% unique(data_tb[["xy_id"]])))
    )

    test_inv_tb <- 
        filter_trajectory(
            data_tb = data_tb,
            filter_col = "CLASSNAME",
            filter_val = "MINERACAO",
            invert = TRUE
        )

    expect_true(nrow(data_tb) == nrow(test_tb) + nrow(test_inv_tb))
    expect_true(sum(test_inv_tb[["CLASSNAME"]] == "MINERACAO") == 0)
})

