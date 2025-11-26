
###-----------------------------------------------------------------------------
### * Data Frames

###-----------------------------------------------------------------------------
### ** plot_tfr_surfs

test_that("'plot_tfr_surfs.data.frame()' works.", {

    msg <- TRUE
    param_df <- make_plot_tfr_surfs_param_df()

    if (requireNamespace("parallelly", quietly = TRUE)) {
        ncores <- parallelly::availableCores(omit = 2)
    } else {
        ncores <- parallel::detectCores() - 2
    }
    cl <- parallel::makeCluster(ncores)
    #parallel::clusterExport(cl, varlist = c("param_df"))

    res <- parallel::parLapply(cl = cl,
                               setNames(nm = seq_len(nrow(param_df))),
                               fun = function(param_i, param_df) {
        library(testthat)
        library(tfrSURFs)
        withr::local_options(list(tfrSURFs.message_about_unknown_aes = FALSE))
        data(test_data_tfrSURFs_list)

        pars <- param_df[param_i, , drop = FALSE]
        k <- "716"

        list(args = paste("[", param_i, " of ", nrow(param_df), "]: ",
                          paste(colnames(param_df), paste0(pars, "; "), sep = " = ")),
             country_code = paste0("Country code = '", k, "'"),
             plot_prob_gg = try(suppressWarnings(
                plot_tfr_surfs(test_data_tfrSURFs_list[[k]],
                                xvar = pars[["xvar"]],
                                yvar = pars[["yvar"]],
                                add_range_regions = pars[["add_range_regions"]],
                                add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                use_ggpattern = pars[["use_ggpattern"]]
                                ))),
             plot_med_gg = try(suppressWarnings(
                plot_tfr_surfs(test_data_tfrSURFs_list[[k]],
                                xvar = pars[["xvar"]],
                                yvar = pars[["yvar"]],
                                x_alt = test_data_tfrSURFs_median_list[[k]],
                                x_alt_label = "Median only",
                                add_range_regions = pars[["add_range_regions"]],
                                add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                use_ggpattern = pars[["use_ggpattern"]]
                                )))
             )
    }, param_df = param_df)
    parallel::stopCluster(cl)

    for (ll in res) {
        if (msg) {
            message(ll[["args"]])
            message("Country code: '", ll[["country_code"]])
            message("Probabilistic...")
        }
        expect_s3_class(ll[["plot_prob_gg"]], "ggplot")
        if (msg) message("Medians...")
        expect_s3_class(ll[["plot_med_gg"]], "ggplot")
    }
})

###-----------------------------------------------------------------------------
### ** plot_surfs_probs

test_that("'plot_surfs_probs.data.frame()' works.", {

    msg <- TRUE
    withr::local_options(list(tfrSURFs.message_about_unknown_aes = FALSE))
    data(test_data_tfrSURFs_list)
    param_df <- make_plot_tfr_surfs_param_df()

    for (param_i in seq_len(nrow(param_df))) {
        pars <- param_df[param_i, , drop = FALSE]
        if (msg) message("[", param_i, " of ", nrow(param_df), "]: ",
                         paste(colnames(param_df), paste0(pars, "; "), sep = " = "))
        for (k in "716"## names(test_data_tfrSURFs_list)
             ) {
            if (msg) message("Country code = '", k, "'")
            if (msg) message("No 'x_alt'")
            expect_s3_class(suppressWarnings(plot_surfs_probs(test_data_tfrSURFs_list[[k]],
                                                               add_range_regions = pars[["add_range_regions"]],
                                                               add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                                               add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                                               use_ggpattern = pars[["use_ggpattern"]]
                                                               )),
                            "ggplot")


            if (msg) message("'x_alt' = medians")
            expect_s3_class(suppressWarnings(plot_surfs_probs(test_data_tfrSURFs_list[[k]],
                                                               x_alt = test_data_tfrSURFs_median_list[[k]],
                                                               x_alt_label = "Median only",
                                                               add_range_regions = pars[["add_range_regions"]],
                                                               add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                                               add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                                               use_ggpattern = pars[["use_ggpattern"]]
                                                               )),
                            "ggplot")
        }
    }
})


###-----------------------------------------------------------------------------
### * Lists

###-----------------------------------------------------------------------------
### ** plot_tfr_surfs

test_that("'plot_tfr_surfs.list()' works.", {

    msg <- TRUE
    withr::local_options(list(tfrSURFs.message_about_unknown_aes = FALSE))
    data(test_data_tfrSURFs_list)
    param_df <- make_plot_tfr_surfs_param_df()

    for (param_i in seq_len(nrow(param_df))) {
        pars <- param_df[param_i, , drop = FALSE]
        if (msg) message("[", param_i, " of ", nrow(param_df), "]: ",
                         paste(colnames(param_df), paste0(pars, "; "), sep = " = "))
        if (msg) message("No 'x_alt'")
        out <- suppressWarnings(plot_tfr_surfs(test_data_tfrSURFs_list,
                                                xvar = pars[["xvar"]],
                                                yvar = pars[["yvar"]],
                                                add_range_regions = pars[["add_range_regions"]],
                                                add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                                add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                                use_ggpattern = pars[["use_ggpattern"]],
                                                incl_small_countries = pars[["incl_small_countries"]]
                                                ))
        for (k in names(out)) {
            if (msg) message("Country code = '", k, "'")
            expect_s3_class(out[[k]], "ggplot")
        }

        if (msg) message("'x_alt' = medians")
        out <- suppressWarnings(plot_tfr_surfs(test_data_tfrSURFs_list,
                                                x_alt = test_data_tfrSURFs_median_list,
                                                x_alt_label = "Median only",
                                                xvar = pars[["xvar"]],
                                                yvar = pars[["yvar"]],
                                                add_range_regions = pars[["add_range_regions"]],
                                                add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                                add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                                use_ggpattern = pars[["use_ggpattern"]],
                                                incl_small_countries = pars[["incl_small_countries"]]
                                                ))
        for (k in names(out)) {
            if (msg) message("Country code = '", k, "'")
            expect_s3_class(out[[k]], "ggplot")
        }
    }
})


###-----------------------------------------------------------------------------
### ** plot_surfs_probs

test_that("'plot_surfs_probs.list()' with no 'alt' surfs to plot works.", {

    msg <- TRUE
    withr::local_options(list(tfrSURFs.message_about_unknown_aes = FALSE))
    data(test_data_tfrSURFs_list)
    param_df <- make_plot_tfr_surfs_param_df()

    for (param_i in seq_len(nrow(param_df))) {
        pars <- param_df[param_i, , drop = FALSE]
        if (msg) message("[", param_i, " of ", nrow(param_df), "]: ",
                         paste(colnames(param_df), paste0(pars, "; "), sep = " = "))
        if (msg) message("No 'x_alt'")
        out <- suppressWarnings(plot_surfs_probs(test_data_tfrSURFs_list,
                                                  add_range_regions = pars[["add_range_regions"]],
                                                  add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                                  add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                                  use_ggpattern = pars[["use_ggpattern"]],
                                                  incl_small_countries = pars[["incl_small_countries"]]
                                                  ))
        for (k in names(out)) {
            if (msg) message("Country code = '", k, "'")
            expect_s3_class(out[[k]], "ggplot")
        }


        if (msg) message("'x_alt' = Medians")
        out <- suppressWarnings(plot_surfs_probs(test_data_tfrSURFs_list,
                                                  x_alt = test_data_tfrSURFs_median_list,
                                                  x_alt_label = "Median only",
                                                  add_range_regions = pars[["add_range_regions"]],
                                                  add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                                                  add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                                                  use_ggpattern = pars[["use_ggpattern"]],
                                                  incl_small_countries = pars[["incl_small_countries"]]
                                                  ))
        for (k in names(out)) {
            if (msg) message("Country code = '", k, "'")
            expect_s3_class(out[[k]], "ggplot")
        }
    }
})
