###-----------------------------------------------------------------------------
### * Set Up

cl <- parallel::makeCluster(parallelly::availableCores(omit = 2))
registerDoParallel(cl)

###-----------------------------------------------------------------------------
### ** Data Frame Methods

###-----------------------------------------------------------------------------
### *** plot_tfr_surfs

test_that("'plot_tfr_surfs.data.frame()' works, no x_alt.", {

    param_df <- make_plot_tfr_surfs_param_df()
    k <- "716"

    results <- foreach(param_i = seq_len(nrow(param_df))) %dopar% {
        library(tfrSURFs)

        ## for (param_i in seq_len(nrow(param_df))) {
        ## cat("\n\ni = ", param_i, "\n\n")

        pars <- param_df[param_i, , drop = FALSE]
        suppressMessages(suppressWarnings(
            plot_tfr_surfs(test_data_tfrSURFs_list[[k]],
                           yvar = pars[["yvar"]],
                           add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                           add_range_regions = pars[["add_range_regions"]],
                           add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                           add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                           maximal_legend = pars[["maximal_legend"]],
                           add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                           add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                           datestamp = pars[["datestamp"]]
                           )))
    }
})


test_that("'plot_tfr_surfs.data.frame()' works, with x_alt.", {

    param_df <- make_plot_tfr_surfs_param_df()
    k <- "716"

    results <- foreach(param_i = seq_len(nrow(param_df))) %dopar% {
        library(tfrSURFs)
        pars <- param_df[param_i, , drop = FALSE]

    plot_tfr_surfs(test_data_tfrSURFs_list[[k]],
                   yvar = pars[["yvar"]],
                   x_alt = test_data_tfrSURFs_median_list[[k]],
                   x_alt_label = "Median only",
                   add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                   add_range_regions = pars[["add_range_regions"]],
                   add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                   add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                   maximal_legend = pars[["maximal_legend"]],
                   add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                   add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                   datestamp = pars[["datestamp"]]
                   )
        }
})

###-----------------------------------------------------------------------------
### *** plot_surfs_probs

test_that("'plot_surfs_probs.data.frame()' works, no x_alt.", {

    param_df <- make_plot_surfs_probs_param_df()
    k <- "716"

    results <- foreach(param_i = seq_len(nrow(param_df))) %dopar% {
        library(tfrSURFs)

        pars <- param_df[param_i, , drop = FALSE]
        suppressMessages(suppressWarnings(
            plot_surfs_probs(test_data_tfrSURFs_list[[k]],
                           yvar = pars[["yvar"]],
                           add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                           add_range_regions = pars[["add_range_regions"]],
                           add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                           add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                           maximal_legend = pars[["maximal_legend"]],
                           add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                           add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                           datestamp = pars[["datestamp"]]
                           )))
    }
})


test_that("'plot_surfs_probs.data.frame()' works, with x_alt.", {

    param_df <- make_plot_surfs_probs_param_df()
    k <- "716"

    results <- foreach(param_i = seq_len(nrow(param_df))) %dopar% {
        library(tfrSURFs)
        pars <- param_df[param_i, , drop = FALSE]

    plot_surfs_probs(test_data_tfrSURFs_list[[k]],
                   yvar = pars[["yvar"]],
                   x_alt = test_data_tfrSURFs_median_list[[k]],
                   x_alt_label = "Median only",
                   add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                   add_range_regions = pars[["add_range_regions"]],
                   add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                   add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                   maximal_legend = pars[["maximal_legend"]],
                   add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                   add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                   datestamp = pars[["datestamp"]]
                   )
        }
})

###-----------------------------------------------------------------------------
### ** List Methods

###-----------------------------------------------------------------------------
### *** plot_tfr_surfs

test_that("'plot_tfr_surfs.list()' works, no x_alt.", {

    param_df <- make_plot_tfr_surfs_param_df()

    results <- foreach(param_i = seq_len(nrow(param_df))) %dopar% {
        library(tfrSURFs)

        pars <- param_df[param_i, , drop = FALSE]
        suppressMessages(suppressWarnings(
            plot_tfr_surfs(test_data_tfrSURFs_list,
                           yvar = pars[["yvar"]],
                           add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                           add_range_regions = pars[["add_range_regions"]],
                           add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                           add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                           maximal_legend = pars[["maximal_legend"]],
                           add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                           add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                           datestamp = pars[["datestamp"]]
                           )))
    }
})


test_that("'plot_tfr_surfs.list()' works, with x_alt.", {

    param_df <- make_plot_tfr_surfs_param_df()
    k <- "716"

    results <- foreach(param_i = seq_len(nrow(param_df))) %dopar% {
        library(tfrSURFs)
        pars <- param_df[param_i, , drop = FALSE]

    plot_tfr_surfs(test_data_tfrSURFs_list,
                   yvar = pars[["yvar"]],
                   x_alt = test_data_tfrSURFs_median_list,
                   x_alt_label = "Median only",
                   add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                   add_range_regions = pars[["add_range_regions"]],
                   add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                   add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                   maximal_legend = pars[["maximal_legend"]],
                   add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                   add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                   datestamp = pars[["datestamp"]]
                   )
        }
})

###-----------------------------------------------------------------------------
### *** plot_surfs_probs

test_that("'plot_surfs_probs.list()' works, no x_alt.", {

    param_df <- make_plot_surfs_probs_param_df()

    results <- foreach(param_i = seq_len(nrow(param_df))) %dopar% {
        library(tfrSURFs)

        pars <- param_df[param_i, , drop = FALSE]
        suppressMessages(suppressWarnings(
            plot_surfs_probs(test_data_tfrSURFs_list,
                           yvar = pars[["yvar"]],
                           add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                           add_range_regions = pars[["add_range_regions"]],
                           add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                           add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                           maximal_legend = pars[["maximal_legend"]],
                           add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                           add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                           datestamp = pars[["datestamp"]]
                           )))
    }
})


test_that("'plot_surfs_probs.list()' works, with x_alt.", {

    param_df <- make_plot_surfs_probs_param_df()

    results <- foreach(param_i = seq_len(nrow(param_df))) %dopar% {
        library(tfrSURFs)
        pars <- param_df[param_i, , drop = FALSE]

    plot_surfs_probs(test_data_tfrSURFs_list,
                   yvar = pars[["yvar"]],
                   x_alt = test_data_tfrSURFs_median_list,
                   x_alt_label = "Median only",
                   add_prob_ref_lines = pars[["add_prob_ref_lines"]],
                   add_range_regions = pars[["add_range_regions"]],
                   add_est_proj_ref_line = pars[["add_est_proj_ref_line"]],
                   add_Schoumaker_stalls = pars[["add_Schoumaker_stalls"]],
                   maximal_legend = pars[["maximal_legend"]],
                   add_prob_TFR_surfs = pars[["add_prob_TFR_surfs"]],
                   add_prob_TFR_surf_projections = pars[["add_prob_TFR_surf_projections"]],
                   datestamp = pars[["datestamp"]]
                   )
        }
})

###-----------------------------------------------------------------------------
### * END

stopCluster(cl)
