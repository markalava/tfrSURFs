################################################################################
###
### Plot functions, EXPORTED
###
################################################################################


###-----------------------------------------------------------------------------
### * Utilities

##' Add textual annotation to a ggplot object
##'
##' Uses \code{\link[ggpubr]{annotate_figure}} to add \code{plot_ann} to the
##' plot \code{gp}.
##'
##' @param gp \pkg{ggplot2} object
##' @param plot_ann Character: annotation to add.
##' @return \pkg{ggplot2} object with annotation added.
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
add_plot_ann <- function(gp, plot_ann) {
    ggpubr::annotate_figure(p = gp, fig.lab = paste0(plot_ann),
                                fig.lab.pos = "top.right",
                                fig.lab.size = 10)
}


##' Add datetime and package version to ggplot object
##'
##' Uses \code{\link[ggpubr]{annotate_figure}} to add the datetime and package
##' version to the plot.
##'
##' @param gp \pkg{ggplot2} object
##' @return \pkg{ggplot2} object with annotation added.
##' @author Mark C Wheldon
##' @keywords internal
##' @noRd
add_plot_datestamp <- function(gp) {
    ggpubr::annotate_figure(p = gp,
                            fig.lab = paste0(format(Sys.time(), "%Y-%m-%d %H:%M"),
                                           "\ntfrSURFs, version ",
                                           packageVersion("tfrSURFs")),
                            fig.lab.pos = "bottom.right",
                            fig.lab.size = 7)
}


###-----------------------------------------------------------------------------
### * Main Plot Functions


##' Plot TFR surfs
##'
##' Uses \pkg{ggplot2} to generate line plots showing time periods where TFR
##' surfs were detected. If \code{yvar = "surf_prob"}, the surf probabilities
##' are plotted; otherwise the total fertility rates (TFRs) are plotted.
##'
##' @param x Output of \code{\link{make_tfr_surfs}}.
##' @param x_alt Alternative results to compare with \code{x} by adding
##'     \code{\link[=ggplot2::geom_rug]{rugs}} to the plot. Structure must match
##'     \code{x}, i.e., if \code{x} is a \code{data.frame}, \code{x_alt} must be
##'     one too, if \code{x} is a list, \code{x_alt} must be a list of the same
##'     length and with the same names.
##' @param xvar,yvar Variables to plot on the x- and y-axes.
##' @param x_alt_label Label for legend if \code{x_alt} is not \code{NULL}.
##' @param add_range_ref_lines Logical; add reference lines for the range
##'     condition?
##' @param add_range_regions Logical; add shading to show periods outside the
##'     range condition?
##' @param add_est_proj_ref_line Logical; add a vertical reference line marking
##'     the first projection year? Ignored if \code{xvar} \code{!=}
##'     \code{"year"}.
##' @param add_Schoumaker_stalls Logical; add shading to show where stalls were
##'     identified by \cite{Schoumaker (2019)}.
##' @param maximal_legend Logical; show all SURF and Schoumaker stall
##'     types in the legend, even if only a subset actually occur?
##' @param add_prob_TFR_surfs Logical; add shading to show where probabilistic
##'     surfs were identified
##' @param add_prob_TFR_surf_projections Logical; \emph{if}
##'     \code{add_prob_TFR_surfs} is \code{TRUE}, should the shading extend into
##'     the projection period?
##' @param xlim_plot,ylim_plot Limits for x- and y-axes.
##' @param plot_ann Character; text string to add to plot in top-right corner.
##' @param datestamp Logical; add datetime and package version in small text in
##'     bottom left corner?
##' @return A \pkg{ggplot2} object, which will be displayed by default.
##' @author Mark Wheldon
##'
##' @family Plotting functions
##'
##' @seealso The \pkg{FPPlateaus} package (https://github.com/markalava/FPPlateaus).
##'
##' @references
##'
##' Schoumaker, B. (2019), \dQuote{Stalls in Fertility Transitions in
##' sub-Saharan Africa: Revisiting the Evidence}, \emph{Studies in Family Planning},
##' 50, 257–278. \doi{10/gf5zpt}.
##'
##' @export
plot_tfr_surfs <- function(x, ...) {
    UseMethod("plot_tfr_surfs")
}

##' @rdname plot_tfr_surfs
##' @export
plot_tfr_surfs.list <- function(x, x_alt = NULL, ..., incl_small_countries = TRUE,
                                 plot = TRUE, verbose = FALSE) {
    stopifnot(is.logical(incl_small_countries))
    if (!incl_small_countries) x <- remove_small_countries(x)
    if (!is.null(x_alt)) {
        if (!identical(length(x_alt), length(x)))
            stop("'x' and 'x_alt' are different lengths.")
        else if (!identical(sort(names(x_alt)), sort(names(x))))
            stop("'x' and 'x_alt' have different names")
        else x_alt <- x_alt[names(x)]
    }

    if (!verbose)
        withr::local_options(list(tfrSURFs.message_about_unknown_aes = FALSE))

    out <- list()
    for (i in seq_along(x)) {
        out <- c(out, setNames(list(plot_tfr_surfs(x = x[[i]], x_alt = x_alt[[i]], ...)),
                               nm = names(x)[i]))
        if (plot) print(out[[i]])
    }
    return(invisible(out))
}

##' @rdname plot_tfr_surfs
##' @export
plot_tfr_surfs.data.frame <- function(x,
                                       x_alt = NULL,
                                       xvar = c("year", "TFR_median"),
                                       yvar = c("surf_prob", "TFR_median"),
                                       x_alt_label = "Non-\nprobabilistic",
                                       add_range_ref_lines = identical(yvar, "surf_prob"),
                                       add_range_regions = TRUE,
                                       add_est_proj_ref_line = TRUE,
                                      add_Schoumaker_stalls = isTRUE(any(x[["Schoumaker_stall_any"]])),
                                      maximal_legend = FALSE,
                                      add_prob_TFR_surfs = isTRUE(any(x[["surf_year"]])),
                                      add_prob_TFR_surf_projections = FALSE,
                                       xlim_plot = if (identical(xvar, "year")) { c(1950, 2050) } else c(0, 10),
                                       ylim_plot = NULL,
                                       plot_ann = NULL,
                                       datestamp = FALSE) {

    ## -------* Sub Functions

    ## Make data frame for schoumaker stalls to be fed to geom_poly().
    make_surf_geom_poly_df <- function(x, surf_label) {
        idx <- which(!is.na(x[, surf_label]) & x[, surf_label])
        if (length(idx)) {
            return(data.frame(x[idx, ],
                              id = as.character(paste(x[idx, "indicator"],
                                                      number_sequence_groups(x[idx, "year"])))) |>
                   plyr::ddply(.variables = c("indicator", xvar, "id"), .fun = function(z) {
                       data.frame(value = 1,
                                  x = c(z[,xvar] - rep(jitt_1, 2), z[,xvar] + rep(jitt_2, 2)),
                                  y = ylim_plot[c(1,2,2,1)])
                   }) |>
                   plyr::ddply(.variables = "id", function(z) {
                       z <- z[z$x %in% c(min(z[["x"]]), max(z[["x"]])), ]
                       return(z)
                   }))
        } else {
            return(data.frame(id = character(), indicator = character(),
                              value = numeric(), x = numeric(), y = numeric()))
        }
    }

    ## -------* Checks

    if (is.null(x[["country_code"]])) stop("'country_code' is not a column in 'x'.")
    if (!identical(length(unique(x[["country_code"]])), 1L))
        stop("'x' must have exactly one country.")

    xvar <- match.arg(xvar)
    yvar <- match.arg(yvar)

    stopifnot(is.logical(add_range_regions))
    stopifnot(is.logical(add_est_proj_ref_line))
    stopifnot(is.logical(add_Schoumaker_stalls))
    stopifnot(is.logical(add_prob_TFR_surfs))
    stopifnot(is.logical(add_prob_TFR_surf_projections))
    stopifnot(is.logical(maximal_legend))

    stopifnot(is.logical(datestamp))

    ## -------* Constants

    ## Some of these were arguments in earlier versions, but should now just be
    ## fixed. Keep in alphabetical order.

    add_source_data <- FALSE
    est_proj_ref_line_year <- x[1, "bayesTFR_present_year"]
    rate_prob_threshold <- x[1, "rate_prob_threshold"]

    ## SURFs and Stalls only plotted if they are present
    add_Schoumaker_stalls <- add_Schoumaker_stalls && isTRUE(any(x[["Schoumaker_stall_any"]]))
    add_prob_TFR_surfs <- add_prob_TFR_surfs && isTRUE(any(x[["surf_year"]]))

    ## -------* Create Dataframe(s) and Plot Annotations

    ## -------** Set up

    ## -------** Annotations

    indicator_abbrev <- "Probabilistic"
    indicator_abbrev_proj <- indicator_abbrev #paste0(indicator_abbrev, " (projected)")
    indicator_not_in_range_abbrev <- "TFR"
    legend_title <- "Legend"
    if (add_Schoumaker_stalls || maximal_legend) {
        legend_title <- paste0(legend_title, " — ", "SURF / Stall Type")
    } else if (add_prob_TFR_surfs) {
        legend_title <- paste0(legend_title, " — ", "SURF Type")
    }
    line_colour <- "black"
    probability_scale <- "percent"
    ribbon_fill <- line_colour

    cname <- x[1, "name"]
    rname <- x[1, "reg_name"]

    TFR_range_condition_min <- min(x[x[["transition_condition_met"]], "year"], na.rm = TRUE)
    TFR_range_condition_max <- max(x[x[["transition_condition_met"]], "year"], na.rm = TRUE)

    if (xvar == "year") {
        jitt_1 <- 0.5
        jitt_2 <- 0.5
    } else jitt_1 <- jitt_2 <- 0.005

    if (is.null(ylim_plot)) {
        if (identical(yvar, "annual_change_50%")) {
            ylim_plot <- c(-10, 10)
            yscale_breaks <- seq(from = ylim_plot[1], to = ylim_plot[2], by = 4)
        } else if (identical(yvar, "surf_prob") && identical(probability_scale, "prop")) {
            ylim_plot <- c(0, 1)
            yscale_breaks <- seq(from = ylim_plot[1], to = ylim_plot[2], by = 0.2)
        } else if (identical(yvar, "surf_prob") && identical(probability_scale, "percent")) {
            ylim_plot <- c(0, 100)
            yscale_breaks <- seq(from = ylim_plot[1], to = ylim_plot[2], by = 20)
        } else if (yvar %in% c("TFR_median")) {
            yvar_indicator <- gsub("_median", "", yvar)
            ylim_plot <- c(0, ceiling(max(c(x[[paste0(yvar_indicator, ".97.5pc")]]) * 1.05, 10)))
            if (is.na(ylim_plot[2])) ylim_plot[2] <- 10
            yscale_breaks <- seq(from = ylim_plot[1], to = ylim_plot[2], by = 1)
        } else {
            ylim_plot <- c(0, 100)
            yscale_breaks <- seq(from = ylim_plot[1], to = ylim_plot[2], by = 20)
        }
    }

    ## y-axis scale (percentage or proportion)
    if (identical(yvar, "surf_prob")) {
        if (identical(probability_scale, "percent")) {
            x[[yvar]] <- 100 * x[[yvar]]
        }
    }

    ## y-axis labels
    if (identical(yvar, "surf_prob")) {
        if (identical(probability_scale, "percent")) {
            y_axis_label <- "SURF probability (%)"
        } else if (identical(probability_scale, "prop")) {
            y_axis_label <- "SURF probability"
        }
    } else if (identical(yvar, "TFR_median")) {
        y_axis_label <- "Children per woman"
    } else if (identical(yvar, "annual_change_50%")) {
        y_axis_label <- "%-age points"
    }

    ## -------** Create Stall Period Dataframes

    ## NOTE: This section uses `plyr::ddply()`. Reason (1): Earlier versions
    ## were designed to work with input that had multiple indicators but the
    ## current version works only with `indicator == "TFR"`. Note that `xvar`
    ## must be of length 1. Reason (2): new, and differing numbers of, rows are
    ## created for `x` and `y`. Without `plyr::ddply()` this would require some
    ## messy base R solution.

    surf_df <- x[!is.na(x[,"surf_year"]) & x[,"surf_year"],]
    surf_df <- surf_df |>
        plyr::ddply(.variables = c("indicator", xvar), .fun = function(z) {
            data.frame(id = as.character(paste(z[,xvar], z[,"indicator"], z[, "surf_year_group"])),
                       value = 1,
                       x = c(z[,xvar] - rep(jitt_1, 2)
                           , z[,xvar] + rep(jitt_2, 2)
                             ),
                       y2 = ylim_plot[c(1,2,2,1)]
                       )
        })

    stall_tfr_df <- make_surf_geom_poly_df(x, surf_label = "Schoumaker_stall_strong")

    stall_tfr_moderate_df <- make_surf_geom_poly_df(x, surf_label = "Schoumaker_stall_moderate")

    stall_tfr_weak_df <- make_surf_geom_poly_df(x, surf_label = "Schoumaker_stall_weak")

    stall_tfr_pretransitional_df <- make_surf_geom_poly_df(x, surf_label = "Schoumaker_stall_pretransitional")

    surf_dummy_df <- data.frame(indicator = unique(x[["indicator"]]),
                                 x = median(x[, xvar]), y = mean(x[, yvar]),
                                 y2 = mean(x[, yvar]),
                                 id = number_sequence_groups(x[, "year"]))

    indicator_not_in_range_df <-
        make_surf_geom_poly_df(data.frame(x[, c("indicator", xvar)],
                                               transition_condition_not_met = !x$transition_condition_met),
                                    surf_label = "transition_condition_not_met")

    ## -------* Build Plot

    ## -------** Base plot

    gp <- ggplot2::ggplot(data = x, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]))

    ## -------** Reference lines

    if (add_range_ref_lines) {
        if (yvar %in% c("TFR_median")) {
            if (identical(yvar, "TFR_median")) {
                ref_line_df_1 <- data.frame(indicator = "TFR",
                                            xintercept = TFR_range_condition_min)
                ref_line_df_2 <- data.frame(indicator = "TFR",
                                            xintercept = TFR_range_condition_max)
            }

            gp <- gp +
                ggplot2::geom_hline(data = ref_line_df_1, ggplot2::aes(yintercept = yintercept), linetype = 2, col = "blue") +
                ggplot2::geom_hline(data = ref_line_df_2, ggplot2::aes(yintercept = yintercept), linetype = 2, col = "blue")

        } else if (identical(yvar, "surf_prob")) {

            if (identical(probability_scale, "percent")) rate_prob_threshold <- 100 * rate_prob_threshold
            ref_line_df <-
                data.frame(indicator = "TFR", yintercept = rate_prob_threshold)

            if (nrow(ref_line_df)) {
                gp <- gp +
                    ggplot2::geom_hline(data = ref_line_df, ggplot2::aes(yintercept = yintercept), linetype = 2, col = "blue")
            }

        }
    }

    ## -------** Fill scales

    fill_alpha <- 0.3
    gp <- gp +
        ggplot2::theme_bw() +
        ggplot2::scale_y_continuous(limits = ylim_plot, breaks = yscale_breaks) +
        ggplot2::scale_fill_manual(breaks = c("Outside fertility\ntransition period", # to get the ordering as listed
                                              x_alt_label,
                                              indicator_abbrev,
                                              indicator_abbrev_proj,
                                              "Schoumaker:\nStrong+\nevidence",
                                              "Schoumaker:\nModerate\nevidence",
                                              "Schoumaker:\nWeak\nevidence",
                                              "Schoumaker:\nPre-\ntransitional"),
                                   values = setNames(c("grey75",
                                                       NA,#"grey30",
                                                       "orange4",
                                                       "orange4",
                                                       RColorBrewer::brewer.pal(n = 11, "Spectral")[11],
                                                       RColorBrewer::brewer.pal(n = 11, "Spectral")[10],
                                                       RColorBrewer::brewer.pal(n = 11, "Spectral")[8],
                                                       NA
                                                       ),
                                                     c("Outside fertility\ntransition period",
                                                       x_alt_label,
                                                       indicator_abbrev,
                                                       indicator_abbrev_proj,
                                                       "Schoumaker:\nStrong+\nevidence",
                                                       "Schoumaker:\nModerate\nevidence",
                                                       "Schoumaker:\nWeak\nevidence",
                                                       "Schoumaker:\nPre-\ntransitional")),
                                   na.value = NA,
                                   name = legend_title) +
        ggplot2::scale_colour_manual(breaks = c("Outside fertility\ntransition period", # to get the ordering as listed
                                                x_alt_label,
                                                indicator_abbrev,
                                                indicator_abbrev_proj,
                                                "Schoumaker:\nStrong+\nevidence",
                                                "Schoumaker:\nModerate\nevidence",
                                                "Schoumaker:\nWeak\nevidence",
                                                "Schoumaker:\nPre-\ntransitional"),
                                     values = setNames(c(NA,
                                                         "grey30",
                                                         NA,
                                                         NA,
                                                         NA,
                                                         NA,
                                                         NA,
                                                         "pink2"## RColorBrewer::brewer.pal(n = 11, "RdBu")[8]
                                                         ),
                                                       c("Outside fertility\ntransition period",
                                                         x_alt_label,
                                                         indicator_abbrev,
                                                         indicator_abbrev_proj,
                                                         "Schoumaker:\nStrong+\nevidence",
                                                         "Schoumaker:\nModerate\nevidence",
                                                         "Schoumaker:\nWeak\nevidence",
                                                         "Schoumaker:\nPre-\ntransitional")),
                                     na.value = NA,
                                     name = legend_title)  +
        ggplot2::labs(title = paste0(cname, " (", rname, ")"),
                      x = xvar, y = y_axis_label) +
        ggplot2::theme(legend.position = "bottom", legend.title.align = 0.5) +
        ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", nrow = 1))

    if (is.numeric(xlim_plot)) {
        if (identical(xvar, "year")) {
            xyear_breaks <- seq(from = xlim_plot[1], to = xlim_plot[2], by = 10)
            gp <- gp +
                ggplot2::scale_x_continuous(limits = c(xlim_plot[1] - 0.5, xlim_plot[2] + 0.5),
                                            breaks = xyear_breaks, labels = xyear_breaks)
        } else {
            gp <- gp + ggplot2::xlim(xlim_plot)
        }
    }

    ## -------** Transition condition

    if (add_range_regions || maximal_legend) {

        if (nrow(indicator_not_in_range_df)) {
                gp <- gp + ggplot2::geom_polygon(data = indicator_not_in_range_df,
                                                 ggplot2::aes(x = x, y = y, group = id,
                                                              fill = "Outside fertility\ntransition period",
                                                              colour = "Outside fertility\ntransition period"),
                                                 alpha = fill_alpha
                                                 )
        }
    }

    ## -------** Schoumaker Stalls

    if (add_Schoumaker_stalls || maximal_legend) {

        ## TFR moderate stalls
        if (!nrow(stall_tfr_moderate_df) && maximal_legend) {
            stall_tfr_moderate_df <- surf_dummy_df
        }
        if (nrow(stall_tfr_df)) {
            gp <- gp + ggplot2::geom_polygon(data = stall_tfr_moderate_df,
                                                 ggplot2::aes(x = x, y = y, group = id,
                                                              fill = "Schoumaker:\nModerate\nevidence",
                                                              colour = "Schoumaker:\nModerate\nevidence"),
                                                 alpha = fill_alpha
                                                 )
        }

        ## TFR stalls
        if (!nrow(stall_tfr_df) && maximal_legend) {
            stall_tfr_df <- surf_dummy_df
        }
        if (nrow(stall_tfr_df)) {
            gp <- gp + ggplot2::geom_polygon(data = stall_tfr_df,
                                                 ggplot2::aes(x = x, y = y, group = id,
                                                              fill = "Schoumaker:\nStrong+\nevidence",
                                                              colour = "Schoumaker:\nStrong+\nevidence"),
                                                 alpha = fill_alpha
                                                 )
        }

        ## TFR weak stalls
        if (!nrow(stall_tfr_weak_df) && maximal_legend) {
            stall_tfr_weak_df <- surf_dummy_df
        }
        if (nrow(stall_tfr_weak_df)) {
        gp <- gp + ggplot2::geom_polygon(data = stall_tfr_weak_df,
                                             ggplot2::aes(x = x, y = y, group = id,
                                                          fill = "Schoumaker:\nWeak\nevidence",
                                                          colour = "Schoumaker:\nWeak\nevidence"),
                                             alpha = fill_alpha
                                             )
        }

        ## TFR pretransitional stalls
        ## These are open boxes, not shaded regions.
        if (!nrow(stall_tfr_pretransitional_df) && maximal_legend) {
            stall_tfr_pretransitional_df <- surf_dummy_df
        }
        if (nrow(stall_tfr_pretransitional_df)) {
        gp <- gp + ggplot2::geom_polygon(data = stall_tfr_pretransitional_df,
                                             ggplot2::aes(x = x, y = y, group = id,
                                                          fill = "Schoumaker:\nPre-\ntransitional",
                                                          colour = "Schoumaker:\nPre-\ntransitional"),
                                             alpha = fill_alpha,
                                             linewidth = 0.75)
        }
    }

    ## -------** Probabilistic Stalls

    if (add_prob_TFR_surfs || maximal_legend) {

        surf_est_df <- surf_df[surf_df[["year"]] <= est_proj_ref_line_year, ]
        surf_proj_df <- surf_df[surf_df[["year"]] > est_proj_ref_line_year, ]

        if (nrow(surf_est_df)) {
            surf_est_df <- plyr::ddply(surf_est_df, .variables = "id", function(z) {
                z <- z[z$x %in% c(min(z[["x"]]), max(z[["x"]])), ]
                return(z)
            })
        } else if (maximal_legend) {
            surf_est_df <- surf_dummy_df
        }
        if (nrow(surf_proj_df)) {
            surf_proj_df <- plyr::ddply(surf_proj_df, .variables = "id", function(z) {
                z <- z[z$x %in% c(min(z[["x"]]), max(z[["x"]])), ]
                return(z)
            })
        } else  if (maximal_legend) {
            surf_proj_df <- surf_dummy_df
        }
        if (nrow(surf_est_df)) {
                gp <- gp + ggplot2::geom_polygon(data =  surf_est_df,
                                                 ggplot2::aes(x = x, y = y2, group = id,
                                                              colour = indicator_abbrev,
                                                              fill = indicator_abbrev),
                                                 alpha = fill_alpha,
                                                 linewidth = 1.05
                                                 )
            }
            if (add_prob_TFR_surf_projections) {
                if (add_prob_TFR_surf_projections) {
                    gp <- gp + ggplot2::geom_polygon(data =  surf_proj_df,
                                                     ggplot2::aes(x = x, y = y2, group = id,
                                                                  colour = indicator_abbrev_proj,
                                                                  fill = indicator_abbrev_proj),
                                                     alpha = fill_alpha,
                                                     linewidth = 1.05
                                                     )
                }
            }
    }

    ## -------** Alternative Stalls to Plot

    if (!is.null(x_alt) || maximal_legend) {

        if (isTRUE(any(x_alt[["surf_year"]])) || isTRUE(maximal_legend)) {

            ## -------*** Make Dataframe

            surf_df_ALT <-
                x_alt[!is.na(x_alt[,"surf_year"]) & x_alt[,"surf_year"],]
            surf_df_ALT <- surf_df_ALT |>
                plyr::ddply(.variables = c("indicator", xvar, "surf_year_group"),
                            .fun = function(z) {
                                data.frame(id = as.character(paste0(z[,xvar], z[,"indicator"], " (", x_alt_label, ")")),
                                           label = x_alt_label,
                                           value = 1,
                                           x = z[,xvar],
                                           ## x = c(z[,xvar] - rep(jitt_1, 2), z[,xvar] + rep(jitt_2, 2)),
                                           y2 = c(-0.5, grid::unit(0.03, "npc"), grid::unit(0.03, "npc"), -0.5)
                                           )
                            })

            surf_dummy_df_ALT <- data.frame(indicator = unique(x_alt[["indicator"]]),
                                            x = -median(x_alt[, xvar], na.rm = TRUE),
                                            y = -mean(x_alt[, yvar], na.rm = TRUE),
                                            y2 = -mean(x_alt[, yvar], na.rm = TRUE),
                                            id = 1)
                                # ^^ Set the x, y, y2 to negative values to try
                                # and stop them from being shown.

            if (nrow(surf_df_ALT)) {
                x_alt_fp_surf_ALT <- surf_df_ALT[surf_df_ALT[["year"]] <= est_proj_ref_line_year, ]
                x_alt_fp_surf_proj_ALT <- surf_df_ALT[surf_df_ALT[["year"]] > est_proj_ref_line_year, ]
            } else {
                x_alt_fp_surf_ALT <- x_alt_fp_surf_proj_ALT <- surf_dummy_df_ALT
            }

            ## -------*** Add Layer to Plot

            ## Rug
            if (!getOption("tfrSURFs.message_about_unknown_aes")) {
                gp <- suppressWarnings({
                    gp +
                        ggplot2::geom_rug(data = x_alt_fp_surf_ALT,
                                          ggplot2::aes(x = x, y = NULL, group = id,
                                                       colour = x_alt_label,
                                                       fill = x_alt_label
                                # ^^ that has to
                                # be there to get
                                # the legend to
                                # merge
                                # properly, but
                                # it will cause
                                # a warning.
                                                       ),
                                          linewidth = 1.1)
                })
                if (add_prob_TFR_surf_projections) {
                    gp <- suppressWarnings({
                        gp +
                            ggplot2::geom_rug(data = x_alt_fp_surf_proj_ALT,
                                              ggplot2::aes(x = x, y = NULL, group = id,
                                                           colour = x_alt_label,
                                                           fill = x_alt_label
                                # ^^ that has to
                                # be there to get
                                # the legend to
                                # merge
                                # properly, but
                                # it will cause
                                # a warning.
                                                           ),
                                              linewidth = 1.1)
                    })
                }
            } else {
                on.exit(message("NOTE: You can ignore the warning from ggplot2 about \"unknown aesthetics: fill\"."),
                        add = TRUE, after = FALSE)
                gp <- gp +
                    ggplot2::geom_rug(data = x_alt_fp_surf_ALT,
                                      ggplot2::aes(x = x, y = NULL, group = id,
                                                   colour = x_alt_label,
                                                   fill = x_alt_label
                                # ^^ that has to
                                # be there to get
                                # the legend to
                                # merge
                                # properly, but
                                # it will cause
                                # a warning.
                                                   ),
                                      linewidth = 1.1)
                if (add_prob_TFR_surf_projections) {
                    gp <- gp +
                        ggplot2::geom_rug(data = x_alt_fp_surf_proj_ALT,
                                          ggplot2::aes(x = x, y = NULL, group = id,
                                                       colour = x_alt_label,
                                                       fill = x_alt_label
                                # ^^ that has to
                                # be there to get
                                # the legend to
                                # merge
                                # properly, but
                                # it will cause
                                # a warning.
                                                       ),
                                          linewidth = 1.1)
                }
            }
        }
    }

    ## -------** Source data

    if (add_source_data) {
        gp <- gp + ggplot2::geom_point(data = source_data_df,
                                       ggplot2::aes(x = .data[[xvar_source]], y = .data[[yvar_source]]))

    }

    ## -------** Indicator Lines and Ribbons

    gp <- gp + ggplot2::geom_line(colour = line_colour, na.rm = TRUE)

    if (yvar %in% c("TFR_median")) {
        yvar_indicator <- gsub("_median", "", yvar)
        if (paste0(yvar_indicator, ".10pc") %in% colnames(x) &&
            paste0(yvar_indicator, ".90pc") %in% colnames(x)) {
            gp <- gp +
                ggplot2::geom_line(ggplot2::aes(y = .data[[paste0(yvar_indicator, ".10pc")]]),
                                   linetype = 2, colour = line_colour, na.rm = TRUE) +
                ggplot2::geom_line(ggplot2::aes(y = .data[[paste0(yvar_indicator, ".90pc")]]),
                                   linetype = 2, colour = line_colour, na.rm = TRUE)
        }
        if (paste0(yvar_indicator, ".2.5pc") %in% colnames(x) &&
            paste0(yvar_indicator, ".97.5pc") %in% colnames(x)) {
            gp <- gp +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[paste0(yvar_indicator, ".2.5pc")]],
                                                  ymax = .data[[paste0(yvar_indicator, ".97.5pc")]]),
                                     alpha = 0.15, fill = ribbon_fill)
        }

    } else if (identical(yvar, "annual_change_50%")) {
        gp <- gp +
            ggplot2::geom_line(ggplot2::aes(y = `annual_change_10pc`), linetype = 2, colour = line_colour, na.rm = TRUE) +
            ggplot2::geom_line(ggplot2::aes(y = `annual_change_90pc`), linetype = 2, colour = line_colour, na.rm = TRUE) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = `annual_change_2.5pc`, ymax = `annual_change_97.5pc`),
                                 alpha = 0.15, fill = ribbon_fill)
    }

    if (add_est_proj_ref_line && identical(xvar, "year")) {
        gp <- gp + ggplot2::geom_vline(ggplot2::aes(xintercept = est_proj_ref_line_year), linetype = 5, colour = "darkgrey")
    }

    ## -------** Annotation in top right corner

    if (!is.null(plot_ann)) gp <- add_plot_ann(gp, plot_ann)

    ## -------** Datestamp in bottom right corner

    if (datestamp) gp <- add_plot_datestamp(gp)

    ## -------* END

    return(gp)
}


##' Combination plot of TFR surfs
##'
##' Multiple surf plots on the same canvas, mostly copied from
##' \pkg{FPPlateaus}. Uses \pkg{ggplot2} and \pkg{ggpubr} to generate line plots
##' showing time periods where TFR surfs were detected.
##'
##' @inheritParams plot_tfr_surfs
##' @param ... Passed directly to \code{\link{plot_tfr_surfs}}.
##' @return A \pkg{ggplot2} object.
##' @author Mark Wheldon
##'
##' @family Plotting functions
##'
##' @seealso The \pkg{FPPlateaus} package (https://github.com/markalava/FPPlateaus).
##'
##' @export
plot_surfs_probs <- function(x, ...) {
    UseMethod("plot_surfs_probs")
}


##' @rdname plot_surfs_probs
##' @export
plot_surfs_probs.list <- function(x, x_alt = NULL, ..., incl_small_countries = TRUE,
                                   plot = TRUE, verbose = FALSE) {

    stopifnot(is.logical(incl_small_countries))
    if (!incl_small_countries) x <- remove_small_countries(x)
    if (!is.null(x_alt)) {
        x_alt <- remove_small_countries(x_alt)
        if (!identical(length(x_alt), length(x)))
            stop("'x' and 'x_alt' are different lengths.")
        else if (!identical(sort(names(x_alt)), sort(names(x))))
            stop("'x' and 'x_alt' have different names")
        else x_alt <- x_alt[names(x)]
    }

    if (!verbose)
        withr::local_options(list(tfrSURFs.message_about_unknown_aes = FALSE))

    out <- list()
    for (i in seq_along(x)) {
        out <- c(out, setNames(list(plot_surfs_probs(x = x[[i]], x_alt = x_alt[[i]], ...)),
                               nm = names(x)[i]))
        if (plot) print(out[[i]])
    }
    return(invisible(out))
}


##' @rdname plot_surfs_probs
##' @export
plot_surfs_probs.data.frame <- function(x, x_alt = NULL, ...) {

    ldots <- list(...)
    ndots <- names(ldots)
    if (any(c("xvar", "yvar") %in% ndots)) stop("You cannot specifiy 'xvar' or 'yvar' in the call to 'plot_surfs_probs'.")

    ## Add just one annotation and datestamp, if requested
    if ("plot_ann" %in% ndots) {
        plot_ann <- ldots[["plot_ann"]]
        ldots["plot_ann"] <- NULL
    } else plot_ann <- NULL

    if ("datestamp" %in% ndots) {
        datestamp <- ldots[["datestamp"]]
        ldots["datestamp"] <- NULL
    } else datestamp <- FALSE

    ## Make the plots
    pl1 <- do.call("plot_tfr_surfs",
                   args = c(list(x = x, x_alt = x_alt,
                                 xvar = "year", yvar = "TFR_median"),
                            ldots)) +
        ggplot2::ggtitle(label = "\nTotal Fertility Rate (TFR)")

    pl2 <- do.call("plot_tfr_surfs",
                   args = c(list(x = x, x_alt = x_alt,
                                 xvar = "year", yvar = "surf_prob"),
                            ldots)) +
        ggplot2::ggtitle(label = "\nSURF Probability")

    ## Country and subregion annotation
    gp <- ggpubr::ggarrange(pl1, pl2, nrow = 1, ncol = 2,
                      common.legend = TRUE, legend = "bottom") |>
        ggpubr::annotate_figure(fig.lab = paste0(x[1, "name"], " (",
                                                 x[1, "reg_name"], ")"),
                                fig.lab.pos = "top.left", fig.lab.size = 16)

    ## Annotation
    if (!is.null(plot_ann)) gp <- add_plot_ann(gp, plot_ann)

    ## Datastamp
    if (datestamp) gp <- add_plot_datestamp(gp)

    ## END
    return(gp)
}
