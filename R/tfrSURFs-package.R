#' @section Package options:
#' \describe{
#' \item{sensitivity_analysis_output_dir_name}{(Default = \code{"sensitivity_analysis"}) Name of the directory in which to save the sensitivity analysis results (see \code{?sensitivity_analysis_helpers}).}
#' \item{sensitivity_analysis_overwrite}{(Default = \code{FALSE}) Logical; should any existing sensitivity analysis results be overridden (see \code{?sensitivity_analysis_helpers})?}
#' \item{show_ggplot_warning_note}{(Default = \code{TRUE}) Logical. \pkg{ggplot2} code in the function \code{\link{plot_tfr_surfs}} produces a spurious warning. When this option is \code{TRUE}, a message is generated noting that this can be ignored.}
#' \item{sim.dir}{(Default = \code{NULL}) Name of the directory containing the sample from the posterior distribution of total fertility rate (TFR); see \code{\link{make_tfr_surfs}}.}
#' \item{verbose}{(Default = \code{getOption("verbose")}) Logical; should lots of messages be generated (esp. by \code{\link{make_tfr_surfs}})?}
#'
#' @references
#'
#' Wheldon, M. C., Kantorová, V., Molitoris, J., Spoorenberg, T., Kamiya, Y., &
#' Gerland, P. (2025). Caught in Transit: Identifying Stalls, Upswings and
#' Reversals in Fertility Transitions using a Probabilistic Approach (U6r7n_v2).
#' SocArXiv. https://doi.org/10.31235/osf.io/u6r7n_v2
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
