###
### GENERAL UTILITY FUNCTIONS
###

### So far, these are just in ALPHABETICAL ORDER.

##' Blank out cells in a table column
##'
##' Blanks out cells in a column that are the same as cell above.  In
##' tabulations, some columns are block titles that apply to several rows
##' beneath them. This function replaces cells with blanks if they are
##' duplicates of cells above them.
##'
##' Argument \code{cols} is passed to \code{\link{[}} as second
##' argument (i.e., \code{x[,j]}) so it can be either numeric or
##' character, as long as it selects a column of \code{x}.
##'
##' @param x Data frame, or object that can be coerced to one with
##'     \code{\link{as.data.frame}}.
##' @param cols Vector of column names or positions. Each element will be passed
##'     to \code{\link{[}} one-by-one.
##' @param reset_row.names Logical; should row names be set to \code{NULL} after
##'     blanking?
##' @return Data frame.
##' @author Mark C. Wheldon
##' @export
blankCells <- function(x, cols, reset_row.names = FALSE) {
    istib <- tibble::is_tibble(x)
    x <- as.data.frame(x)
    for (j in cols) {
        if (is.factor(x[, j])) x[, j] <- as.character(x[, j])
        z <- c("", head(x[, j], -1))
        x[x[, j] == z, j] <- ""
    }
    if (reset_row.names) row.names(x) <- NULL
    if (istib) x <- tibble::as_tibble(x)
    return(x)
}


##' Find breaks in an ordered numeric vector
##'
##' Elements that differ from their previous element by more than
##' \code{diff_tol} are flagged.
##'
##' @param x Vector.
##' @param diff_tol The difference that marks a break in the sequence.
##' @param first_is_break Logical; should the first element of \code{x} be
##'     considered a \dQuote{break} and be flagged?
##' @param add \code{\link[base]{cbind}} the result to \code{x}?
##' @return Vector (\code{add = FALSE}) or data frame (\code{add = TRUE}).
##' @author Mark Wheldon
##' @noRd
find_breaks <- function(x, diff_tol = (if (is.numeric(x)) {1} else {0}), first_is_break = FALSE, add = FALSE) {
    stopifnot(is.numeric(x) || is.logical(x))
    if (first_is_break) out <- as.logical(c(TRUE, diff(x) != diff_tol))
    else out <- as.logical(c(FALSE, diff(x) != diff_tol))
    if (add) return(data.frame(x, out))
    else return(out)
}


##' Create group numbers based on sequence breaks
##'
##' Like \code{number_groups} but uses \code{find_breaks} to determine
##' group membership.
##'
##' @param x Vector.
##' @param add \code{\link[base]{cbind}} the result to \code{x}?
##' @param diff_tol The difference that marks a break in the sequence.
##' @return Vector (\code{add = FALSE}) or data frame (\code{add = TRUE}).
##' @author Mark Wheldon
##' @noRd
number_sequence_groups <- function(x, add = FALSE) {
    out <- cumsum(find_breaks(x, add = FALSE))
    if (add) return(data.frame(x, out))
    else return(out)
}


##' Get default argument values as vector
##'
##' Returns the default values for function arguments as a vector.
##'
##' @param fn Name of function; passed to \code{\link{match.arg}}.
##' @param arg Character; name of argument. If \code{NULL}, all arguments are returned.
##' @return Vector; the type depends on the argument defaults.
##' @author Mark C Wheldon
##' @keywords internal
##' @export
get_arg_defs <- function(fn, arg = NULL) {
    fmls <- formals(match.fun(fn))
    if (is.null(arg)) return(fmls)
    else return(as.character(fmls[[arg]])[-1])
}


## COPIED from ?integer in base package
is.wholenumber <- function(x, tol = .Machine[["double.eps"]]^0.5)  abs(x - round(x)) < tol


## Set up cluster
set_cores_make_cluster <- function(ncores = NULL, maxjobs = NULL) {
    if (is.null(ncores)) return(NULL)
    else {
        stopifnot(is.numeric(ncores))
        stopifnot(is.numeric(maxjobs) && maxjobs > 0)
        if (ncores < 0) {
            if (!requireNamespace("parallelly", quietly = TRUE))
                stop("'ncores' is negative but package 'parallelly' is not installed: either install 'parallelly' and try again or set option 'cl.cores' to a positive number.")
            ncores <- parallelly::availableCores(omit = -ncores)
        } else {
            ncores <- ncores
        }
        if (is.null(maxjobs)) maxjobs <- ncores
        return(parallel::makeCluster(min(ncores, maxjobs)))
    }
}
