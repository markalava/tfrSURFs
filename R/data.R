#' Table of locations: Country names, regions, and codes
#'
#' A table of all country and area names, numeric codes, and the regions and
#' subregions they belong to, for statistical purposes as available in 2024
#' (United Nations, 2024). All names are the English names using only ASCII
#' characters.
#'
#' @format
#' A data frame with `r nrow(tfrSURFs::UNlocations_countries)` rows and `r ncol(tfrSURFs::UNlocations_countries)` columns:
#' \describe{
#'   \item{\code{country_code}}{Three-digit M49 numeric country code (United Nations, 2018).}
#'   \item{\code{name}}{Country name.}
#'   \item{\code{area_name}}{Major geographic area.}
#'   \item{\code{reg_name}}{Subregional geographic area (within \code{area_name}).}
#'   \item{\code{Pop_lt_90k_2024}}{Logical flag indicating whether the country or area had a population less than 90000 in mid-2024.}
#' }
#' @source `r comment(tfrSURFs::UNlocations_countries)`
#'
#' @references
#' United Nations (2018) "Standard Country or Area Codes for Statistical Use (M49)", New York, N.Y.:
#' United Nations, Department of Economic and Social Affairs, Statistics
#' Division. \url{https://unstats.un.org/unsd/methodology/m49/} (accessed
#' 2025-06-18).
#'
#' United Nations (2024) \emph{World Population Prospects: The 2024 Revision},
#' New York, N.Y.: United Nations, Department of Economic and Social Affairs,
#' Population Division. \url{https://population.un.org/wpp/} (accessed
#' 2025-06-18).
#'
#' @keywords datasets
"UNlocations_countries"


#' TFR stalls identified by Schoumaker (2019)
#'
#' Stalls in total fertility rates (TFR) in countries in sub-Saharan Africa
#' identified by Schoumaker (2019). These are taken from Appendix B in the
#' Supporting Information.
#'
#' @section Changes:
#'
#' \subsection{Year ranges}{
#' This data dataset is in \dQuote{long}, or \dQuote{database} format: each row
#' corresponds to a separate country-year combination in the original. In the
#' original, some countries had overlapping year ranges (for example, Benin had
#' three entries with year ranges \dQuote{1996-2001}, \dQuote{2001-2006},
#' \dQuote{2006-2012}). This implied there was ambiguity over which stall the
#' year-range endpoints belonged to (if any). To resolve this, year ranges in the
#' original were read as closed on the left, open on the right i.e., [yyyy,
#' zzzz). For example, the period "1996-2001" was read as [1996, 2001) so that
#' 1996 was recorded as belonging to this particular stall but 2001 was not.}
#'
#' \subsection{Ghana 1998 DHS}{
#' The year ranges for following entries for Ghana
#' \preformatted{
#'     Country     Period     ...
#'     -------    ---------
#'     Ghana      1993-1999   ...
#'     Ghana      1999-2003   ...
#' }
#' where modified to \dQuote{1993-1998} and \dQuote{1998-2003} to reflect the
#' fact that the DHS in Ghana was conducted in 1998, not 1999. The modification
#' matches the information in Table 1 of the original journal article.
#' }
#'
#' @format
#' A data frame with 169 rows and 6 columns:
#' \describe{
#' \item{\code{year}}{Numeric; calendar year}
#' \item{\code{country_code}}{Numeric; three-digit numeric country code (\cite{United Nations, 2018}).}
#'   \item{\code{name}}{Country name.}
#' \item{\code{Schoumaker_stall_type}}{Character; type of Schoumaker stall. Takes one of five values, \code{"Strong+ evidence"}, \code{"Moderate evidence"}, \code{"Limited evidence"}, \code{"Pretransitional"}, or \code{"No stall"}.}
#' \item{\code{Schoumaker_stall_strong}}{Logical; stalls with strong, or very strong, support.}
#' \item{\code{Schoumaker_stall_moderate}}{Logical; stalls with moderate support.}
#' \item{\code{Schoumaker_stall_weak}}{Logical; stalls with weak support.}
#' \item{\code{Schoumaker_stall_any}}{Logical; stalls with any degree of support.}
#' \item{\code{Schoumaker_stall_pretransitional}}{Logical; pretransitional stalls.}
#' \item{\code{Schoumaker_stall_none}}{Logical; time period analyzed but no stall found.}
#' }
#' @source Schoumaker, B. (2019), "Stalls in Fertility Transitions in sub-Saharan Africa: Revisiting the Evidence," Studies in Family Planning, 50, 257–278, Appendix B. \url{https://doi.org/10/gf5zpt}. Published under CC BY 4.0 - Attribution 4.0 International (\url{https://creativecommons.org/licenses/by/4.0/}).
#'
#' @references
#' United Nations, Department of Economic and Social Affairs, Statistics Division (2018), "Standard Country or Area Codes for Statistical Use (M49)," Available at \url{https://unstats.un.org/unsd/methodology/m49/}.
#'
#' @keywords datasets
"schoumaker_2019_tfr_stalls"


#' SURF output column definitions
#'
#' List and definitions of columns in the data frames produced by \code{\link{make_tfr_surfs}}.
#'
#' @format
#' A data frame with columns:
#' \describe{
#' \item{column_group}{Grouping category of column}
#' \item{column_name}{Name of column}
#' \item{type}{Data type of column}
#' \item{description}{Description of the column}
#' }
#'
#' @keywords datasets
"output_column_definitions"



#' Data sets used for testing
#'
#' Results of running \code{\link{make_tfr_surfs}} on very short,
#' \emph{definitely not converged} runs of \pkg{bayesTFR} functions, for small
#' subsets of countries.
#' \describe{
#' \item{\code{"test_data_tfrSURFs_list"}}{\code{\link{make_tfr_surfs}} applied to country codes: 12, 716, 508, 417, 764, 32, 250, 242.}
#' \item{\code{"test_data_small_c_tfrSURFs_list"}}{\code{\link{make_tfr_surfs}} applied to countries with less than country codes: 184, 212, 500.}
#' }
#'
#' These need to be prepared during development and stored in \file{./data}
#' because they rely on a run of \pkg{bayesTFR} which is too big to distribute
#' with the package.
#'
#' @format
#' Data frames in the format produced by \code{\link{make_tfr_surfs}}.
#'
#' @source See \pkg{bayesTFR}. These results were based on parameter estimates and projections with 10 trajectories only.
#'
#' @keywords datasets
"test_data_tfrSURFs_list"

#' @rdname test_data_tfrSURFs_list
"test_data_small_c_tfrSURFs_list"
