#' conindex: Rank-Dependent Inequality (Concentration) Indices
#'
#' An R translation of the Stata command `conindex` (O'Donnell, O'Neill, Van
#' Ourti & Walsh, 2016). It computes rank-dependent inequality indices used to
#' measure socioeconomic inequality in a health or economic outcome.
#'
#' @section Indices:
#' All indices are estimated with the "convenient covariance" regression approach
#' (Kakwani 1980; Jenkins 1988; Kakwani et al. 1997). The main entry point is
#' [conindex()]; the index is selected by the options, exactly as in Stata:
#'
#' \tabular{ll}{
#'   Gini                         \tab `truezero` (no `rankvar`) \cr
#'   Generalized Gini             \tab `truezero, generalized` \cr
#'   Concentration index (CI)     \tab `rankvar, truezero` \cr
#'   Generalized CI               \tab `rankvar, truezero, generalized` \cr
#'   Modified CI                  \tab `rankvar, limits = min` \cr
#'   Wagstaff                     \tab `rankvar, bounded, limits = c(min,max), wagstaff` \cr
#'   Erreygers                    \tab `rankvar, bounded, limits = c(min,max), erreygers` \cr
#'   Extended CI                  \tab `rankvar, truezero, v` \cr
#'   Symmetric CI                 \tab `rankvar, truezero, beta` \cr
#' }
#'
#' @section Measurement scales (from the original help file):
#' \describe{
#'   \item{Fixed}{Unique scale, zero = complete absence (e.g. hospital visits).}
#'   \item{Ratio}{Ratios meaningful, zero = complete absence (e.g. life expectancy).}
#'   \item{Cardinal}{Differences meaningful, zero fixed arbitrarily (e.g. a health
#'     utility index) — use `limits = min` for the modified index.}
#'   \item{Ordinal}{Only ordering meaningful (e.g. self-assessed health).}
#'   \item{Nominal}{Classification only, no ordering (e.g. type of illness).}
#' }
#'
#' @references
#' O'Donnell, O., O'Neill, S., Van Ourti, T. & Walsh, B. (2016). conindex:
#' Estimation of concentration indices. *The Stata Journal*.
#'
#' O'Donnell, O., van Doorslaer, E., Wagstaff, A. & Lindelow, M. (2008).
#' *Analyzing Health Equity Using Household Survey Data*. World Bank Institute.
#'
#' Erreygers, G. (2009). Correcting the concentration index. *Journal of Health
#' Economics* 28, 504-515.
#'
#' Wagstaff, A. (2005). The bounds of the concentration index when the variable
#' of interest is binary, with an application to immunization inequality.
#' *Health Economics* 14, 429-432.
#'
#' @seealso [conindex()], [run_conindex_app()]
#' @keywords internal
"_PACKAGE"
