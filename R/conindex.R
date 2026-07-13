#' Rank-dependent inequality (concentration) indices
#'
#' `conindex()` computes a range of rank-dependent inequality indices, including
#' the Gini coefficient, the concentration index (CI), the generalized
#' concentration index, the modified concentration index, the Wagstaff and
#' Erreygers normalised indices for bounded variables, and the distributionally
#' sensitive extended and symmetric indices (and their generalized versions).
#' It is a faithful R translation of the Stata command `conindex` (O'Donnell et
#' al. 2016).
#'
#' There is no default index: the options select which index is computed, exactly
#' as in the Stata command. See the vignette and the examples below.
#'
#' @param data A data frame.
#' @param outcome String naming the outcome variable of interest (`h`).
#' @param rankvar String naming the ranking (socioeconomic) variable. If `NULL`
#'   (or equal to `outcome`), observations are ranked by `outcome` and the index
#'   is a Gini coefficient.
#' @param weights String naming a weights variable, or `NULL` for equal weights.
#' @param subset Optional logical vector (length `nrow(data)`) selecting the
#'   estimation sample (the analogue of Stata `if`).
#' @param cluster String naming a cluster variable for clustered standard errors.
#' @param robust Logical; request Huber/White/sandwich (HC1) standard errors.
#' @param truezero Logical; declare the outcome ratio-scaled (fixed zero),
#'   yielding the standard concentration index.
#' @param limits Numeric vector giving the theoretical minimum (and, for bounded
#'   variables, maximum): `limits = min` or `limits = c(min, max)`. Use `NA` for a
#'   missing bound.
#' @param generalized Logical; request the generalized (absolute) index. Requires
#'   `truezero`.
#' @param bounded Logical; the outcome is bounded. Requires `limits = c(min, max)`.
#' @param wagstaff,erreygers Logical; Wagstaff / Erreygers normalisation for
#'   bounded variables (with `bounded` and `limits`).
#' @param v Numeric > 1; the extended-index distributional-sensitivity parameter.
#'   Requires `truezero`.
#' @param beta Numeric > 1; the symmetric-index sensitivity parameter. Requires
#'   `truezero`.
#' @param compare String naming a group variable across which to compare the index
#'   and test homogeneity.
#' @param by String naming a variable; the index is computed **separately** within
#'   each level of `by` (the analogue of the Stata `by varlist:` prefix), returning
#'   one result per group. Cannot be combined with `compare`.
#' @param keeprank Logical; if `TRUE` the fractional rank is returned in the result
#'   (element `rank`).
#' @param svy Logical; use a complex survey design (experimental — see Details).
#' @param design A [survey::svydesign()] object, required when `svy = TRUE`.
#'
#' @details
#' Standard errors reproduce Stata's `regress` variance estimators: HC1 for
#' `robust`, cluster-robust (HC1 with the small-sample cluster adjustment) for
#' `cluster`. Standard errors are **not** produced for the extended and symmetric
#' indices (matching the original). The `svy` path is experimental: it fits the
#' convenient-covariance regression with [survey::svyglm()] and has not been
#' validated against the Stata oracle.
#'
#' @return An object of class `conindex` (see [print.conindex()]) with elements
#'   including `value` (the index), `se`, `n`, `nunique`, `type`, and — with
#'   `compare` — a `comparison` component.
#'
#' @references
#' O'Donnell, O., O'Neill, S., Van Ourti, T. & Walsh, B. (2016). conindex:
#' Estimation of concentration indices. *The Stata Journal*.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   h = rgamma(500, 2, 2),
#'   ses = rnorm(500),
#'   wt = runif(500, 0.5, 1.5)
#' )
#' # Standard concentration index
#' conindex(df, "h", rankvar = "ses", truezero = TRUE)
#' # Gini coefficient (rank by the outcome itself)
#' conindex(df, "h", truezero = TRUE)
#' @export
conindex <- function(data, outcome, rankvar = NULL, weights = NULL,
                     subset = NULL, cluster = NULL, robust = FALSE,
                     truezero = FALSE, limits = NULL, generalized = FALSE,
                     bounded = FALSE, wagstaff = FALSE, erreygers = FALSE,
                     v = NULL, beta = NULL, compare = NULL, by = NULL,
                     keeprank = FALSE, svy = FALSE, design = NULL) {
  cl <- match.call()

  ## --- Argument / column checks -------------------------------------------
  stopifnot(is.data.frame(data))
  .check_col(data, outcome, "outcome")
  .check_col(data, rankvar, "rankvar")
  .check_col(data, weights, "weights")
  .check_col(data, cluster, "cluster")
  .check_col(data, compare, "compare")
  .check_col(data, by, "by")
  if (!is.null(by) && !is.null(compare)) {
    stop("The option compare cannot be used in conjunction with by.", call. = FALSE)
  }
  if (svy && is.null(design)) {
    stop("When svy = TRUE, a survey design (design=) must be supplied.", call. = FALSE)
  }
  if (svy && !is.null(weights)) {
    stop("When the svy option is used, weights should only be specified using the survey design.",
         call. = FALSE)
  }

  ## --- by: run the command separately within each group -------------------
  if (!is.null(by)) {
    return(.conindex_by(
      data, outcome = outcome, rankvar = rankvar, weights = weights,
      subset = subset, cluster = cluster, robust = robust, truezero = truezero,
      limits = limits, generalized = generalized, bounded = bounded,
      wagstaff = wagstaff, erreygers = erreygers, v = v, beta = beta,
      keeprank = keeprank, svy = svy, design = design, by = by, cl = cl
    ))
  }

  ## --- compare(): delegate to the group routine ---------------------------
  if (!is.null(compare)) {
    return(.conindex_compare(
      data, outcome, rankvar, weights, subset, cluster, robust, truezero,
      limits, generalized, bounded, wagstaff, erreygers, v, beta, compare,
      keeprank, cl
    ))
  }

  ## --- Build the estimation sample (touse) --------------------------------
  n_all <- nrow(data)
  touse <- rep(TRUE, n_all)
  if (!is.null(subset)) {
    if (length(subset) != n_all || !is.logical(subset)) {
      stop("`subset` must be a logical vector of length nrow(data).", call. = FALSE)
    }
    touse <- touse & !is.na(subset) & subset
  }
  req <- c(outcome, rankvar, weights, cluster)
  for (nm in req) touse <- touse & !is.na(data[[nm]])

  if (!any(touse)) stop("No observations left after applying the sample restriction.", call. = FALSE)

  h_all <- data[[outcome]]
  h <- h_all[touse]
  ranking <- if (is.null(rankvar)) h else data[[rankvar]][touse]
  w <- if (is.null(weights)) rep(1, sum(touse)) else data[[weights]][touse]
  clus <- if (is.null(cluster)) NULL else data[[cluster]][touse]

  ## --- Summaries: N, mean, negative-value note ----------------------------
  N <- length(h)
  testmean <- if (is.null(weights)) mean(h) else sum(w * h) / sum(w)
  n_neg <- sum(h < 0)
  if (n_neg > 0) message(sprintf("Note: '%s' has %d values less than 0", outcome, n_neg))

  index <- if (is.null(rankvar) || identical(rankvar, outcome)) "Gini" else "CI"

  ## --- Resolve index type & validate options; standardise h_star ----------
  res <- .resolve_flags(
    h = h, h_all = h_all, testmean = testmean, index = index,
    truezero = truezero, limits = limits, generalized = generalized,
    bounded = bounded, wagstaff = wagstaff, erreygers = erreygers,
    v = v, beta = beta, svy = svy
  )
  flags <- res$flags
  h_star <- res$h_star

  ## --- Compute -------------------------------------------------------------
  out <- .compute_index(
    h = h, h_star = h_star, ranking = ranking, w = w, flags = flags,
    se = list(robust = robust, cluster = clus, design = if (svy) design else NULL)
  )

  ## --- Assemble result -----------------------------------------------------
  # Degrees of freedom for the displayed t p-value: Stata's regress uses the
  # residual df, except with vce(cluster) where it uses (#clusters - 1).
  p_df <- if (!is.na(out$nclus)) out$nclus - 1 else out$df_r

  result <- list(
    value = out$CI,
    se = out$CIse,
    n = out$N,
    nunique = out$Nunique,
    rss = out$RSS,
    type = out$type,
    index = index,
    df_r = out$df_r,
    nclus = out$nclus,
    p_value = if (is.na(out$CIse)) NA_real_ else 2 * stats::pt(abs(out$CI / out$CIse), p_df, lower.tail = FALSE),
    se_type = if (!is.null(clus) || robust) "Robust std. error" else "Std. error",
    outcome = outcome,
    rankvar = rankvar,
    cluster = cluster,
    call = cl
  )
  # Hidden internals used by compare(): convenient-regression vectors + mask.
  result$.lhs <- out$lhs
  result$.rhs <- out$rhs
  result$.touse <- touse
  if (keeprank) {
    fr <- .fractional_rank(ranking, w)
    rank_full <- rep(NA_real_, n_all)
    rank_full[touse] <- fr$frnk
    result$rank <- rank_full
  }
  class(result) <- "conindex"
  result
}

#' @keywords internal
#' @noRd
.check_col <- function(data, col, argname) {
  if (is.null(col)) return(invisible())
  if (!is.character(col) || length(col) != 1) {
    stop(sprintf("`%s` must be a single column name (string) or NULL.", argname), call. = FALSE)
  }
  if (!col %in% names(data)) {
    stop(sprintf("Column '%s' (%s) not found in `data`.", col, argname), call. = FALSE)
  }
  invisible()
}
