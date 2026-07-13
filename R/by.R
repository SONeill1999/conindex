# by: run conindex separately within each level of a grouping variable.
# The R analogue of Stata's `by varlist:` prefix (byable(recall) in the .ado).
# Unlike compare(), no cross-group tests are produced: each group is an entirely
# independent estimation.

#' @keywords internal
#' @noRd
.conindex_by <- function(data, outcome, rankvar, weights, subset, cluster,
                         robust, truezero, limits, generalized, bounded,
                         wagstaff, erreygers, v, beta, keeprank, svy, design,
                         by, cl) {
  n_all <- nrow(data)

  # User subset (if any), combined per group below. by-variable missings excluded.
  base_touse <- rep(TRUE, n_all)
  if (!is.null(subset)) {
    if (length(subset) != n_all || !is.logical(subset)) {
      stop("`subset` must be a logical vector of length nrow(data).", call. = FALSE)
    }
    base_touse <- base_touse & !is.na(subset) & subset
  }
  base_touse <- base_touse & !is.na(data[[by]])

  byvals <- sort(unique(data[[by]][base_touse]))
  if (length(byvals) == 0) {
    stop("No non-missing values of the by-variable in the sample.", call. = FALSE)
  }

  results <- vector("list", length(byvals))
  for (i in seq_along(byvals)) {
    grp_subset <- base_touse & (data[[by]] == byvals[i])
    results[[i]] <- conindex(
      data, outcome = outcome, rankvar = rankvar, weights = weights,
      subset = grp_subset, cluster = cluster, robust = robust,
      truezero = truezero, limits = limits, generalized = generalized,
      bounded = bounded, wagstaff = wagstaff, erreygers = erreygers,
      v = v, beta = beta, keeprank = keeprank, svy = svy, design = design
    )
  }
  names(results) <- as.character(byvals)

  structure(
    list(by_var = by, levels = byvals, results = results, call = cl),
    class = "conindex_by"
  )
}

#' Print by-group concentration indices
#'
#' @param x A `conindex_by` object.
#' @param ... Passed to [print.conindex()].
#' @return `x`, invisibly.
#' @export
print.conindex_by <- function(x, ...) {
  for (i in seq_along(x$levels)) {
    cat("\n", strrep("-", 60), "\n", sep = "")
    cat("-> ", x$by_var, " = ", as.character(x$levels[i]), "\n", sep = "")
    print(x$results[[i]], ...)
  }
  invisible(x)
}

#' Combine by-group indices into a data frame
#'
#' @param x A `conindex_by` object.
#' @param ... Ignored.
#' @param row.names,optional Passed through to the data-frame constructor.
#' @return A data frame with one row per by-group (`level`, `type`, `index`,
#'   `se`, `n`).
#' @exportS3Method as.data.frame conindex_by
as.data.frame.conindex_by <- function(x, row.names = NULL, optional = FALSE, ...) {
  do.call(rbind, lapply(seq_along(x$levels), function(i) {
    r <- x$results[[i]]
    data.frame(
      level = as.character(x$levels[i]),
      type = r$type,
      index = r$value,
      se = r$se,
      n = r$n,
      stringsAsFactors = FALSE
    )
  }))
}
