# Across-group comparison of a concentration index with homogeneity tests.
# Faithful translation of Stata/conindex.ado section 13 (lines 535-611).

#' @keywords internal
#' @noRd
.conindex_compare <- function(data, outcome, rankvar, weights, subset, cluster,
                              robust, truezero, limits, generalized, bounded,
                              wagstaff, erreygers, v, beta, compare, keeprank,
                              cl) {
  n_all <- nrow(data)

  # Sample mask (touse): non-missing on required vars + subset.
  touse <- rep(TRUE, n_all)
  if (!is.null(subset)) touse <- touse & !is.na(subset) & subset
  for (nm in c(outcome, rankvar, weights, cluster, compare)) {
    touse <- touse & !is.na(data[[nm]])
  }

  # Integer group ids 1..gmax (Stata egen group()).
  gvals <- sort(unique(data[[compare]][touse]))
  group <- match(data[[compare]], gvals)
  gmax <- length(gvals)
  if (gmax < 2) stop("compare() requires at least two groups.", call. = FALSE)

  # Common option set forwarded to each per-group call.
  base_args <- list(
    data = data, outcome = outcome, rankvar = rankvar, weights = weights,
    cluster = cluster, robust = robust, truezero = truezero, limits = limits,
    generalized = generalized, bounded = bounded, wagstaff = wagstaff,
    erreygers = erreygers, v = v, beta = beta
  )

  # Overall index (whole sample) for the header.
  overall <- do.call(conindex, c(base_args, list(subset = touse)))

  # Per-group indices + pooled convenient-regression vectors.
  lhscomp <- rep(NA_real_, n_all)
  rhscomp <- rep(NA_real_, n_all)
  groups <- vector("list", gmax)
  for (i in seq_len(gmax)) {
    gi <- touse & group == i
    ri <- do.call(conindex, c(base_args, list(subset = gi)))
    groups[[i]] <- list(value = ri$value, se = ri$se, n = ri$n,
                        type = ri$type, group_value = gvals[i])
    if (!is.null(ri$.lhs)) {
      lhscomp[ri$.touse] <- ri$.lhs
      rhscomp[ri$.touse] <- ri$.rhs
    }
  }

  # Chow-type F test via restricted vs unrestricted pooled regressions.
  Ftest <- NULL
  if (all(!is.na(lhscomp[touse]))) {
    dd <- data.frame(
      lhs = lhscomp[touse], rhs = rhscomp[touse],
      grp = factor(group[touse])
    )
    m_r <- stats::lm(lhs ~ rhs + grp, data = dd)          # common slope + group FE
    m_u <- stats::lm(lhs ~ rhs * grp, data = dd)          # slope interacted with group
    sse_r <- sum(stats::residuals(m_r)^2)
    sse_u <- sum(stats::residuals(m_u)^2)
    n_r <- stats::nobs(m_r)
    Fstat <- ((sse_r - sse_u) / (gmax - 1)) / (sse_u / (n_r - 2 * gmax))
    p_F <- stats::pf(Fstat, gmax - 1, n_r - 2 * gmax, lower.tail = FALSE)
    Ftest <- list(F = Fstat, df1 = gmax - 1, df2 = n_r - 2 * gmax, p = p_F,
                  sse_restricted = sse_r, sse_unrestricted = sse_u,
                  n_restricted = n_r)
  }

  # Two-group large-sample z test.
  ztest <- NULL
  if (gmax == 2 && !is.na(groups[[1]]$se) && !is.na(groups[[2]]$se)) {
    diff <- groups[[2]]$value - groups[[1]]$value
    diffse <- sqrt(groups[[1]]$se^2 + groups[[2]]$se^2)
    z <- diff / diffse
    p_z <- 2 * (1 - stats::pnorm(abs(z)))
    ztest <- list(diff = diff, se = diffse, z = z, p = p_z,
                  CI0 = groups[[1]]$value, CI1 = groups[[2]]$value,
                  CIse0 = groups[[1]]$se, CIse1 = groups[[2]]$se)
  }

  result <- overall
  result$comparison <- list(
    compare_var = compare, groups = groups, Ftest = Ftest, ztest = ztest
  )
  result$call <- cl
  class(result) <- c("conindex_compare", "conindex")
  result
}
