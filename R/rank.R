#' Weighted fractional rank
#'
#' Computes the weighted fractional rank used by all concentration indices,
#' reproducing the tie handling of the Stata `conindex` command exactly.
#'
#' Observations are ordered by `ranking`. The running cumulative weight is
#' formed, and observations sharing the same value of `ranking` (ties) are
#' assigned the mid-interval rank so that tied observations share a single
#' fractional rank. Formally, for the cumulative weight `cumw` and its lag
#' `cumw_1` (0 for the first observation), with `cumwr = max(cumw)` and
#' `cumwr_1 = min(cumw_1)` taken within each tie group,
#' \deqn{frnk = (cumwr\_1 + 0.5 (cumwr - cumwr\_1)) / sumw.}
#'
#' @param ranking Numeric vector to rank by (the SES / rank variable).
#' @param w Numeric vector of weights (same length as `ranking`).
#'
#' @return A list with elements `frnk` (fractional rank), `cumwr`, `cumwr_1`
#'   and `sumw`, each aligned to the *original* input order.
#' @keywords internal
#' @noRd
.fractional_rank <- function(ranking, w) {
  n <- length(ranking)
  sumw <- sum(w)

  # Stable sort by the ranking variable, remembering original positions.
  ord <- order(ranking)
  r_sorted <- ranking[ord]
  w_sorted <- w[ord]

  cumw <- cumsum(w_sorted)
  cumw_1 <- c(0, cumw[-n])                       # lag; first obs -> 0

  # Collapse ties: within each group of equal `ranking`, cumwr = max(cumw),
  # cumwr_1 = min(cumw_1). ave() operates within groups defined by r_sorted.
  grp <- match(r_sorted, unique(r_sorted))
  cumwr <- stats::ave(cumw, grp, FUN = max)
  cumwr_1 <- stats::ave(cumw_1, grp, FUN = min)

  frnk_sorted <- (cumwr_1 + 0.5 * (cumwr - cumwr_1)) / sumw

  # Restore original order.
  frnk <- numeric(n)
  cumwr_o <- numeric(n)
  cumwr_1_o <- numeric(n)
  frnk[ord] <- frnk_sorted
  cumwr_o[ord] <- cumwr
  cumwr_1_o[ord] <- cumwr_1

  list(frnk = frnk, cumwr = cumwr_o, cumwr_1 = cumwr_1_o, sumw = sumw)
}
