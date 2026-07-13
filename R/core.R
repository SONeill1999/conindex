# Internal numeric engine for conindex.
# Faithful translation of Stata/conindex.ado sections 6-10 (see
# ../pseudocode/conindex.pseudo). Operates on vectors already restricted to the
# estimation sample.

#' Compute a single concentration index
#'
#' @param h Numeric outcome vector (in-sample).
#' @param h_star Numeric standardised outcome (`= (h-min)/(max-min)` for bounded,
#'   else `= h`).
#' @param ranking Numeric ranking (SES) vector.
#' @param w Numeric weight vector.
#' @param flags List of resolved logical/numeric options: `generalized`,
#'   `modified`, `bounded`, `wagstaff`, `erreygers`, `extended`, `symmetric`,
#'   `v`, `beta`, `xmin`, `xmax`, `index`, `svy`.
#' @param se List controlling the variance estimator: `robust` (logical),
#'   `cluster` (numeric/character vector or NULL), `design` (survey design or NULL).
#' @return List with `CI`, `CIse`, `RSS`, `N`, `Nunique`, `type`, `nclus`, `df_r`.
#' @keywords internal
#' @noRd
.compute_index <- function(h, h_star, ranking, w, flags, se) {
  n <- length(h)

  ## --- Section 6: fractional rank, rank variance, weighted mean ------------
  fr <- .fractional_rank(ranking, w)
  frnk <- fr$frnk
  cumwr <- fr$cumwr
  cumwr_1 <- fr$cumwr_1
  sumw <- fr$sumw

  sigma2 <- sum((w / sumw) * (frnk - 0.5)^2)
  meanlhs <- sum(w * h_star) / sumw            # weighted mean of h_star

  ## Scale factor folding weights into an unweighted OLS (convenient covariance)
  if (isTRUE(flags$svy) && !flags$extended && !flags$symmetric) {
    scale <- rep(1, n)
  } else {
    scale <- sqrt(w)
  }

  ## =========================================================================
  ## Extended / symmetric indices (Section 9) -- replace lhs/rhs entirely
  ## =========================================================================
  if (flags$extended || flags$symmetric) {
    grp <- match(ranking, unique(ranking))
    temp1 <- w * h_star
    sumlhs <- sum(temp1)
    sumwr <- stats::ave(w, grp, FUN = sum)
    meanoverall <- sumlhs / sumw
    meanlhs2 <- stats::ave(temp1, grp, FUN = sum) / sumwr   # group weighted mean of h_star

    # One representative row per unique rank value (temp0 == 1).
    rep_mask <- !duplicated(grp)

    if (flags$extended) {
      v <- flags$v
      rhs <- (sumwr / sumw) + (1 - cumwr / sumw)^v - (1 - cumwr_1 / sumw)^v
      rhs[!rep_mask] <- NA
      temp2 <- sum(rhs[rep_mask]^2)
      if (flags$generalized) {
        type <- paste("Gen. extended", flags$index)
        lhs <- (meanlhs2 * (v^(v / (v - 1))) / (v - 1)) * temp2
      } else {
        type <- paste("Extended", flags$index)
        lhs <- (meanlhs2 / meanoverall) * temp2
      }
    } else { # symmetric
      beta <- flags$beta
      rhs <- (2^(beta - 2)) *
        (abs(cumwr / sumw - 0.5)^beta - abs(cumwr_1 / sumw - 0.5)^beta)
      rhs[!rep_mask] <- NA
      temp2 <- sum(rhs[rep_mask]^2)
      if (flags$generalized) {
        type <- paste("Gen. symmetric", flags$index)
        lhs <- meanlhs2 * 4 * temp2
      } else {
        type <- paste("Symmetric", flags$index)
        lhs <- (meanlhs2 / meanoverall) * temp2
      }
    }

    d <- data.frame(lhs = lhs[rep_mask], rhs = rhs[rep_mask])
    m <- stats::lm(lhs ~ rhs, data = d)                 # WITH constant, per ado
    ci <- unname(stats::coef(m)["rhs"])
    return(list(
      CI = ci, CIse = NA_real_, RSS = sum(stats::residuals(m)^2),
      N = n, Nunique = stats::nobs(m), type = type,
      nclus = NA_real_, df_r = m$df.residual,
      lhs = NULL, rhs = NULL
    ))
  }

  ## =========================================================================
  ## Standard / generalized / modified / Wagstaff / Erreygers (Sections 8, 10)
  ## =========================================================================
  # meanlhs adjustment for the modified index while building lhs (ado 340-342)
  meanlhs_for_lhs <- meanlhs
  if (flags$modified && !flags$bounded) meanlhs_for_lhs <- meanlhs - flags$xmin

  lhs <- 2 * sigma2 * (h_star / meanlhs_for_lhs) * scale
  intercept <- scale
  rhs <- frnk * scale

  type <- flags$index

  # Index-specific LHS transforms (ado 418-441); meanlhs here is the raw mean.
  if (flags$modified) {
    type <- paste("Modified", flags$index)
    lhs <- lhs * (meanlhs) / (meanlhs - flags$xmin)
  }
  if (flags$wagstaff) {
    type <- paste("Wagstaff norm.", flags$index)
    lhs <- lhs / (1 - meanlhs)
  }
  if (flags$erreygers) {
    type <- paste("Erreygers norm.", flags$index)
    lhs <- lhs * (4 * meanlhs)
  }
  if (flags$generalized) {
    type <- paste("Gen.", flags$index)
    lhs <- lhs * meanlhs
  }

  ## --- Section 10: convenient-covariance regression, no constant ----------
  d <- data.frame(lhs = lhs, rhs = rhs, intercept = intercept)

  if (!is.null(se$design)) {
    # Experimental survey-linearised path (not verified against the Stata oracle)
    if (!requireNamespace("survey", quietly = TRUE)) {
      stop("The 'survey' package is required for svy standard errors.", call. = FALSE)
    }
    dsg <- stats::update(se$design, .lhs = lhs, .rhs = rhs, .int = intercept)
    m <- survey::svyglm(.lhs ~ .rhs + .int - 1, design = dsg)
    V <- stats::vcov(m)
    ci <- unname(stats::coef(m)[".rhs"])
    cise <- sqrt(V[".rhs", ".rhs"])
    rss <- sum(stats::residuals(m, "working")^2)
    return(list(CI = ci, CIse = cise, RSS = rss, N = n,
                Nunique = n, type = type, nclus = NA_real_,
                df_r = stats::df.residual(m), lhs = lhs, rhs = rhs))
  }

  m <- stats::lm(lhs ~ rhs + intercept - 1, data = d)
  ci <- unname(stats::coef(m)["rhs"])

  # Variance estimator: match Stata regress vce().
  nclus <- NA_real_
  if (!is.null(se$cluster)) {
    V <- sandwich::vcovCL(m, cluster = se$cluster, type = "HC1", cadjust = TRUE)
    nclus <- length(unique(se$cluster))
  } else if (isTRUE(se$robust)) {
    V <- sandwich::vcovHC(m, type = "HC1")
  } else {
    V <- stats::vcov(m)
  }
  cise <- sqrt(V["rhs", "rhs"])

  list(
    CI = ci, CIse = cise, RSS = sum(stats::residuals(m)^2),
    N = n, Nunique = stats::nobs(m), type = type,
    nclus = nclus, df_r = m$df.residual, lhs = lhs, rhs = rhs
  )
}
