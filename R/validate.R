# Option validation and index-type resolution.
# Faithful translation of Stata/conindex.ado section 5 (lines 153-320).
# Errors abort with an informative message (Stata's `exit 498`).
# `h_all` is the full outcome column (Stata's unweighted whole-data summarize
# used for range checks); `h` is the in-sample outcome.

#' @keywords internal
#' @noRd
.resolve_flags <- function(h, h_all, testmean, index, truezero, limits,
                           generalized, bounded, wagstaff, erreygers,
                           v, beta, svy) {
  fail <- function(msg) stop(msg, call. = FALSE)

  ## limits -> xmin / xmax (NA = missing)
  xmin <- if (is.null(limits) || length(limits) < 1) NA_real_ else limits[1]
  xmax <- if (is.null(limits) || length(limits) < 2) NA_real_ else limits[2]

  h_star <- h
  extended <- FALSE
  symmetric <- FALSE
  modified <- FALSE
  gen_flag <- isTRUE(generalized)

  rng_all <- range(h_all, na.rm = TRUE)     # unweighted whole-data min/max

  ## --- truezero sanity (ado 158-169) --------------------------------------
  if (truezero) {
    if (testmean == 0) {
      fail(sprintf("The mean of the variable is 0 - the standard concentration index is not defined in this case."))
    }
    if (!is.na(xmin) && xmin > 0) {
      fail("The lower bound for a ratio scale variable cannot be greater than 0.")
    }
  }

  ## --- generalized requires truezero (ado 170-180) ------------------------
  if (!truezero && gen_flag) {
    fail("The option truezero must be used when specifying the generalized option.")
  }

  ## --- bounded requires limits(min max); standardise h* (ado 182-203) -----
  bounded_flag <- isTRUE(bounded)
  if (bounded_flag) {
    if (is.na(xmax)) {
      fail("For bounded variables, limits must be specified as limits = c(min, max).")
    }
    if (is.na(xmin) || (!is.na(xmax) && (xmin > xmax || xmin == xmax))) {
      fail("For bounded variables, limits must be specified as limits = c(min, max).")
    }
    if (!is.na(xmin)) {
      if (rng_all[1] < xmin || rng_all[2] > xmax) {
        fail("The variable takes values outside of the specified limits.")
      }
      if (rng_all[1] >= xmin && rng_all[2] <= xmax) {
        h_star <- (h - xmin) / (xmax - xmin)
      }
    }
  }

  ## --- normalization flags (ado 204-216) ----------------------------------
  wag_flag <- isTRUE(wagstaff)
  err_flag <- isTRUE(erreygers)
  if (!bounded_flag && (err_flag || wag_flag)) {
    fail(paste0("Wagstaff and Erreygers Normalisations are only for use with bounded variables. ",
                "Use bounded = TRUE and limits = c(min, max)."))
  }
  if (err_flag && wag_flag) {
    fail("The option wagstaff cannot be used in conjunction with the option erreygers.")
  }

  ## --- v() => extended; beta() => symmetric; must be > 1 (ado 217-240) ----
  if (!is.null(v)) {
    if (!is.numeric(v) || length(v) != 1 || is.na(v)) {
      fail("For the option v, the value must be a number greater than 1.")
    }
    if (v <= 1) fail("For the option v, the value must not be less than 1.")
    extended <- TRUE
  }
  if (!is.null(beta)) {
    if (!is.numeric(beta) || length(beta) != 1 || is.na(beta)) {
      fail("For the option beta, the value must be a number greater than 1.")
    }
    if (beta <= 1) fail("For the option beta, the value must not be less than 1.")
    symmetric <- TRUE
  }

  ## --- mutual-exclusion rules (ado 242-255) -------------------------------
  if (extended && symmetric) {
    fail("The option v cannot be used in conjunction with the option beta.")
  }
  if ((extended || symmetric) && (err_flag || wag_flag)) {
    fail("Wagstaff and Erreygers Normalisations are not supported for extended/symmetric indices.")
  }
  if (gen_flag && (err_flag || wag_flag)) {
    fail("Cannot specify generalized in conjunction with Wagstaff or Erreygers Normalisations.")
  }

  ## --- values within limits when only a min is set (ado 257-270) ----------
  if (!is.na(xmin)) {
    if (rng_all[1] < xmin) {
      fail("The variable takes values outside of the specified limits.")
    }
    if (truezero) {
      message("Note: The option truezero has been specified in conjunction with the limits option.")
    }
  }

  ## --- default index = MODIFIED CI (ado 272-288) --------------------------
  if (!truezero && !extended && !symmetric && !err_flag && !wag_flag &&
      !gen_flag && !bounded_flag) {
    modified <- TRUE
    if (is.na(xmin) || !is.na(xmax)) {
      fail(paste0("For the modified concentration index, limits must be specified as ",
                  "limits = min (a single minimum value)."))
    }
    if (rng_all[1] == rng_all[2]) {
      fail("The modified concentration index cannot be computed since the variable is always equal to its minimum value.")
    }
  }

  ## --- extended/symmetric need truezero (ado 290-295) ---------------------
  if (!truezero && (extended || symmetric)) {
    fail("The extended and symmetric indices should be used for ratio-scale variables and hence truezero must be specified also.")
  }

  ## --- informational notes (ado 314-315) ----------------------------------
  if (gen_flag && extended) {
    message("Note: The extended index equals the Erreygers normalised CI when v = 2")
  }
  if (gen_flag && symmetric) {
    message("Note: The symmetric index equals the Erreygers normalised CI when beta = 2")
  }

  flags <- list(
    index = index, generalized = gen_flag, modified = modified,
    bounded = bounded_flag, wagstaff = wag_flag, erreygers = err_flag,
    extended = extended, symmetric = symmetric,
    v = if (is.null(v)) NA_real_ else v,
    beta = if (is.null(beta)) NA_real_ else beta,
    xmin = xmin, xmax = xmax, svy = isTRUE(svy)
  )
  list(flags = flags, h_star = h_star)
}
