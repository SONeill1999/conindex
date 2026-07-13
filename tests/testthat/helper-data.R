# Synthetic fixture used by the behaviour-invariant tests. Deterministic.
make_synth <- function(n = 400, seed = 42) {
  set.seed(seed)
  ses <- rnorm(n)
  # Outcome mildly correlated with SES so the index is non-trivial and non-zero.
  h <- pmax(0.01, 2 + 0.5 * ses + rnorm(n))
  data.frame(
    h = h,
    hbin = as.numeric(h > stats::median(h)),   # bounded [0,1] outcome
    ses = ses,
    wt = runif(n, 0.5, 1.5),
    grp = rep(0:1, length.out = n)
  )
}

# Locate the local (non-redistributable) DHS data used for the exact Stata
# oracle tests. Returns NULL if not present, so the oracle tests skip cleanly
# on CI / other machines.
oracle_datadir <- function() {
  candidates <- c(
    Sys.getenv("CONINDEX_DATA_DIR", unset = NA),
    file.path(dirname(dirname(getwd())), "Data"),           # tests/testthat -> pkg -> project? adjusted below
    "C:/Asus Laptop/Projects/124) Conindex for R/Data"
  )
  for (d in candidates) {
    if (!is.na(d) && file.exists(file.path(d, "CDHS2010hh.dta"))) return(d)
  }
  NULL
}

skip_if_no_oracle <- function() {
  if (is.null(oracle_datadir())) {
    testthat::skip("DHS oracle data not available (set CONINDEX_DATA_DIR).")
  }
  if (!requireNamespace("haven", quietly = TRUE)) {
    testthat::skip("haven not installed.")
  }
}
