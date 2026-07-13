# Live cross-check against Stata via RStata. This is heavy (launches Stata) and
# requires commercial software + the non-redistributable DHS data, so it is
# OPT-IN: set CONINDEX_RUN_STATA=1 (and, if needed, CONINDEX_STATA_PATH) to run.
# It always skips on CI and on machines without Stata.

stata_path_guess <- function() {
  p <- Sys.getenv("CONINDEX_STATA_PATH", unset = "")
  if (nzchar(p)) return(p)
  cand <- Sys.glob(c("C:/Program Files/Stata*/Stata*-64.exe",
                     "C:/Program Files/Stata*/Stata*.exe"))
  if (length(cand)) shQuote(sub("\\.exe$", "", cand[1])) else ""
}

skip_if_no_stata <- function() {
  if (!identical(Sys.getenv("CONINDEX_RUN_STATA"), "1")) {
    testthat::skip("Set CONINDEX_RUN_STATA=1 to run the live Stata cross-check.")
  }
  if (!requireNamespace("RStata", quietly = TRUE)) testthat::skip("RStata not installed.")
  if (!nzchar(stata_path_guess())) testthat::skip("No Stata executable found.")
  skip_if_no_oracle()
}

run_stata_ci <- function(dofile_body) {
  options("RStata.StataPath" = stata_path_guess())
  options("RStata.StataVersion" = 18)
  out <- utils::capture.output(RStata::stata(dofile_body, stata.echo = TRUE))
  l <- grep("^CIXR\\|", out, value = TRUE)
  p <- strsplit(sub("^CIXR\\|", "", l[1]), "\\|")[[1]]
  list(ci = suppressWarnings(as.numeric(trimws(p[2]))),
       cise = suppressWarnings(as.numeric(trimws(p[3]))))
}

test_that("R matches live Stata for the standard concentration index", {
  skip_if_no_stata()
  proj <- dirname(oracle_datadir())
  do_txt <- paste(
    sprintf('cd "%s"', proj), "set linesize 255", 'do "Stata/conindex.ado"',
    'use "Data/CDHS2010hh.dta", clear',
    'quietly conindex healthexp [pweight=sampweight_hh], rankvar(wealthindex) truezero cluster(PSU)',
    'display "CIXR|hh_CI|" %21.15e r(CI) "|" %21.15e r(CIse) "|" r(N)',
    sep = "\n")
  st <- run_stata_ci(do_txt)

  hh <- haven::read_dta(file.path(oracle_datadir(), "CDHS2010hh.dta"))
  r <- conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
                truezero = TRUE, cluster = "PSU")
  expect_equal(r$value, st$ci, tolerance = 1e-6)
  expect_equal(r$se, st$cise, tolerance = 1e-6)
})

test_that("R matches live Stata for the Erreygers index", {
  skip_if_no_stata()
  proj <- dirname(oracle_datadir())
  do_txt <- paste(
    sprintf('cd "%s"', proj), "set linesize 255", 'do "Stata/conindex.ado"',
    'use "Data/CDHS2010kids.dta", clear',
    'quietly conindex u1mr [aweight=sampweight], rankvar(wealthindex) erreygers bounded limits(0 1) cluster(PSU)',
    'display "CIXR|kids_err|" %21.15e r(CI) "|" %21.15e r(CIse) "|" r(N)',
    sep = "\n")
  st <- run_stata_ci(do_txt)

  kids <- haven::read_dta(file.path(oracle_datadir(), "CDHS2010kids.dta"))
  r <- conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
                erreygers = TRUE, bounded = TRUE, limits = c(0, 1), cluster = "PSU")
  expect_equal(r$value, st$ci, tolerance = 1e-6)
  expect_equal(r$se, st$cise, tolerance = 1e-6)
})
