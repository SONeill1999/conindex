# Contributing to conindex

Thanks for your interest in improving **conindex**. This package is a translation
of the Stata command of the same name, and the guiding principle is **numerical
fidelity to the original**.

## Ground rules

1. **Preserve behaviour.** Any change to the estimation code must keep the exact
   index values and standard errors produced by the Stata command. The oracle
   values live in `tests/testthat/test-oracle.R`.
2. **Add a test with every change.** New options or bug fixes need a `testthat`
   test. Prefer behaviour-invariant tests (see `test-indices.R`) that do not rely
   on the non-redistributable DHS data.
3. **Match the style.** Follow the tidyverse style guide; keep functions small and
   documented with roxygen2.

## Development workflow

```r
# from the package directory
devtools::load_all()
devtools::test()          # run the test suite
devtools::document()      # regenerate NAMESPACE / man pages
devtools::check()         # full R CMD check
```

### Running the oracle tests

The exact-value tests require the DHS Cambodia 2010 data, which is **not**
distributed with the package. If you have it locally, point the tests at it:

```r
Sys.setenv(CONINDEX_DATA_DIR = "/path/to/Data")
devtools::test()
```

Without the data these tests skip automatically.

## Reporting bugs

Open an issue with a minimal reproducible example (`reprex::reprex()`), the output
you got, and the output you expected — ideally the corresponding Stata result.

## Pull requests

* Branch from `main`, keep PRs focused.
* Ensure `devtools::check()` passes with no errors or warnings.
* Update `NEWS.md` under a top "development" heading.

By contributing you agree that your contributions are licensed under GPL-3.
