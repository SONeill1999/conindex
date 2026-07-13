# conindex 1.7.0

Initial R release — a translation of the Stata `conindex` command (v1.7, 12 March
2025). Index values and standard errors are validated against the original Stata
output.

Ported feature history from the Stata command:

* Core rank-dependent indices: Gini, concentration index, generalized, modified,
  Wagstaff, Erreygers, extended (`v`), symmetric (`beta`), and their generalized
  versions.
* `svy` option for complex survey designs (added to Stata 16 Feb 2016; provided
  here experimentally via the **survey** package).
* Graph support in Stata used `lorenz.ado` (18 July 2018); in R, curves are drawn
  natively — the hard dependency is dropped.

## New in the R port

* `compare()` group comparison with F- and z-tests.
* Robust (HC1) and cluster-robust standard errors matching Stata's `regress`.
* `run_conindex_app()` — a Shiny GUI (the Stata package shipped no dialog).
* `testthat` suite, including exact-value regression tests against the Stata
  oracle (run locally when the DHS data is available).
