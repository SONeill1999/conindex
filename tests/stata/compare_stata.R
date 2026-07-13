#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Live Stata-vs-R comparison for the conindex package.
#
# Drives Stata via the RStata package, runs each command from
# `Stata/Example do file.do` (the Stata "tests"), and compares r(CI), r(CIse)
# and the compare() statistics against the R conindex() equivalents.
#
# Requirements (all optional; the script stops cleanly if missing):
#   * Stata (set the path below or via env var CONINDEX_STATA_PATH)
#   * RStata, haven, devtools R packages
#   * The DHS Cambodia 2010 data in <project>/Data (not redistributable)
#
# Run:  Rscript tests/stata/compare_stata.R
# Output: tests/stata/comparison_results.csv and tests/stata/STATA_COMPARISON.md
# ---------------------------------------------------------------------------

# Project root: env override, else two levels up from this script, else default.
proj <- Sys.getenv("CONINDEX_PROJECT", unset = "")
if (!nzchar(proj) || !dir.exists(file.path(proj, "Data"))) {
  proj <- "C:/Asus Laptop/Projects/124) Conindex for R"
}
pkg <- file.path(proj, "conindex")

stata_path <- Sys.getenv("CONINDEX_STATA_PATH",
                         unset = '"C:\\Program Files\\Stata18\\StataSE-64"')
options("RStata.StataPath" = stata_path)
options("RStata.StataVersion" = 18)

for (p in c("RStata", "haven", "devtools")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' is required.", call. = FALSE)
}
if (!file.exists(file.path(proj, "Data", "CDHS2010hh.dta"))) {
  stop("DHS data not found under ", file.path(proj, "Data"), call. = FALSE)
}

suppressMessages(devtools::load_all(pkg, quiet = TRUE))
hh   <- haven::read_dta(file.path(proj, "Data", "CDHS2010hh.dta"))
kids <- haven::read_dta(file.path(proj, "Data", "CDHS2010kids.dta"))
kids$u1sr <- 1 - kids$u1mr

## ---- Stata command <-> R call correspondence -------------------------------
spec <- list(
  list(id="hh_CI", ds="hh",
       stata='conindex healthexp [pweight=sampweight_hh], rankvar(wealthindex) truezero cluster(PSU)',
       r=function(d) conindex(d,"healthexp",rankvar="wealthindex",weights="sampweight_hh",truezero=TRUE,cluster="PSU")),
  list(id="hh_GenCI", ds="hh",
       stata='conindex healthexp [aweight=sampweight_hh], rankvar(wealthindex) generalized truezero cluster(PSU)',
       r=function(d) conindex(d,"healthexp",rankvar="wealthindex",weights="sampweight_hh",generalized=TRUE,truezero=TRUE,cluster="PSU")),
  list(id="hh_Ext_v1.5", ds="hh",
       stata='conindex healthexp [aweight=sampweight_hh], rankvar(wealthindex) v(1.5) truezero cluster(PSU)',
       r=function(d) conindex(d,"healthexp",rankvar="wealthindex",weights="sampweight_hh",v=1.5,truezero=TRUE,cluster="PSU")),
  list(id="hh_Sym_b5", ds="hh",
       stata='conindex healthexp [aweight=sampweight_hh], rankvar(wealthindex) beta(5) truezero cluster(PSU)',
       r=function(d) conindex(d,"healthexp",rankvar="wealthindex",weights="sampweight_hh",beta=5,truezero=TRUE,cluster="PSU")),
  list(id="kids_CI_u1mr", ds="kids",
       stata='conindex u1mr [aweight=sampweight], rankvar(wealthindex) truezero cluster(PSU)',
       r=function(d) conindex(d,"u1mr",rankvar="wealthindex",weights="sampweight",truezero=TRUE,cluster="PSU")),
  list(id="kids_CI_u1sr", ds="kids",
       stata='conindex u1sr [aweight=sampweight], rankvar(wealthindex) truezero cluster(PSU)',
       r=function(d) conindex(d,"u1sr",rankvar="wealthindex",weights="sampweight",truezero=TRUE,cluster="PSU")),
  list(id="kids_GenCI_u1mr", ds="kids",
       stata='conindex u1mr [aweight=sampweight], rankvar(wealthindex) generalized truezero cluster(PSU)',
       r=function(d) conindex(d,"u1mr",rankvar="wealthindex",weights="sampweight",generalized=TRUE,truezero=TRUE,cluster="PSU")),
  list(id="kids_Erreygers", ds="kids",
       stata='conindex u1mr [aweight=sampweight], rankvar(wealthindex) erreygers bounded limits(0 1) cluster(PSU)',
       r=function(d) conindex(d,"u1mr",rankvar="wealthindex",weights="sampweight",erreygers=TRUE,bounded=TRUE,limits=c(0,1),cluster="PSU")),
  list(id="kids_Wagstaff", ds="kids",
       stata='conindex u1mr [aweight=sampweight], rankvar(wealthindex) wagstaff bounded limits(0 1) cluster(PSU)',
       r=function(d) conindex(d,"u1mr",rankvar="wealthindex",weights="sampweight",wagstaff=TRUE,bounded=TRUE,limits=c(0,1),cluster="PSU")),
  list(id="kids_Ext_v1.5", ds="kids",
       stata='conindex u1mr [aweight=sampweight], rankvar(wealthindex) truezero v(1.5) cluster(PSU)',
       r=function(d) conindex(d,"u1mr",rankvar="wealthindex",weights="sampweight",truezero=TRUE,v=1.5,cluster="PSU")),
  list(id="kids_Sym_b1.5", ds="kids",
       stata='conindex u1mr [aweight=sampweight], rankvar(wealthindex) truezero beta(1.5) cluster(PSU)',
       r=function(d) conindex(d,"u1mr",rankvar="wealthindex",weights="sampweight",truezero=TRUE,beta=1.5,cluster="PSU")),
  list(id="kids_GenExt_v1.5", ds="kids",
       stata='conindex u1mr [aweight=sampweight], rankvar(wealthindex) generalized truezero v(1.5) cluster(PSU)',
       r=function(d) conindex(d,"u1mr",rankvar="wealthindex",weights="sampweight",generalized=TRUE,truezero=TRUE,v=1.5,cluster="PSU")),
  list(id="kids_GenSym_b5", ds="kids",
       stata='conindex u1mr [aweight=sampweight], rankvar(wealthindex) generalized truezero beta(5) cluster(PSU)',
       r=function(d) conindex(d,"u1mr",rankvar="wealthindex",weights="sampweight",generalized=TRUE,truezero=TRUE,beta=5,cluster="PSU"))
)

mk_block <- function(s) {
  useline <- if (s$ds=="hh") 'use "Data/CDHS2010hh.dta", clear' else 'use "Data/CDHS2010kids.dta", clear'
  extra <- if (s$ds=="kids") "capture drop u1sr\ngen u1sr = 1 - u1mr" else ""
  paste(useline, extra, paste0("quietly ", s$stata),
        paste0('display "CIXR|', s$id, '|" %21.15e r(CI) "|" %21.15e r(CIse) "|" r(N)'),
        sep="\n")
}

# compare() case (returns F, CI0, CI1, z). One marker per line to avoid the
# 80-column console wrap that would truncate a combined display.
compare_block <- paste(
  'use "Data/CDHS2010kids.dta", clear',
  'quietly conindex u1mr [aweight=sampweight], rankvar(wealthindex) erreygers bounded limits(0 1) cluster(PSU) compare(urban)',
  'display "CMPF|"   %21.15e r(F)',
  'display "CMPCI0|" %21.15e r(CI0)',
  'display "CMPCI1|" %21.15e r(CI1)',
  'display "CMPZ|"   %21.15e r(z)',
  sep="\n")

do_txt <- paste(c(sprintf('cd "%s"', proj), 'set linesize 255', 'do "Stata/conindex.ado"',
                  vapply(spec, mk_block, character(1)), compare_block), collapse="\n")

message("Running ", length(spec), " index cases + 1 compare case through Stata via RStata ...")
out <- capture.output(RStata::stata(do_txt, stata.echo = TRUE))

## ---- Parse Stata output ----------------------------------------------------
num <- function(x) suppressWarnings(as.numeric(trimws(x)))   # "." -> NA (missing SE)
parse_ci <- function(l) { p <- strsplit(sub("^CIXR\\|","",l),"\\|")[[1]]
  list(id=p[1], ci=num(p[2]), cise=num(p[3]), n=num(p[4])) }
stata_res <- lapply(grep("^CIXR\\|", out, value=TRUE), parse_ci)
names(stata_res) <- vapply(stata_res, `[[`, character(1), "id")

reldiff <- function(a,b) if ((is.na(a)&&is.na(b))) 0 else abs(a-b)/max(1,abs(b))
rows <- lapply(spec, function(s) {
  d <- if (s$ds=="hh") hh else kids
  rr <- s$r(d); sr <- stata_res[[s$id]]
  cir <- reldiff(rr$value, sr$ci)
  ser <- if (is.na(rr$se)&&is.na(sr$cise)) 0 else reldiff(rr$se, sr$cise)
  ok <- isTRUE(cir < 1e-6) &&
        ((is.na(rr$se)&&is.na(sr$cise)) || isTRUE(ser < 1e-6)) &&
        isTRUE(rr$n == sr$n)
  data.frame(id=s$id, stata_CI=sr$ci, r_CI=rr$value, CI_reldiff=cir,
             stata_SE=sr$cise, r_SE=rr$se, SE_reldiff=ser, N=sr$n, PASS=ok,
             stringsAsFactors=FALSE)
})
res <- do.call(rbind, rows)

## compare() row
grab <- function(tag) { l <- grep(paste0("^", tag, "\\|"), out, value=TRUE)
  if (length(l)) num(sub(paste0("^", tag, "\\|"), "", l[1])) else NA_real_ }
cmp_row <- NULL
if (length(grep("^CMPF\\|", out))) {
  s_F <- grab("CMPF"); s_CI0 <- grab("CMPCI0"); s_CI1 <- grab("CMPCI1"); s_z <- grab("CMPZ")
  rc <- conindex(kids, "u1mr", rankvar="wealthindex", weights="sampweight",
                 erreygers=TRUE, bounded=TRUE, limits=c(0,1), cluster="PSU", compare="urban")
  cmp_row <- data.frame(
    stat=c("F","CI0","CI1","z"),
    stata=c(s_F, s_CI0, s_CI1, s_z),
    r=c(rc$comparison$Ftest$F, rc$comparison$ztest$CI0, rc$comparison$ztest$CI1, rc$comparison$ztest$z),
    stringsAsFactors=FALSE)
  cmp_row$reldiff <- abs(cmp_row$stata - cmp_row$r) / pmax(1, abs(cmp_row$stata))
  cmp_row$PASS <- cmp_row$reldiff < 1e-5
}

write.csv(res, file.path(pkg, "tests", "stata", "comparison_results.csv"), row.names=FALSE)
print(res, digits=8)
if (!is.null(cmp_row)) { cat("\ncompare(urban):\n"); print(cmp_row, digits=8) }
all_pass <- all(res$PASS) && (is.null(cmp_row) || all(cmp_row$PASS))
cat("\nALL PASS:", all_pass, "\n")

## ---- Write the human-readable summary --------------------------------------
fmt <- function(x) formatC(x, format="g", digits=8)
lines <- c(
  "# Stata <-> R test correspondence (conindex)",
  "",
  sprintf("Generated by `tests/stata/compare_stata.R` against Stata 18 via RStata on %s.", Sys.Date()),
  "",
  "Each row runs the Stata command from `Stata/Example do file.do` live and",
  "compares `r(CI)` / `r(CIse)` with the matching R `conindex()` call.",
  "Relative differences at the 1e-9 level or below are floating-point identical.",
  "",
  "| Stata test (id) | R equivalent | index | CI rel.diff | SE rel.diff | PASS |",
  "|---|---|---|---|---|---|"
)
for (i in seq_along(spec)) {
  s <- spec[[i]]; rw <- res[i,]
  lines <- c(lines, sprintf("| `%s` | `conindex(%s, ...)` | %s | %s | %s | %s |",
    s$id, s$ds,
    sub("^conindex \\w+.*?rankvar", "rankvar", s$stata),  # short label
    fmt(rw$CI_reldiff), if (is.na(rw$SE_reldiff)) "n/a" else fmt(rw$SE_reldiff),
    if (rw$PASS) "PASS" else "**FAIL**"))
}
if (!is.null(cmp_row)) {
  lines <- c(lines, "",
    "## compare(urban) — Erreygers, group homogeneity tests", "",
    "| statistic | Stata | R | rel.diff | PASS |", "|---|---|---|---|---|")
  for (j in seq_len(nrow(cmp_row))) lines <- c(lines,
    sprintf("| %s | %s | %s | %s | %s |", cmp_row$stat[j], fmt(cmp_row$stata[j]),
            fmt(cmp_row$r[j]), fmt(cmp_row$reldiff[j]), if (isTRUE(cmp_row$PASS[j])) "PASS" else "**FAIL**"))
}
lines <- c(lines, "", sprintf("**Overall: %s**", if (all_pass) "ALL PASS" else "FAILURES PRESENT"))

## Coverage gaps: Stata example commands / features NOT directly cross-checked.
lines <- c(lines, "",
  "## Coverage and gaps (missing tests)",
  "",
  "The cases above span every index *type* in the package. The following items",
  "from the Stata materials are **not** directly compared here:",
  "",
  "* **`bys urban: conindex ...`** (by-prefix, `Example do file.do` line 68) is now",
  "  implemented as the `by =` argument and verified against the `.smcl` `bys urban:`",
  "  values in `tests/testthat/test-oracle.R` (urban=0: -0.02985274, urban=1:",
  "  -0.02869979). No longer a gap.",
  "* **`graph`** (Lorenz / concentration curves) and **`keepgraphdata`** — visual",
  "  output with no numeric oracle in the `.smcl`; not cross-checked.",
  "* **`keeprank`** — the returned fractional rank is unit-tested in R",
  "  (`test-indices.R`) but not compared against a Stata-saved rank variable.",
  "* **`svy`** — experimental in the R port; the example do-file contains no `svy`",
  "  run, so there is no oracle to compare against.",
  "* **Plain `robust` and classical (non-cluster) SEs** — every example uses",
  "  `cluster(PSU)`; HC1 robust and classical OLS SEs are exercised only by the R",
  "  unit tests, not cross-checked against Stata here.",
  "* **`fweight`** — only `aweight`/`pweight` appear in the examples; integer",
  "  frequency weights are not cross-checked.",
  "* **Redundant permutations** (e.g. `healthexp v(5)`, `u1sr` variants of every",
  "  index) are omitted because they duplicate an index type already verified.")

writeLines(lines, file.path(pkg, "tests", "stata", "STATA_COMPARISON.md"))
cat("\nWrote STATA_COMPARISON.md and comparison_results.csv\n")
if (!all_pass) quit(status = 1)
