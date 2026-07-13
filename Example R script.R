################################################################################
# Example R script.R
#
# R equivalent of "Stata/Example do file.do", using the conindex R package.
################################################################################

## ---- Setup -----------------------------------------------------------------
proj <- "C:/Asus Laptop/Projects/124) Conindex for R"

# Load the package. Use load_all() while developing; once installed you can
# simply use library(conindex).
if (requireNamespace("conindex", quietly = TRUE)) {
  library(conindex)
} else {
  devtools::load_all(file.path(proj, "conindex"))
}
library(haven)   # read Stata .dta files

# Optional: mirror Stata's `log using` by teeing console output to a file.
# sink(file.path(proj, "Results from R.txt"), split = TRUE)

## ---- Small helpers (Stata xtile / graph analogues) -------------------------

# Weighted quantiles (analogue of xtile with pweights).
wtd_quantile <- function(x, w, probs) {
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- (cumsum(w) - 0.5 * w) / sum(w)
  stats::approx(cw, x, xout = probs, rule = 2)$y
}
wtd_quintile <- function(x, w, n = 5) {
  ok <- !is.na(x) & !is.na(w)
  brks <- wtd_quantile(x[ok], w[ok], probs = seq(0, 1, length.out = n + 1))
  out <- rep(NA_integer_, length(x))
  out[ok] <- as.integer(cut(x[ok], breaks = brks, include.lowest = TRUE, labels = FALSE))
  out
}

# Concentration curve (what Stata's `graph` option drew via lorenz.ado):
# cumulative share of the outcome against cumulative population share, ranked
# by the socioeconomic variable.
conc_curve <- function(data, outcome, rankvar, weights, main = "") {
  keep <- !is.na(data[[outcome]]) & !is.na(data[[rankvar]]) & !is.na(data[[weights]])
  d <- data[keep, ]
  o <- order(d[[rankvar]])
  h <- d[[outcome]][o]; w <- d[[weights]][o]
  popshare <- cumsum(w) / sum(w)
  hshare   <- cumsum(w * h) / sum(w * h)
  plot(c(0, popshare), c(0, hshare), type = "l", xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Cumulative population share (ranked by SES)",
       ylab = paste("Cumulative share of", outcome), main = main)
  abline(0, 1, lty = 2)   # line of equality
}

################################################################################
# Indices in Table 1 for healthexp:
################################################################################
hh <- read_dta(file.path(proj, "Data", "CDHS2010hh.dta"))

# sum healthexp [aweight=sampweight_hh]
cat("healthexp:  n =", sum(!is.na(hh$healthexp)),
    "  weighted mean =", weighted.mean(hh$healthexp, hh$sampweight_hh, na.rm = TRUE), "\n")

# xtile wealthquint_hh = wealthindex [pweight=sampweight_hh], n(5)
hh$wealthquint_hh <- wtd_quintile(hh$wealthindex, hh$sampweight_hh, n = 5)

# graph bar (mean) healthexp [pweight = sampweight_hh], over(wealthquint_hh)
bar_means <- sapply(sort(unique(na.omit(hh$wealthquint_hh))), function(q) {
  i <- which(hh$wealthquint_hh == q)
  weighted.mean(hh$healthexp[i], hh$sampweight_hh[i], na.rm = TRUE)
})
barplot(bar_means, names.arg = paste0("Q", seq_along(bar_means)),
        main = "Mean healthexp by wealth quintile", ylab = "Mean healthexp")

# In Stata, aweights were not supported by lorenz, so the concentration curve
# used pweights. Here conc_curve() is weight-agnostic and works with either.
print(conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
               truezero = TRUE, cluster = "PSU"))
conc_curve(hh, "healthexp", "wealthindex", "sampweight_hh",
           main = "Concentration curve: healthexp")

print(conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
               generalized = TRUE, truezero = TRUE, cluster = "PSU"))
print(conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
               v = 1.5, truezero = TRUE, cluster = "PSU"))
print(conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
               v = 5, truezero = TRUE, cluster = "PSU"))
print(conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
               beta = 1.5, truezero = TRUE, cluster = "PSU"))
print(conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
               beta = 5, truezero = TRUE, cluster = "PSU"))

################################################################################
# Indices in Table 1 for u1mr/u1sr:
################################################################################
kids <- read_dta(file.path(proj, "Data", "CDHS2010kids.dta"))

# sum u1mr [aweight=sampweight]
cat("u1mr:  n =", sum(!is.na(kids$u1mr)),
    "  weighted mean =", weighted.mean(kids$u1mr, kids$sampweight, na.rm = TRUE), "\n")

# xtile wealthquint = wealthindex [pweight=sampweight], n(5)
kids$wealthquint <- wtd_quintile(kids$wealthindex, kids$sampweight, n = 5)

# graph bar (mean) u1mr [pweight = sampweight], over(wealthquint)
bar_means <- sapply(sort(unique(na.omit(kids$wealthquint))), function(q) {
  i <- which(kids$wealthquint == q)
  weighted.mean(kids$u1mr[i], kids$sampweight[i], na.rm = TRUE)
})
barplot(bar_means, names.arg = paste0("Q", seq_along(bar_means)),
        main = "Mean u1mr by wealth quintile", ylab = "Mean u1mr")

# gen u1sr = 1 - u1mr
kids$u1sr <- 1 - kids$u1mr

print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               truezero = TRUE, cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               truezero = TRUE, cluster = "PSU"))
print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               generalized = TRUE, truezero = TRUE, cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               generalized = TRUE, truezero = TRUE, cluster = "PSU"))

print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               erreygers = TRUE, bounded = TRUE, limits = c(0, 1), cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               erreygers = TRUE, bounded = TRUE, limits = c(0, 1), cluster = "PSU"))
print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               wagstaff = TRUE, bounded = TRUE, limits = c(0, 1), cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               wagstaff = TRUE, bounded = TRUE, limits = c(0, 1), cluster = "PSU"))

print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               truezero = TRUE, v = 1.5, cluster = "PSU"))
print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               truezero = TRUE, beta = 1.5, cluster = "PSU"))
print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               truezero = TRUE, v = 5, cluster = "PSU"))
print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               truezero = TRUE, beta = 5, cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               truezero = TRUE, v = 1.5, cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               truezero = TRUE, beta = 1.5, cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               truezero = TRUE, v = 5, cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               truezero = TRUE, beta = 5, cluster = "PSU"))

print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               generalized = TRUE, truezero = TRUE, v = 1.5, cluster = "PSU"))
print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               generalized = TRUE, truezero = TRUE, beta = 1.5, cluster = "PSU"))
print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               generalized = TRUE, truezero = TRUE, v = 5, cluster = "PSU"))
print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               generalized = TRUE, truezero = TRUE, beta = 5, cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               generalized = TRUE, truezero = TRUE, v = 1.5, cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               generalized = TRUE, truezero = TRUE, beta = 1.5, cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               generalized = TRUE, truezero = TRUE, v = 5, cluster = "PSU"))
print(conindex(kids, "u1sr", rankvar = "wealthindex", weights = "sampweight",
               generalized = TRUE, truezero = TRUE, beta = 5, cluster = "PSU"))

################################################################################
# Compare option:
################################################################################
# conindex ... compare(urban)   -> compare = "urban"
print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               erreygers = TRUE, bounded = TRUE, limits = c(0, 1),
               cluster = "PSU", compare = "urban"))

# bys urban: conindex ...       -> by = "urban"
print(conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
               erreygers = TRUE, bounded = TRUE, limits = c(0, 1),
               cluster = "PSU", by = "urban"))

# sink()   # close the log if opened above
