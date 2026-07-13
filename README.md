# conindex <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/SONeill1999/conindex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SONeill1999/conindex/actions/workflows/R-CMD-check.yaml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

**conindex** computes rank-dependent inequality (concentration) indices used to
measure socioeconomic inequality in health and economic outcomes. It is a faithful
R translation of the Stata command [`conindex`](https://doi.org/10.1177/1536867X1601600112)
(O'Donnell, O'Neill, Van Ourti & Walsh, 2016) — index values and standard errors
reproduce the original to displayed precision. The translation used https://github.com/stata-translations/Stata2R (Thompson, S., Tamuri, A.& Pérez-Suárez, D. (2026). Stata2R (Version 0.0.1) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.21340235)

## Installation

```r
# install.packages("remotes")
remotes::install_github("SONeill1999/conindex")
```

## Supported indices

| Index | Options |
|-------|---------|
| Gini coefficient | `truezero` (no `rankvar`) |
| Generalized Gini | `truezero, generalized` |
| Concentration index (CI) | `rankvar, truezero` |
| Generalized CI | `rankvar, truezero, generalized` |
| Modified CI | `rankvar, limits = min` |
| Wagstaff (bounded) | `rankvar, bounded, limits = c(min,max), wagstaff` |
| Erreygers (bounded) | `rankvar, bounded, limits = c(min,max), erreygers` |
| Extended CI | `rankvar, truezero, v` |
| Symmetric CI | `rankvar, truezero, beta` |

Robust (HC1), cluster-robust, and (experimentally) survey-linearised standard
errors are supported, along with across-group comparison via `compare`.

## Quick start

```r
library(conindex)

set.seed(1)
df <- data.frame(ses = rnorm(500), wt = runif(500, 0.5, 1.5))
df$h <- pmax(0.05, 2 + 0.6 * df$ses + rnorm(500))

# Standard concentration index, cluster-robust SE
conindex(df, "h", rankvar = "ses", weights = "wt", truezero = TRUE)

# Erreygers-normalised index for a bounded outcome
df$hbin <- as.numeric(df$h > median(df$h))
conindex(df, "hbin", rankvar = "ses", bounded = TRUE, limits = c(0, 1),
         erreygers = TRUE)

# Point-and-click GUI
# run_conindex_app(df)
```

See `vignette("conindex")` for a full walkthrough.

## Relationship to other R packages

The [**rineq**](https://cran.r-project.org/package=rineq) package covers the
standard, generalized, Erreygers and Wagstaff indices and adds decomposition.
**conindex** additionally provides the modified, extended and symmetric indices,
clustered/survey standard errors, and the `compare` group tests, mirroring the
Stata command's interface.

## Citation

Please cite O'Donnell, O., O'Neill, S., Van Ourti, T. & Walsh, B. (2016).
conindex: Estimation of concentration indices. *The Stata Journal* 16(1), 112-138.

## License

GPL-3 © the conindex authors.
