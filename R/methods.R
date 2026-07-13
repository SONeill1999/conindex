#' Print a concentration index
#'
#' @param x A `conindex` object.
#' @param digits Number of significant digits.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.conindex <- function(x, digits = 7, ...) {
  cat("\nConcentration index:", x$type, "\n")
  se_txt <- if (is.na(x$se)) "        ." else formatC(x$se, digits = digits, format = "g")
  p_txt <- if (is.na(x$p_value)) "     ." else formatC(x$p_value, digits = 4, format = "f")
  tab <- data.frame(
    N = x$n,
    Index = formatC(x$value, digits = digits, format = "g"),
    SE = se_txt,
    p = p_txt,
    check.names = FALSE
  )
  names(tab)[3] <- x$se_type
  print(tab, row.names = FALSE)
  if (!is.na(x$nclus)) {
    cat(sprintf("(Note: Std. error adjusted for %d clusters in %s)\n", x$nclus, x$cluster))
  }
  if (x$nunique != x$n) {
    rv <- if (is.null(x$rankvar)) x$outcome else x$rankvar
    cat(sprintf("(Note: Only %d unique values for %s)\n", x$nunique, rv))
  }
  if (is.na(x$se)) {
    cat("(Note: Standard errors for the extended and symmetric indices are not calculated.)\n")
  }
  invisible(x)
}

#' @rdname print.conindex
#' @export
print.conindex_compare <- function(x, digits = 7, ...) {
  NextMethod()
  cmp <- x$comparison
  cat("\nFor groups (", cmp$compare_var, "):\n", sep = "")
  for (g in cmp$groups) {
    cat(sprintf("  group %s: index = %s (n = %d)\n",
                as.character(g$group_value),
                formatC(g$value, digits = digits, format = "g"), g$n))
  }
  if (!is.null(cmp$Ftest)) {
    cat(sprintf("\nHo: index equal across groups (equal variances, small sample)\n"))
    cat(sprintf("  F(%d, %d) = %s, p = %s\n",
                cmp$Ftest$df1, cmp$Ftest$df2,
                formatC(cmp$Ftest$F, digits = digits, format = "g"),
                formatC(cmp$Ftest$p, digits = 4, format = "f")))
  }
  if (!is.null(cmp$ztest)) {
    cat(sprintf("Ho: diff = 0 (large sample)\n"))
    cat(sprintf("  Diff = %s, SE = %s, z = %s, p = %s\n",
                formatC(cmp$ztest$diff, digits = digits, format = "g"),
                formatC(cmp$ztest$se, digits = digits, format = "g"),
                formatC(cmp$ztest$z, digits = 4, format = "f"),
                formatC(cmp$ztest$p, digits = 4, format = "f")))
  }
  invisible(x)
}

#' Extract the concentration index estimate
#' @param object A `conindex` object.
#' @param ... Ignored.
#' @return The index value (numeric scalar).
#' @export
coef.conindex <- function(object, ...) {
  stats::setNames(object$value, object$type)
}

#' Summarise a concentration index
#' @param object A `conindex` object.
#' @param ... Ignored.
#' @return `object`, invisibly (prints a summary).
#' @export
summary.conindex <- function(object, ...) {
  print(object)
  invisible(object)
}
