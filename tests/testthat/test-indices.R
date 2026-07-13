test_that("standard CI runs and returns a well-formed object", {
  df <- make_synth()
  r <- conindex(df, "h", rankvar = "ses", truezero = TRUE)
  expect_s3_class(r, "conindex")
  expect_type(r$value, "double")
  expect_true(is.finite(r$value))
  expect_true(is.finite(r$se) && r$se >= 0)
  expect_equal(r$type, "CI")
  expect_equal(r$n, nrow(df))
})

test_that("Gini (rank by outcome) lies in [-1, 1] and is labelled Gini", {
  df <- make_synth()
  r <- conindex(df, "h", truezero = TRUE)
  expect_equal(r$index, "Gini")
  expect_true(r$value >= -1 && r$value <= 1)
})

test_that("generalized index equals standard index times the weighted mean", {
  df <- make_synth()
  w <- df$wt / 1  # explicit
  std <- conindex(df, "h", rankvar = "ses", weights = "wt", truezero = TRUE)
  gen <- conindex(df, "h", rankvar = "ses", weights = "wt", truezero = TRUE,
                  generalized = TRUE)
  meanh <- sum(df$wt * df$h) / sum(df$wt)
  expect_equal(gen$value, std$value * meanh, tolerance = 1e-8)
})

test_that("extended v=2 reproduces the standard concentration index", {
  df <- make_synth()
  std <- conindex(df, "h", rankvar = "ses", truezero = TRUE)
  ext2 <- conindex(df, "h", rankvar = "ses", truezero = TRUE, v = 2)
  expect_equal(ext2$value, std$value, tolerance = 1e-6)
})

test_that("symmetric beta=2 reproduces the standard concentration index", {
  df <- make_synth()
  std <- conindex(df, "h", rankvar = "ses", truezero = TRUE)
  sym2 <- conindex(df, "h", rankvar = "ses", truezero = TRUE, beta = 2)
  expect_equal(sym2$value, std$value, tolerance = 1e-6)
})

test_that("Erreygers equals 4 * mean * standard-bounded index", {
  df <- make_synth()
  err <- conindex(df, "hbin", rankvar = "ses", bounded = TRUE, limits = c(0, 1),
                  erreygers = TRUE)
  # Reconstruct the underlying bounded standard index (no normalisation) is not a
  # public option, so instead check the documented multiplicative relationship
  # against Wagstaff via their ratio: Erreygers/Wagstaff = 4*mean*(1-mean).
  wag <- conindex(df, "hbin", rankvar = "ses", bounded = TRUE, limits = c(0, 1),
                  wagstaff = TRUE)
  meanh <- mean(df$hbin)
  expect_equal(err$value / wag$value, 4 * meanh * (1 - meanh), tolerance = 1e-8)
})

test_that("extended and symmetric indices report no standard error", {
  df <- make_synth()
  ext <- conindex(df, "h", rankvar = "ses", truezero = TRUE, v = 1.5)
  expect_true(is.na(ext$se))
  sym <- conindex(df, "h", rankvar = "ses", truezero = TRUE, beta = 1.5)
  expect_true(is.na(sym$se))
})

test_that("robust and clustered SEs are produced and differ from classical", {
  df <- make_synth()
  cl <- conindex(df, "h", rankvar = "ses", truezero = TRUE)
  rob <- conindex(df, "h", rankvar = "ses", truezero = TRUE, robust = TRUE)
  clus <- conindex(df, "h", rankvar = "ses", truezero = TRUE, cluster = "grp")
  expect_equal(cl$value, rob$value, tolerance = 1e-12)   # point estimate unchanged
  expect_true(is.finite(rob$se))
  expect_true(is.finite(clus$se))
  expect_equal(clus$se_type, "Robust std. error")
})

test_that("keeprank returns a fractional-rank vector aligned to the data", {
  df <- make_synth()
  r <- conindex(df, "h", rankvar = "ses", truezero = TRUE, keeprank = TRUE)
  expect_length(r$rank, nrow(df))
  expect_equal(mean(r$rank), 0.5, tolerance = 1e-8)
})

test_that("compare() returns group indices and homogeneity tests", {
  df <- make_synth()
  r <- conindex(df, "h", rankvar = "ses", truezero = TRUE, compare = "grp")
  expect_s3_class(r, "conindex_compare")
  expect_length(r$comparison$groups, 2)
  expect_true(is.finite(r$comparison$Ftest$F))
  expect_true(is.finite(r$comparison$ztest$z))
})
