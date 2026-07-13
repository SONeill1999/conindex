test_that("by returns one result per group", {
  df <- make_synth()
  r <- conindex(df, "h", rankvar = "ses", truezero = TRUE, by = "grp")
  expect_s3_class(r, "conindex_by")
  expect_length(r$results, length(unique(df$grp)))
  expect_true(all(vapply(r$results, inherits, logical(1), "conindex")))
})

test_that("by-group indices equal the compare() group indices", {
  # by: and compare() estimate the same per-group indices; only the reporting
  # (and homogeneity tests) differ.
  df <- make_synth()
  byr <- conindex(df, "h", rankvar = "ses", truezero = TRUE, by = "grp")
  cmp <- conindex(df, "h", rankvar = "ses", truezero = TRUE, compare = "grp")
  expect_equal(byr$results[["0"]]$value, cmp$comparison$groups[[1]]$value, tolerance = 1e-10)
  expect_equal(byr$results[["1"]]$value, cmp$comparison$groups[[2]]$value, tolerance = 1e-10)
})

test_that("by and compare cannot be combined", {
  df <- make_synth()
  expect_error(
    conindex(df, "h", rankvar = "ses", truezero = TRUE, by = "grp", compare = "grp"),
    "cannot be used in conjunction with by"
  )
})

test_that("by respects a user subset and drops by-variable missings", {
  df <- make_synth()
  df$grp[1:5] <- NA
  sub <- df$ses > -5   # trivially TRUE, exercises the subset path
  r <- conindex(df, "h", rankvar = "ses", truezero = TRUE, by = "grp", subset = sub)
  # Rows with NA group are excluded, so total n across groups < nrow.
  total_n <- sum(vapply(r$results, function(x) x$n, numeric(1)))
  expect_equal(total_n, sum(!is.na(df$grp)))
})

test_that("as.data.frame summarises by-groups", {
  df <- make_synth()
  r <- conindex(df, "h", rankvar = "ses", truezero = TRUE, by = "grp")
  d <- as.data.frame(r)
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 2)
  expect_setequal(names(d), c("level", "type", "index", "se", "n"))
})
