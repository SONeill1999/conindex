test_that("unknown columns raise informative errors", {
  df <- make_synth()
  expect_error(conindex(df, "nope", rankvar = "ses", truezero = TRUE), "not found")
  expect_error(conindex(df, "h", rankvar = "nope", truezero = TRUE), "not found")
})

test_that("generalized requires truezero", {
  df <- make_synth()
  expect_error(
    conindex(df, "h", rankvar = "ses", generalized = TRUE),
    "truezero"
  )
})

test_that("extended / symmetric require truezero", {
  df <- make_synth()
  expect_error(conindex(df, "h", rankvar = "ses", v = 1.5), "truezero")
  expect_error(conindex(df, "h", rankvar = "ses", beta = 1.5), "truezero")
})

test_that("v and beta must exceed 1", {
  df <- make_synth()
  expect_error(conindex(df, "h", rankvar = "ses", truezero = TRUE, v = 1), "less than 1")
  expect_error(conindex(df, "h", rankvar = "ses", truezero = TRUE, beta = 0.5), "less than 1")
})

test_that("v and beta cannot be combined", {
  df <- make_synth()
  expect_error(
    conindex(df, "h", rankvar = "ses", truezero = TRUE, v = 1.5, beta = 1.5),
    "cannot be used in conjunction"
  )
})

test_that("Wagstaff/Erreygers require bounded", {
  df <- make_synth()
  expect_error(
    conindex(df, "hbin", rankvar = "ses", erreygers = TRUE),
    "bounded"
  )
  expect_error(
    conindex(df, "hbin", rankvar = "ses", wagstaff = TRUE),
    "bounded"
  )
})

test_that("Wagstaff and Erreygers are mutually exclusive", {
  df <- make_synth()
  expect_error(
    conindex(df, "hbin", rankvar = "ses", bounded = TRUE, limits = c(0, 1),
             wagstaff = TRUE, erreygers = TRUE),
    "cannot be used in conjunction"
  )
})

test_that("bounded requires a max limit", {
  df <- make_synth()
  expect_error(
    conindex(df, "hbin", rankvar = "ses", bounded = TRUE, limits = 0),
    "min, max"
  )
})

test_that("bounded rejects out-of-range values", {
  df <- make_synth()
  expect_error(
    conindex(df, "h", rankvar = "ses", bounded = TRUE, limits = c(0, 1)),
    "outside of the specified limits"
  )
})

test_that("modified index (default) requires a single minimum limit", {
  df <- make_synth()
  # No index-selecting option + no limits -> modified path complains.
  expect_error(conindex(df, "h", rankvar = "ses"), "single minimum value")
  # Providing a max as well is invalid for the modified index.
  expect_error(conindex(df, "h", rankvar = "ses", limits = c(0, 5)), "single minimum value")
})

test_that("svy without a design errors", {
  df <- make_synth()
  expect_error(conindex(df, "h", rankvar = "ses", truezero = TRUE, svy = TRUE),
               "survey design")
})
