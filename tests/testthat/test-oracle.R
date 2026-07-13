# Exact-value regression tests against the Stata `conindex` output captured in
# `Stata/Results from Stata.smcl`. These require the (non-redistributable) DHS
# Cambodia 2010 data and skip automatically when it is unavailable.

test_that("healthexp indices match the Stata oracle", {
  skip_if_no_oracle()
  hh <- haven::read_dta(file.path(oracle_datadir(), "CDHS2010hh.dta"))

  r <- conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
                truezero = TRUE, cluster = "PSU")
  expect_equal(r$value, 0.24786719, tolerance = 1e-6)
  expect_equal(r$se,    0.07246288, tolerance = 1e-6)

  r <- conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
                generalized = TRUE, truezero = TRUE, cluster = "PSU")
  expect_equal(r$value, 2977.0381, tolerance = 1e-2)
  expect_equal(r$se,    870.32394, tolerance = 1e-2)

  r <- conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
                v = 1.5, truezero = TRUE, cluster = "PSU")
  expect_equal(r$value, 0.16955602, tolerance = 1e-6)
  expect_true(is.na(r$se))

  r <- conindex(hh, "healthexp", rankvar = "wealthindex", weights = "sampweight_hh",
                beta = 5, truezero = TRUE, cluster = "PSU")
  expect_equal(r$value, 0.39431835, tolerance = 1e-6)
})

test_that("under-1 mortality/survival indices match the Stata oracle", {
  skip_if_no_oracle()
  kids <- haven::read_dta(file.path(oracle_datadir(), "CDHS2010kids.dta"))
  kids$u1sr <- 1 - kids$u1mr

  r <- conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
                truezero = TRUE, cluster = "PSU")
  expect_equal(r$value, -0.18890669, tolerance = 1e-6)
  expect_equal(r$se,     0.02546028, tolerance = 1e-6)

  r <- conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
                erreygers = TRUE, bounded = TRUE, limits = c(0, 1), cluster = "PSU")
  expect_equal(r$value, -0.04586189, tolerance = 1e-6)
  expect_equal(r$se,     0.00618113, tolerance = 1e-6)

  r <- conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
                wagstaff = TRUE, bounded = TRUE, limits = c(0, 1), cluster = "PSU")
  expect_equal(r$value, -0.20111301, tolerance = 1e-6)
  expect_equal(r$se,     0.02710541, tolerance = 1e-6)

  r <- conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
                generalized = TRUE, truezero = TRUE, beta = 5, cluster = "PSU")
  expect_equal(r$value, -0.06051755, tolerance = 1e-6)
})

test_that("by = urban matches the Stata `bys urban:` oracle values", {
  skip_if_no_oracle()
  kids <- haven::read_dta(file.path(oracle_datadir(), "CDHS2010kids.dta"))

  r <- conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
                erreygers = TRUE, bounded = TRUE, limits = c(0, 1),
                cluster = "PSU", by = "urban")
  # From `Stata/Results from Stata.smcl` (bys urban: ... erreygers).
  expect_equal(r$results[["0"]]$value, -0.02985274, tolerance = 1e-6)
  expect_equal(r$results[["0"]]$se,     0.00724954, tolerance = 1e-6)
  expect_equal(r$results[["1"]]$value, -0.02869979, tolerance = 1e-6)
  expect_equal(r$results[["1"]]$se,     0.01006162, tolerance = 1e-6)
  # Displayed p-value uses (#clusters - 1) df, as Stata's vce(cluster) does.
  expect_equal(round(r$results[["1"]]$p_value, 4), 0.0048)
})

test_that("compare(urban) matches the Stata oracle F- and z-tests", {
  skip_if_no_oracle()
  kids <- haven::read_dta(file.path(oracle_datadir(), "CDHS2010kids.dta"))

  rc <- conindex(kids, "u1mr", rankvar = "wealthindex", weights = "sampweight",
                 erreygers = TRUE, bounded = TRUE, limits = c(0, 1),
                 cluster = "PSU", compare = "urban")
  expect_equal(rc$comparison$Ftest$F, 0.66985873, tolerance = 1e-4)
  expect_equal(rc$comparison$Ftest$p, 0.4131, tolerance = 1e-3)
  expect_equal(rc$comparison$ztest$CI0, -0.02985274, tolerance = 1e-6)
  expect_equal(rc$comparison$ztest$CI1, -0.02869979, tolerance = 1e-6)
  expect_equal(rc$comparison$ztest$p, 0.9259, tolerance = 1e-3)
})
