test_that("fractional rank has mean 0.5 and lies in (0, 1]", {
  df <- make_synth()
  fr <- conindex:::.fractional_rank(df$ses, rep(1, nrow(df)))
  expect_equal(mean(fr$frnk), 0.5, tolerance = 1e-8)
  expect_true(all(fr$frnk > 0 & fr$frnk <= 1))
})

test_that("tied ranks share the same fractional rank", {
  ranking <- c(1, 1, 2, 3, 3, 3)
  w <- rep(1, 6)
  fr <- conindex:::.fractional_rank(ranking, w)
  expect_equal(fr$frnk[1], fr$frnk[2])          # the two 1s
  expect_equal(fr$frnk[4], fr$frnk[5])          # the three 3s
  expect_equal(fr$frnk[5], fr$frnk[6])
  expect_equal(mean(fr$frnk), 0.5, tolerance = 1e-12)
})

test_that("weighted rank respects weights (heavier obs spans more mass)", {
  ranking <- c(1, 2, 3)
  fr_eq <- conindex:::.fractional_rank(ranking, c(1, 1, 1))
  fr_hi <- conindex:::.fractional_rank(ranking, c(10, 1, 1))
  # A heavy lowest-rank obs occupies more of the cumulative-weight interval, so
  # its mid-interval fractional rank is larger than under equal weights.
  expect_gt(fr_hi$frnk[1], fr_eq$frnk[1])
  # It still sits below the higher-ranked observations.
  expect_lt(fr_hi$frnk[1], fr_hi$frnk[2])
})

test_that("rank output is aligned to the original (unsorted) order", {
  ranking <- c(3, 1, 2)
  fr <- conindex:::.fractional_rank(ranking, rep(1, 3))
  # Lowest value (position 2) should get the smallest fractional rank.
  expect_equal(which.min(fr$frnk), 2L)
  expect_equal(which.max(fr$frnk), 1L)
})
