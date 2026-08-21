test_that("gs_corr matches analytical formula", {
  t <- c(1 / 3, 2 / 3, 1)
  corr <- graphicalMCP:::gs_corr(t)
  expected <- outer(t, t, function(ti, tj) sqrt(pmin(ti, tj) / pmax(ti, tj)))

  expect_equal(corr, expected)
})

test_that("gs_corr returns identity for single analysis", {
  expect_equal(graphicalMCP:::gs_corr(1), matrix(1, 1, 1))
})

test_that("gs_corr is symmetric with unit diagonal", {
  t <- c(0.2, 0.5, 0.8, 1)
  corr <- graphicalMCP:::gs_corr(t)

  expect_true(isSymmetric(corr))
  expect_equal(diag(corr), rep(1, length(t)))
})

test_that("gs_corr values are in [0, 1]", {
  t <- c(0.1, 0.5, 1)
  corr <- graphicalMCP:::gs_corr(t)

  expect_true(all(corr >= 0 & corr <= 1))
})
