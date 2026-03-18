test_that("spending functions return correct cumulative spending at boundaries", {
  # At info_frac = 0 (implicit), spending = 0

  # At info_frac = 1, spending = alpha
  alpha <- 0.025
  t <- c(1/3, 2/3, 1)

  expect_equal(spending_of(alpha, 1), alpha)
  expect_equal(spending_pocock(alpha, 1), alpha)
  expect_equal(spending_hsd(alpha, 1), alpha)
  expect_equal(spending_linear(alpha, 1), alpha)

  expect_equal(tail(spending_of(alpha, t), 1), alpha)
  expect_equal(tail(spending_pocock(alpha, t), 1), alpha)
  expect_equal(tail(spending_hsd(alpha, t), 1), alpha)
  expect_equal(tail(spending_linear(alpha, t), 1), alpha)
})

test_that("spending functions are monotonically non-decreasing", {
  alpha <- 0.025
  t <- c(0.1, 0.3, 0.5, 0.7, 1)

  expect_true(all(diff(spending_of(alpha, t)) >= 0))
  expect_true(all(diff(spending_pocock(alpha, t)) >= 0))
  expect_true(all(diff(spending_hsd(alpha, t)) >= 0))
  expect_true(all(diff(spending_linear(alpha, t)) >= 0))
})

test_that("spending_linear returns alpha * t", {
  alpha <- 0.025
  t <- c(1/3, 2/3, 1)
  expect_equal(spending_linear(alpha, t), alpha * t)
})

test_that("spending_hsd gamma parameter works", {
  alpha <- 0.025
  t <- c(0.5, 1)

  # Different gamma values produce different spending
  s1 <- spending_hsd(alpha, t, gamma = -4)
  s2 <- spending_hsd(alpha, t, gamma = -1)
  s3 <- spending_hsd(alpha, t, gamma = 1)

  expect_false(isTRUE(all.equal(s1, s2)))
  expect_false(isTRUE(all.equal(s2, s3)))

  # All end at alpha

  expect_equal(s1[2], alpha)
  expect_equal(s2[2], alpha)
  expect_equal(s3[2], alpha)
})

test_that("OBF is more conservative than Pocock at early analyses", {
  alpha <- 0.025
  t <- c(1/3, 2/3, 1)

  of_spend <- spending_of(alpha, t)
  poc_spend <- spending_pocock(alpha, t)

  # OBF spends less at early analyses

  expect_true(of_spend[1] < poc_spend[1])
  expect_true(of_spend[2] < poc_spend[2])
})

test_that("spending functions match gsDesign", {
  skip_if_not_installed("gsDesign")

  alpha <- 0.025
  t_list <- list(c(1/3, 2/3, 1), c(0.5, 1), c(0.2, 0.6, 1))

  for (t in t_list) {
    expect_equal(
      spending_of(alpha, t),
      gsDesign::sfLDOF(alpha, t)$spend,
      tolerance = 1e-10
    )
    expect_equal(
      spending_pocock(alpha, t),
      gsDesign::sfLDPocock(alpha, t)$spend,
      tolerance = 1e-10
    )
    expect_equal(
      spending_hsd(alpha, t, gamma = -4),
      gsDesign::sfHSD(alpha, t, param = -4)$spend,
      tolerance = 1e-10
    )
    # spending_linear is simply alpha * t, verified in dedicated test above
  }
})
