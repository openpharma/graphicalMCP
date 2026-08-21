test_that("repeated_p returns a single numeric value", {
  rp <- repeated_p(c(0.024, 0.01), c(0.5, 1), spending_of)
  expect_length(rp, 1)
  expect_type(rp, "double")
})

test_that("repeated_p is in [0, 1] or slightly above 1", {
  # Normal case
  rp <- repeated_p(c(0.024, 0.01), c(0.5, 1), spending_of)
  expect_true(rp > 0 & rp <= 1)

  # Very large p-values should give repeated_p near 1
  rp_large <- repeated_p(c(0.5, 0.5), c(0.5, 1), spending_of)
  expect_true(rp_large >= 0.5)
})

test_that("smaller observed p-values give smaller repeated p-values", {
  rp1 <- repeated_p(c(0.05, 0.01), c(0.5, 1), spending_of)
  rp2 <- repeated_p(c(0.05, 0.005), c(0.5, 1), spending_of)
  expect_true(rp2 < rp1)
})

test_that("repeated_p inverts the boundary correctly", {
  p_obs <- c(0.05, 0.02, 0.01)
  t <- c(1 / 3, 2 / 3, 1)

  for (k in 1:3) {
    rp_k <- repeated_p(p_obs[1:k], t[1:k], spending_of)

    # Skip extreme values
    if (rp_k >= 1 - 1e-6 || rp_k <= 1e-6) next

    # At alpha = rp_k, the boundary at analysis k should equal p_obs[k]
    b <- graphicalMCP:::gs_boundaries(rp_k, t[1:k], spending_of)
    expect_equal(p_obs[k], b$bounds_nominal[k], tolerance = 1e-4)
  }
})

test_that("repeated_p works with different spending functions", {
  p <- c(0.02, 0.01)
  t <- c(0.5, 1)

  rp_of <- repeated_p(p, t, spending_of)
  rp_poc <- repeated_p(p, t, spending_pocock)
  rp_lin <- repeated_p(p, t, spending_linear)

  # All should be valid
  expect_true(rp_of > 0)
  expect_true(rp_poc > 0)
  expect_true(rp_lin > 0)

  # Different spending functions give different repeated p-values
  expect_false(isTRUE(all.equal(rp_of, rp_poc)))
})

test_that("repeated_p for single analysis equals the p-value", {
  # With one analysis at info_frac = 1, the boundary IS alpha.
  # So repeated_p = the observed p-value itself.
  p <- 0.01
  rp <- repeated_p(p, 1, spending_of)
  expect_equal(rp, p, tolerance = 1e-4)
})

test_that("repeated_p returns near 1 for very large p-values", {
  rp <- repeated_p(c(0.99, 0.99), c(0.5, 1), spending_of)
  expect_true(rp > 0.99)
})

test_that("repeated_p returns lower bound for very small p-values", {
  rp <- repeated_p(c(1e-15, 1e-15), c(0.5, 1), spending_of)
  expect_true(rp < 1e-4)
})

test_that("repeated_p returns 1 for p-values that never cross the boundary", {
  # p = 1 at all analyses: boundary never crossed
  expect_message(
    rp <- repeated_p(c(1, 1), c(0.5, 1), spending_of),
    "upper bound"
  )
  expect_equal(rp, 1)
})

test_that("repeated_p handles spending function that errors at small alpha", {
  # Create a spending function that errors for very small alpha
  bad_spending <- function(alpha, info_frac) {
    if (alpha < 1e-5) stop("too small")
    spending_of(alpha, info_frac)
  }

  # With a very small p-value, the lower bound check may trigger the error
  # The tryCatch should handle it gracefully
  expect_no_error({
    rp <- repeated_p(c(0.01, 0.005), c(0.5, 1), bad_spending)
  })
  expect_true(is.numeric(rp))
})

test_that("repeated_p handles single analysis", {
  rp <- repeated_p(0.01, 1, spending_of)
  expect_equal(rp, 0.01, tolerance = 1e-6)
})

test_that("repeated_p returns lower bound message for extremely small p", {
  expect_message(
    rp <- repeated_p(1e-20, 1, spending_of),
    "lower bound"
  )
  expect_true(rp <= 1e-6)
})
