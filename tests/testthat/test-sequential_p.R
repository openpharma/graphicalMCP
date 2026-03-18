test_that("sequential_p returns a single numeric value", {
  sp <- sequential_p(c(0.024, 0.01), c(0.5, 1), spending_of)
  expect_length(sp, 1)
  expect_type(sp, "double")
})

test_that("sequential_p <= repeated_p at the same analysis", {
  p <- c(0.05, 0.02, 0.01)
  t <- c(1/3, 2/3, 1)

  rp <- repeated_p(p, t, spending_of)
  sp <- sequential_p(p, t, spending_of)

  expect_true(sp <= rp + 1e-6)
})

test_that("sequential_p equals cummin of repeated_p", {
  p <- c(0.05, 0.02, 0.01)
  t <- c(1/3, 2/3, 1)

  rep_p_vals <- numeric(3)
  seq_p_vals <- numeric(3)
  for (k in 1:3) {
    rep_p_vals[k] <- repeated_p(p[1:k], t[1:k], spending_of)
    seq_p_vals[k] <- sequential_p(p[1:k], t[1:k], spending_of)
  }

  expect_equal(seq_p_vals, cummin(rep_p_vals), tolerance = 1e-4)
})

test_that("sequential_p equals repeated_p for single analysis", {
  p <- 0.01
  t <- 0.5

  rp <- repeated_p(p, t, spending_of)
  sp <- sequential_p(p, t, spending_of)

  expect_equal(sp, rp, tolerance = 1e-6)
})

test_that("sequential_p is non-increasing across analyses", {
  p <- c(0.05, 0.02, 0.01)
  t <- c(1/3, 2/3, 1)

  seq_vals <- numeric(3)
  for (k in 1:3) {
    seq_vals[k] <- sequential_p(p[1:k], t[1:k], spending_of)
  }

  expect_true(all(diff(seq_vals) <= 1e-6))
})

test_that("sequential_p returns near 1 for very large p-values", {
  sp <- sequential_p(c(0.99, 0.99), c(0.5, 1), spending_of)
  expect_true(sp > 0.99)
})

test_that("sequential_p returns lower bound for very small p-values", {
  sp <- sequential_p(c(1e-15, 1e-15), c(0.5, 1), spending_of)
  expect_true(sp < 1e-4)
})

test_that("sequential_p matches gsDesign::sequentialPValue", {
  skip_if_not_installed("gsDesign")

  scenarios <- list(
    list(p = c(0.024, 0.01), t = c(0.5, 1), sfu = gsDesign::sfLDOF,
         fn = spending_of),
    list(p = c(0.05, 0.02, 0.01), t = c(1/3, 2/3, 1), sfu = gsDesign::sfLDOF,
         fn = spending_of),
    list(p = c(0.024, 0.01), t = c(0.5, 1), sfu = gsDesign::sfLDPocock,
         fn = spending_pocock)
  )

  for (sc in scenarios) {
    K <- length(sc$p)
    z_obs <- qnorm(1 - sc$p)

    sp <- sequential_p(sc$p, sc$t, sc$fn)

    gsd_d <- gsDesign::gsDesign(
      k = K, test.type = 1, alpha = 0.025,
      sfu = sc$sfu, timing = sc$t[-K]
    )
    gsd_sp <- gsDesign::sequentialPValue(
      gsD = gsd_d, n.I = gsd_d$n.I, Z = z_obs
    )

    expect_equal(sp, gsd_sp, tolerance = 1e-4)
  }
})
