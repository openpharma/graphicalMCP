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

# =============================================================================
# spending_wt tests
# =============================================================================

test_that("spending_wt returns alpha at info_frac = 1", {
  alpha <- 0.025
  expect_equal(spending_wt(alpha, 1, delta = 0), alpha, tolerance = 1e-4)
  expect_equal(spending_wt(alpha, 1, delta = 0.25), alpha, tolerance = 1e-4)
  expect_equal(spending_wt(alpha, 1, delta = 0.5), alpha, tolerance = 1e-4)
})

test_that("spending_wt returns 0 or alpha for edge cases", {
  # Single analysis at t=1 should return alpha
  expect_equal(spending_wt(0.025, 1, delta = 0.5), 0.025, tolerance = 1e-4)
})

test_that("spending_wt returns 0 when alpha = 0", {
  expect_equal(spending_wt(0, c(0.5, 1), delta = 0.25), c(0, 0))
})

test_that("spending_wt is monotonically non-decreasing", {
  alpha <- 0.025
  t <- c(0.2, 0.5, 0.8, 1)
  for (delta in c(0, 0.1, 0.25, 0.5)) {
    sp <- spending_wt(alpha, t, delta = delta)
    expect_true(all(diff(sp) >= -1e-10),
                label = paste("delta =", delta))
  }
})

test_that("spending_wt caps at alpha for info_frac > 1", {
  alpha <- 0.025
  sp <- spending_wt(alpha, c(0.5, 1.2), delta = 0.25)
  expect_equal(sp[2], alpha, tolerance = 1e-6)
})

test_that("spending_wt delta=0 gives OBF-like boundaries (conservative at early analyses)", {
  alpha <- 0.025
  t <- c(1/3, 2/3, 1)
  sp_obf <- spending_wt(alpha, t, delta = 0)
  # OBF spends very little at early analyses
  expect_true(sp_obf[1] < 0.001)
  expect_equal(sp_obf[3], alpha, tolerance = 1e-3)
})

test_that("spending_wt delta=0.5 gives Pocock-like boundaries (more uniform spending)", {
  alpha <- 0.025
  t <- c(1/3, 2/3, 1)
  sp_poc <- spending_wt(alpha, t, delta = 0.5)
  # Pocock spends more at early analyses than OBF
  sp_obf <- spending_wt(alpha, t, delta = 0)
  expect_true(sp_poc[1] > sp_obf[1])
  expect_equal(sp_poc[3], alpha, tolerance = 1e-3)
})

test_that("spending_wt produces Wang-Tsiatis boundary shape", {
  # c_k * t_k^(0.5 - delta) should be approximately constant
  alpha <- 0.025
  t <- c(1/3, 2/3, 1)
  for (delta in c(0, 0.25, 0.5)) {
    b <- gs_boundaries(alpha, t,
                       function(a, tf) spending_wt(a, tf, delta = delta))
    C_values <- b$bounds_z * t^(0.5 - delta)
    expect_true(max(C_values) - min(C_values) < 0.01,
                label = paste("delta =", delta))
  }
})

test_that("spending_wt matches rpact Wang-Tsiatis boundaries", {
  skip_if_not_installed("rpact")
  alpha <- 0.025
  t <- c(1/3, 2/3, 1)

  for (delta in c(0.1, 0.25, 0.4)) {
    b <- gs_boundaries(alpha, t,
                       function(a, tf) spending_wt(a, tf, delta = delta))
    rpact_gsd <- rpact::getDesignGroupSequential(
      sided = 1, alpha = alpha,
      informationRates = t,
      typeOfDesign = "WT",
      deltaWT = delta
    )
    expect_equal(b$bounds_z, rpact_gsd$criticalValues,
                 tolerance = 1e-3,
                 label = paste("delta =", delta))
  }
})

test_that("spending_wt delta=0 matches rpact OBF boundaries", {
  skip_if_not_installed("rpact")
  alpha <- 0.025
  t <- c(0.5, 1)

  b <- gs_boundaries(alpha, t,
                     function(a, tf) spending_wt(a, tf, delta = 0))
  rpact_gsd <- rpact::getDesignGroupSequential(
    sided = 1, alpha = alpha,
    informationRates = t,
    typeOfDesign = "OF"
  )
  expect_equal(b$bounds_z, rpact_gsd$criticalValues, tolerance = 1e-3)
})

test_that("spending_wt delta=0.5 matches rpact Pocock boundaries", {
  skip_if_not_installed("rpact")
  alpha <- 0.025
  t <- c(0.5, 1)

  b <- gs_boundaries(alpha, t,
                     function(a, tf) spending_wt(a, tf, delta = 0.5))
  rpact_gsd <- rpact::getDesignGroupSequential(
    sided = 1, alpha = alpha,
    informationRates = t,
    typeOfDesign = "P"
  )
  expect_equal(b$bounds_z, rpact_gsd$criticalValues, tolerance = 1e-3)
})

test_that("spending_wt errors with invalid inputs", {
  expect_error(spending_wt(0.025, c(-0.1, 1), delta = 0.5))
  expect_error(spending_wt(0.025, c(0.5, 1, 1.1), delta = 0.5))
})

test_that("spending_wt works in graph_test_shortcut_gsd", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(0.5, 1),
    spending_fn = function(a, t) spending_wt(a, t, delta = 0.25)
  )

  expect_s3_class(result, "gsd_graph_report")
  expect_length(result$outputs$rejected, 2)
})

# =============================================================================
# spending functions with info_frac > 1
# =============================================================================

test_that("spending functions cap at alpha for info_frac > 1", {
  alpha <- 0.025
  for (fn in list(spending_of, spending_pocock, spending_linear)) {
    sp <- fn(alpha, c(0.5, 1.2))
    expect_equal(sp[2], alpha)
  }
  sp_hsd <- spending_hsd(alpha, c(0.5, 1.2), gamma = -4)
  expect_equal(sp_hsd[2], alpha)
})

test_that("spending functions error with two info_frac >= 1", {
  alpha <- 0.025
  expect_error(spending_of(alpha, c(0.5, 1, 1.1)))
  expect_error(spending_pocock(alpha, c(0.5, 1, 1.1)))
  expect_error(spending_hsd(alpha, c(0.5, 1, 1.1)))
  expect_error(spending_linear(alpha, c(0.5, 1, 1.1)))
})

# =============================================================================
# spending_with_time tests
# =============================================================================

test_that("spending_with_time creates a valid spending function", {
  sf <- spending_with_time(spending_of, c(0.5, 1))
  expect_true(is.function(sf))
  result <- sf(0.025, c(0.6, 1.2))
  expect_length(result, 2)
  expect_equal(result[2], 0.025, tolerance = 1e-6)
})

test_that("spending_with_time errors with two spending_time >= 1", {
  expect_error(spending_with_time(spending_of, c(0.5, 1, 1.1)))
})

test_that("spending_with_time allows spending_time > 1 (single value)", {
  sf <- spending_with_time(spending_of, c(0.5, 1.1))
  result <- sf(0.025, c(0.6, 1.2))
  expect_equal(result[2], 0.025, tolerance = 1e-6)
})
