test_that("gs_boundaries returns correct structure", {
  b <- graphicalMCP:::gs_boundaries(0.025, c(0.5, 1), spending_of)

  expect_type(b, "list")
  expect_named(b, c("bounds_z", "bounds_nominal"))
  expect_length(b$bounds_z, 2)
  expect_length(b$bounds_nominal, 2)
})

test_that("Z boundaries are decreasing (OBF) or flat (Pocock-like)", {
  b_of <- graphicalMCP:::gs_boundaries(0.025, c(1 / 3, 2 / 3, 1), spending_of)
  # OBF boundaries are strictly decreasing
  expect_true(all(diff(b_of$bounds_z) < 0))

  # Nominal p-value boundaries are increasing for OBF
  expect_true(all(diff(b_of$bounds_nominal) > 0))
})

test_that("nominal boundaries are consistent with Z boundaries", {
  b <- graphicalMCP:::gs_boundaries(0.025, c(0.5, 1), spending_of)
  expected_nominal <- pnorm(b$bounds_z, lower.tail = FALSE)
  expect_equal(b$bounds_nominal, expected_nominal)
})

test_that("gs_boundaries matches gsDesign", {
  skip_if_not_installed("gsDesign")

  test_cases <- list(
    list(
      alpha = 0.025, t = c(1 / 3, 2 / 3, 1), sfu = gsDesign::sfLDOF,
      fn = spending_of
    ),
    list(
      alpha = 0.025, t = c(0.5, 1), sfu = gsDesign::sfLDOF,
      fn = spending_of
    ),
    list(
      alpha = 0.025, t = c(1 / 3, 2 / 3, 1), sfu = gsDesign::sfLDPocock,
      fn = spending_pocock
    ),
    list(
      alpha = 0.01, t = c(0.2, 0.6, 1), sfu = gsDesign::sfLDOF,
      fn = spending_of
    )
  )

  for (tc in test_cases) {
    K <- length(tc$t)
    gsd_design <- gsDesign::gsDesign(
      k = K, test.type = 1, alpha = tc$alpha,
      sfu = tc$sfu, timing = tc$t[-K]
    )

    graphicalMCP_b <- graphicalMCP:::gs_boundaries(tc$alpha, tc$t, tc$fn)

    expect_equal(graphicalMCP_b$bounds_z, gsd_design$upper$bound, tolerance = 1e-3)
    expect_equal(
      graphicalMCP_b$bounds_nominal,
      pnorm(gsd_design$upper$bound, lower.tail = FALSE),
      tolerance = 1e-3
    )
  }
})

test_that("gs_boundaries matches rpact", {
  skip_if_not_installed("rpact")

  rpact_cases <- list(
    list(alpha = 0.025, t = c(1 / 3, 2 / 3, 1), type = "asOF", fn = spending_of),
    list(alpha = 0.025, t = c(0.5, 1), type = "asOF", fn = spending_of),
    list(alpha = 0.025, t = c(1 / 3, 2 / 3, 1), type = "asP", fn = spending_pocock)
  )

  for (tc in rpact_cases) {
    rpact_d <- suppressMessages(rpact::getDesignGroupSequential(
      typeOfDesign = tc$type,
      informationRates = tc$t,
      alpha = tc$alpha
    ))

    graphicalMCP_b <- graphicalMCP:::gs_boundaries(tc$alpha, tc$t, tc$fn)

    expect_equal(graphicalMCP_b$bounds_z, rpact_d$criticalValues, tolerance = 1e-3)
    expect_equal(graphicalMCP_b$bounds_nominal, rpact_d$stageLevels, tolerance = 1e-3)
  }
})
