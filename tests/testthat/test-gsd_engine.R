test_that("gsd_engine_mvtnorm exposes the two-function engine interface", {
  engine <- gsd_engine_mvtnorm(list(spending_of, spending_pocock))

  expect_type(engine, "list")
  expect_named(engine, c("repeated_p", "nominal_bounds"))
  expect_true(is.function(engine$repeated_p))
  expect_true(is.function(engine$nominal_bounds))
})

test_that("gsd_engine_mvtnorm delegates to repeated_p() per hypothesis", {
  # Hypothesis 1 uses O'Brien-Fleming spending, hypothesis 2 uses Pocock
  engine <- gsd_engine_mvtnorm(list(spending_of, spending_pocock))
  p <- c(0.05, 0.02)
  info_frac <- c(0.5, 1)

  expect_equal(
    suppressMessages(engine$repeated_p(p, info_frac, 1)),
    suppressMessages(repeated_p(p, info_frac, spending_of))
  )
  expect_equal(
    suppressMessages(engine$repeated_p(p, info_frac, 2)),
    suppressMessages(repeated_p(p, info_frac, spending_pocock))
  )
})

test_that("gsd_engine_mvtnorm delegates to gs_boundaries() per hypothesis", {
  engine <- gsd_engine_mvtnorm(list(spending_of, spending_pocock))
  info_frac <- c(1 / 3, 2 / 3, 1)

  expect_equal(
    engine$nominal_bounds(0.025, info_frac, 1),
    gs_boundaries(0.025, info_frac, spending_of)$bounds_nominal
  )
  expect_equal(
    engine$nominal_bounds(0.025, info_frac, 2),
    gs_boundaries(0.025, info_frac, spending_pocock)$bounds_nominal
  )
})
