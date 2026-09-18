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

  # repeated_p() integrates the joint distribution with randomized
  # quasi-Monte Carlo (mvtnorm::GenzBretz), so fix the seed before each paired
  # evaluation and allow a small tolerance for the integration error. A
  # dispatch to the wrong spending function would differ by far more.
  set.seed(1)
  engine_of <- suppressMessages(engine$repeated_p(p, info_frac, 1))
  set.seed(1)
  direct_of <- suppressMessages(repeated_p(p, info_frac, spending_of))
  expect_equal(engine_of, direct_of, tolerance = 1e-3)

  set.seed(1)
  engine_pocock <- suppressMessages(engine$repeated_p(p, info_frac, 2))
  set.seed(1)
  direct_pocock <- suppressMessages(repeated_p(p, info_frac, spending_pocock))
  expect_equal(engine_pocock, direct_pocock, tolerance = 1e-3)

  # The two hypotheses use different spending functions, so their repeated
  # p-values must differ: this guards against dispatching to the wrong one.
  expect_false(isTRUE(all.equal(engine_of, engine_pocock, tolerance = 1e-3)))
})

test_that("gsd_engine_mvtnorm delegates to gs_boundaries() per hypothesis", {
  engine <- gsd_engine_mvtnorm(list(spending_of, spending_pocock))
  info_frac <- c(1 / 3, 2 / 3, 1)

  # See the note on randomized integration in the test above.
  set.seed(1)
  engine_of <- engine$nominal_bounds(0.025, info_frac, 1)
  set.seed(1)
  direct_of <- gs_boundaries(0.025, info_frac, spending_of)$bounds_nominal
  expect_equal(engine_of, direct_of, tolerance = 1e-3)

  set.seed(1)
  engine_pocock <- engine$nominal_bounds(0.025, info_frac, 2)
  set.seed(1)
  direct_pocock <-
    gs_boundaries(0.025, info_frac, spending_pocock)$bounds_nominal
  expect_equal(engine_pocock, direct_pocock, tolerance = 1e-3)

  # Different spending functions must give different boundaries.
  expect_false(isTRUE(all.equal(engine_of, engine_pocock, tolerance = 1e-3)))
})
