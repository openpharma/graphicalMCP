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

# gsDesign engine -------------------------------------------------------------
# The gsDesign engine integrates deterministically, so it is compared with the
# mvtnorm engine at the tolerance of the latter's randomized integration.

test_that("gsd_engine_gsDesign exposes the same two-function interface", {
  skip_if_not_installed("gsDesign")
  info_frac <- matrix(rep(c(1 / 3, 2 / 3), each = 2), 2, 2)
  engine <- gsd_engine_gsDesign(
    0.025, info_frac,
    rep(list(gsDesign::sfLDOF), 2), rep(list(NULL), 2), NULL
  )

  expect_named(engine, c("repeated_p", "nominal_bounds"))
  expect_true(is.function(engine$repeated_p))
  expect_true(is.function(engine$nominal_bounds))
})

test_that("gsd_engine_gsDesign matches the mvtnorm engine for spending functions", {
  skip_if_not_installed("gsDesign")
  info_frac <- matrix(rep(c(1 / 3, 2 / 3), each = 2), 2, 2)
  p <- rbind(c(0.0062, 0.0002), c(0.13, 0.06))
  gs <- gsd_engine_gsDesign(
    0.025, info_frac,
    list(gsDesign::sfLDOF, gsDesign::sfLDPocock), rep(list(NULL), 2), NULL
  )
  mv <- gsd_engine_mvtnorm(list(spending_of, spending_pocock))

  for (j in 1:2) {
    for (k in 1:2) {
      expect_lt(
        abs(
          suppressMessages(gs$repeated_p(p[j, 1:k], info_frac[j, 1:k], j)) -
            suppressMessages(mv$repeated_p(p[j, 1:k], info_frac[j, 1:k], j))
        ),
        1e-4
      )
    }
    for (a in c(0.025, 0.0125, 0.003125)) {
      expect_lt(
        max(abs(
          gs$nominal_bounds(a, info_frac[j, ], j) -
            mv$nominal_bounds(a, info_frac[j, ], j)
        )),
        1e-4
      )
    }
  }
})

test_that("gsd_engine_gsDesign honours usTime like spending_with_time()", {
  skip_if_not_installed("gsDesign")
  info_frac <- matrix(rep(c(1 / 3, 2 / 3), each = 2), 2, 2)
  usTime <- matrix(rep(c(0.5, 0.8), each = 2), 2, 2)
  p <- c(0.017, 0.0035)
  gs <- gsd_engine_gsDesign(
    0.025, info_frac,
    rep(list(gsDesign::sfLDOF), 2), rep(list(NULL), 2), usTime
  )
  mv <- gsd_engine_mvtnorm(
    rep(list(spending_with_time(spending_of, c(0.5, 0.8))), 2)
  )

  expect_lt(
    abs(
      suppressMessages(gs$repeated_p(p, info_frac[1, ], 1)) -
        suppressMessages(mv$repeated_p(p, info_frac[1, ], 1))
    ),
    1e-4
  )
  expect_lt(
    max(abs(
      gs$nominal_bounds(0.0125, info_frac[1, ], 1) -
        mv$nominal_bounds(0.0125, info_frac[1, ], 1)
    )),
    1e-4
  )
})

test_that("gsd_engine_gsDesign handles a single-analysis hypothesis in closed form", {
  skip_if_not_installed("gsDesign")
  # H1 has one analysis at 60% information; H2 has two
  info_frac <- rbind(c(0.6, NA), c(0.5, 1))
  gs <- gsd_engine_gsDesign(
    0.025, info_frac,
    rep(list(gsDesign::sfLDOF), 2), rep(list(NULL), 2), NULL
  )
  mv <- gsd_engine_mvtnorm(rep(list(spending_of), 2))

  expect_lt(
    abs(
      suppressMessages(gs$repeated_p(0.01, 0.6, 1)) -
        suppressMessages(mv$repeated_p(0.01, 0.6, 1))
    ),
    1e-4
  )
  expect_equal(
    gs$nominal_bounds(0.0125, 0.6, 1),
    gsDesign::sfLDOF(0.0125, 0.6, NULL)$spend
  )
  # A classical family with a single analysis is a fixed-sample test
  gs_wt <- gsd_engine_gsDesign(0.025, info_frac, list("WT", "WT"), list(0.25, 0.25), NULL)
  expect_equal(gs_wt$repeated_p(0.01, 0.6, 1), 0.01)
  expect_equal(gs_wt$nominal_bounds(0.0125, 0.6, 1), 0.0125)
})

test_that("gsd_engine_gsDesign reproduces exact Wang-Tsiatis repeated p-values", {
  skip_if_not_installed("gsDesign")
  # Reference values were cross-validated against rpact's exact Wang-Tsiatis
  # critical values (Delta = 0.25, three equally spaced analyses)
  info_frac <- matrix(c(1 / 3, 2 / 3, 1), 1, 3)
  gs <- gsd_engine_gsDesign(0.025, info_frac, list("WT"), list(0.25), NULL)
  p <- c(0.05, 0.02, 0.01)
  reference <- c(0.154240, 0.043770, 0.013016)

  for (k in 1:3) {
    expect_equal(
      gs$repeated_p(p[1:k], info_frac[1, 1:k], 1),
      reference[k],
      tolerance = 1e-4
    )
  }
  # The Wang-Tsiatis boundaries satisfy b_k = C * t_k^(Delta - 0.5)
  b_z <- stats::qnorm(gs$nominal_bounds(0.025, info_frac[1, ], 1), lower.tail = FALSE)
  shape <- b_z / info_frac[1, ]^(0.25 - 0.5)
  expect_lt(max(shape) - min(shape), 1e-6)
})

test_that("gsd_engine_gsDesign handles allocated levels of zero and one", {
  skip_if_not_installed("gsDesign")
  info_frac <- matrix(c(0.5, 1), 1, 2)
  gs <- gsd_engine_gsDesign(0.025, info_frac, list(gsDesign::sfLDOF), list(NULL), NULL)

  expect_equal(gs$nominal_bounds(0, info_frac[1, ], 1), c(0, 0))
  expect_equal(gs$nominal_bounds(1, info_frac[1, ], 1), c(1, 1))
})

test_that("gsd_engine_gsDesign clamps extreme p-values like repeated_p()", {
  skip_if_not_installed("gsDesign")
  info_frac <- matrix(c(1 / 3, 2 / 3), 1, 2)
  gs <- gsd_engine_gsDesign(0.025, info_frac, list(gsDesign::sfLDOF), list(NULL), NULL)
  mv <- gsd_engine_mvtnorm(list(spending_of))

  # Overwhelming evidence: both engines report their lower search bound
  expect_equal(
    gs$repeated_p(c(1e-9, 1e-9), info_frac[1, ], 1),
    suppressMessages(mv$repeated_p(c(1e-9, 1e-9), info_frac[1, ], 1))
  )
  # No evidence at all: both engines report 1
  expect_equal(
    gs$repeated_p(c(0.999999, 0.999999), info_frac[1, ], 1),
    suppressMessages(mv$repeated_p(c(0.999999, 0.999999), info_frac[1, ], 1))
  )
})

test_that("gsd_engine_gsDesign copes with spending that underflows at an early analysis", {
  skip_if_not_installed("gsDesign")
  # At 10% information the O'Brien-Fleming spend underflows to exactly 0 for
  # all but the largest levels; the engine must still return a valid level
  # rather than propagate gsDesign's "Final spend must be > 0" error
  info_frac <- matrix(c(0.1, 0.5, 1), 1, 3)
  gs <- gsd_engine_gsDesign(0.025, info_frac, list(gsDesign::sfLDOF), list(NULL), NULL)

  expect_no_error(rp <- gs$repeated_p(1e-20, 0.1, 1))
  expect_true(rp > 0 && rp < 0.05)
  # Weak evidence at 10% information is crossed only at a very lenient level:
  # the one at which the first-analysis spend equals the p-value
  expect_no_error(rp_weak <- gs$repeated_p(0.5, 0.1, 1))
  reference <- stats::uniroot(
    function(a) gsDesign::sfLDOF(a, 0.1, NULL)$spend - 0.5,
    c(0.01, 0.9999)
  )$root
  expect_equal(rp_weak, reference, tolerance = 1e-4)
})
