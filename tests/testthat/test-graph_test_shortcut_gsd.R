# Common setup for GSD tests
gsd_graph <- function() {
  graph_create(
    c(0.5, 0.5, 0, 0),
    rbind(
      c(0, 0.5, 0.5, 0),
      c(0.5, 0, 0, 0.5),
      c(0, 1, 0, 0),
      c(1, 0, 0, 0)
    )
  )
}

gsd_p <- function() {
  rbind(
    H1 = c(0.0062, 0.0002),
    H2 = c(0.017, 0.0035),
    H3 = c(0.009, 0.002),
    H4 = c(0.13, 0.06)
  )
}

test_that("graph_test_shortcut_gsd returns correct class and structure", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of
  )

  expect_s3_class(result, "gsd_graph_report")

  # Check inputs
  expect_equal(result$inputs$graph, g)
  expect_equal(result$inputs$p, p, ignore_attr = TRUE)
  expect_equal(result$inputs$alpha, 0.025)

  # Check outputs structure
  expect_true(is.matrix(result$outputs$repeated_p))
  expect_true(is.matrix(result$outputs$sequential_p))
  expect_length(result$outputs$adjusted_p, 4)
  expect_length(result$outputs$rejected, 4)
  expect_length(result$outputs$decision_at, 4)
  expect_s3_class(result$outputs$graph, "initial_graph")

  # test_values should be NULL by default
  expect_null(result$test_values)
})

test_that("test_values output has correct structure", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    test_values = TRUE
  )

  expect_type(result$test_values, "list")
  expect_length(result$test_values, 2)

  for (k in 1:2) {
    tv <- result$test_values[[k]]
    if (!is.null(tv)) {
      expect_s3_class(tv, "data.frame")
      expect_true(all(c(
        "Analysis", "Hypothesis", "Weight", "p",
        "Boundary", "Reject"
      ) %in% names(tv)))
    }
  }
})

test_that("Maurer-Bretz case study: look_back = FALSE", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = FALSE
  )

  # H1, H2, H3 rejected; H4 not
  expect_equal(
    result$outputs$rejected,
    c(H1 = TRUE, H2 = TRUE, H3 = TRUE, H4 = FALSE)
  )

  # All rejections at analysis 2
  expect_equal(result$outputs$decision_at[["H1"]], 2L)
  expect_equal(result$outputs$decision_at[["H2"]], 2L)
  expect_equal(result$outputs$decision_at[["H3"]], 2L)
  expect_equal(result$outputs$decision_at[["H4"]], 2L)
})

test_that("Maurer-Bretz case study: look_back = TRUE", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = TRUE
  )

  # Same rejections as look_back = FALSE for this example
  expect_equal(
    result$outputs$rejected,
    c(H1 = TRUE, H2 = TRUE, H3 = TRUE, H4 = FALSE)
  )
})

test_that("look_back = TRUE vs FALSE can differ", {
  g <- gsd_graph()

  p <- rbind(
    H1 = c(0.02, 0.0002),
    H2 = c(0.02, 0.003),
    H3 = c(0.0008, 0.006),
    H4 = c(0.3, 0.2)
  )

  result_no_lb <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_pocock,
    look_back = FALSE
  )

  result_lb <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_pocock,
    look_back = TRUE
  )

  # look_back = FALSE: H1, H2 rejected; H3 not
  expect_false(result_no_lb$outputs$rejected[["H3"]])

  # look_back = TRUE: H3 also rejected (via look-back to analysis 1)
  expect_true(result_lb$outputs$rejected[["H3"]])
})

test_that("look_back = TRUE: first_rejected_at looks back to earlier analysis", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))

  p <- rbind(
    H1 = c(0.010, 0.020),
    H2 = c(0.010, 0.005)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(0.5, 1), spending_fn = spending_linear,
    look_back = TRUE
  )

  # Both operationally rejected at analysis 2
  expect_true(result$outputs$rejected[["H1"]])
  expect_true(result$outputs$rejected[["H2"]])
  expect_equal(result$outputs$decision_at[["H1"]], 2L)
  expect_equal(result$outputs$decision_at[["H2"]], 2L)

  # H1 first rejected at analysis 1 (via look_back), H2 at analysis 2
  expect_equal(result$outputs$first_rejected_at[["H1"]], 1L)
  expect_equal(result$outputs$first_rejected_at[["H2"]], 2L)
})

test_that("repeated_p and sequential_p matrices are correct dimensions", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of
  )

  expect_equal(dim(result$outputs$repeated_p), c(4, 2))
  expect_equal(dim(result$outputs$sequential_p), c(4, 2))
  expect_equal(rownames(result$outputs$repeated_p), c("H1", "H2", "H3", "H4"))
})

test_that("sequential_p <= repeated_p elementwise", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of
  )

  expect_true(all(result$outputs$sequential_p <= result$outputs$repeated_p + 1e-6))
})

test_that("sequential_p is cummin of repeated_p per hypothesis", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of
  )

  for (j in 1:4) {
    expect_equal(
      result$outputs$sequential_p[j, ],
      cummin(result$outputs$repeated_p[j, ]),
      tolerance = 1e-6
    )
  }
})

test_that("different spending functions per hypothesis", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(0.5, 1),
    spending_fn = list(spending_of, spending_pocock)
  )

  expect_s3_class(result, "gsd_graph_report")
  expect_length(result$outputs$rejected, 2)
})

test_that("different info fractions per hypothesis", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = rbind(c(0.5, 1), c(0.6, 1)),
    spending_fn = spending_of
  )

  expect_s3_class(result, "gsd_graph_report")
  expect_equal(dim(result$inputs$info_frac), c(2, 2))
})

test_that("no rejections when p-values are large", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.5, 0.5), H2 = c(0.5, 0.5))

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(0.5, 1), spending_fn = spending_of
  )

  expect_equal(
    result$outputs$rejected,
    c(H1 = FALSE, H2 = FALSE)
  )
  expect_equal(result$outputs$decision_at, c(H1 = 2L, H2 = 2L))
})

test_that("all rejected when p-values are very small", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.0001, 0.0001), H2 = c(0.0001, 0.0001))

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(0.5, 1), spending_fn = spending_pocock
  )

  expect_true(all(result$outputs$rejected))
})

test_that("look_back = FALSE and TRUE agree when repeated_p are monotone", {
  # When repeated p-values decrease across analyses, sequential_p = repeated_p
  # and both modes should give identical results
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.1, 0.001), H2 = c(0.1, 0.001))

  r_no <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(0.5, 1), spending_fn = spending_of,
    look_back = FALSE
  )
  r_lb <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(0.5, 1), spending_fn = spending_of,
    look_back = TRUE
  )

  expect_equal(r_no$outputs$rejected, r_lb$outputs$rejected)
})

# Input validation tests
test_that("invalid inputs throw errors", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  # Invalid graph
  expect_error(graph_test_shortcut_gsd(
    unclass(g), p, 0.025, c(0.5, 1), spending_of
  ))

  # Invalid p - wrong dimensions
  expect_error(graph_test_shortcut_gsd(
    g, rbind(c(0.01, 0.02)), 0.025, c(0.5, 1), spending_of
  ))

  # Invalid alpha
  expect_error(graph_test_shortcut_gsd(
    g, p, 1.5, c(0.5, 1), spending_of
  ))
  expect_error(graph_test_shortcut_gsd(
    g, p, "0.025", c(0.5, 1), spending_of
  ))

  # Invalid info_frac - wrong number of rows
  expect_error(graph_test_shortcut_gsd(
    g, p, 0.025, rbind(c(0.5, 1), c(0.5, 1), c(0.5, 1)), spending_of
  ))

  # Invalid info_frac - not monotonic
  expect_error(graph_test_shortcut_gsd(
    g, p, 0.025, c(1, 0.5), spending_of
  ))

  # Invalid spending_fn - not a function
  expect_error(graph_test_shortcut_gsd(
    g, p, 0.025, c(0.5, 1), "spending_of"
  ))

  # Invalid look_back
  expect_error(graph_test_shortcut_gsd(
    g, p, 0.025, c(0.5, 1), spending_of,
    look_back = "yes"
  ))

  # Invalid test_values
  expect_error(graph_test_shortcut_gsd(
    g, p, 0.025, c(0.5, 1), spending_of,
    test_values = 1
  ))
})

test_that("print method works without error", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    test_values = TRUE
  )

  expect_output(print(result))
})

test_that("print method works for look_back = TRUE", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = TRUE, test_values = TRUE
  )

  expect_output(print(result))
})

test_that("print method works without test_values", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of
  )

  expect_output(print(result))
})

test_that("three analyses work correctly", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(0.1, 0.05, 0.001),
    H2 = c(0.1, 0.05, 0.001)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3, 1), spending_fn = spending_of
  )

  expect_equal(dim(result$outputs$repeated_p), c(2, 3))
  expect_equal(dim(result$outputs$sequential_p), c(2, 3))
  expect_s3_class(result, "gsd_graph_report")
})

test_that("info_frac not ending at 1 works (interim-only)", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3),
    spending_fn = spending_of
  )

  expect_s3_class(result, "gsd_graph_report")
})

# NA padding tests (different K per hypothesis)
test_that("NA padding: H1 has 2 analyses, H2 has 3", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(0.024, 0.01, NA),
    H2 = c(0.015, 0.005, 0.001)
  )
  info_frac <- rbind(
    c(0.5, 1, NA),
    c(1 / 3, 2 / 3, 1)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = info_frac,
    spending_fn = spending_of
  )

  expect_s3_class(result, "gsd_graph_report")
  expect_equal(dim(result$outputs$repeated_p), c(2, 3))
  expect_equal(dim(result$outputs$sequential_p), c(2, 3))

  # H1's third column should be NA in repeated_p and sequential_p
  expect_true(is.na(result$outputs$repeated_p["H1", 3]))
  expect_true(is.na(result$outputs$sequential_p["H1", 3]))

  # H2's columns should all be non-NA
  expect_false(any(is.na(result$outputs$repeated_p["H2", ])))
  expect_false(any(is.na(result$outputs$sequential_p["H2", ])))
})

test_that("NA padding: vector info_frac with NA in p errors", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(0.024, 0.01, NA),
    H2 = c(0.015, 0.005, 0.001)
  )

  expect_error(
    graph_test_shortcut_gsd(
      g, p,
      alpha = 0.025,
      info_frac = c(1 / 3, 2 / 3, 1),
      spending_fn = spending_of
    ),
    "info_frac must be a matrix"
  )
})

test_that("NA padding: info_frac as matrix with matching NAs", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(0.024, 0.01, NA),
    H2 = c(0.015, 0.005, 0.001)
  )
  info_frac <- rbind(
    c(0.5, 1, NA),
    c(1 / 3, 2 / 3, 1)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = info_frac,
    spending_fn = spending_of
  )

  expect_s3_class(result, "gsd_graph_report")
})

test_that("NA padding: mismatched NAs in p and info_frac matrix errors", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(0.024, 0.01, NA),
    H2 = c(0.015, 0.005, 0.001)
  )
  info_frac <- rbind(
    c(0.5, 1, 0.9), # not NA where p is NA
    c(1 / 3, 2 / 3, 1)
  )

  expect_error(graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = info_frac,
    spending_fn = spending_of
  ))
})

test_that("NA padding: non-contiguous NAs work (tested at analyses 1 and 3)", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(0.024, NA, 0.01),
    H2 = c(0.015, 0.005, 0.001)
  )
  info_frac <- rbind(
    c(0.5, NA, 1),
    c(1 / 3, 2 / 3, 1)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = info_frac,
    spending_fn = spending_of
  )

  expect_s3_class(result, "gsd_graph_report")
  expect_true(is.na(result$outputs$repeated_p["H1", 2]))
  expect_false(is.na(result$outputs$repeated_p["H1", 1]))
  expect_false(is.na(result$outputs$repeated_p["H1", 3]))
})

test_that("NA padding: leading NA works (H tested at later analyses)", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(NA, 0.01, 0.005),
    H2 = c(0.015, 0.005, 0.001)
  )
  info_frac <- rbind(
    c(NA, 0.5, 1),
    c(1 / 3, 2 / 3, 1)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = info_frac,
    spending_fn = spending_of
  )

  expect_s3_class(result, "gsd_graph_report")
  expect_true(is.na(result$outputs$repeated_p["H1", 1]))
  expect_false(is.na(result$outputs$repeated_p["H1", 2]))
})

test_that("NA padding: all-NA row errors", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(NA, NA),
    H2 = c(0.015, 0.005)
  )
  info_frac <- rbind(
    c(NA, NA),
    c(0.5, 1)
  )

  expect_error(graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = info_frac,
    spending_fn = spending_of
  ))
})

test_that("NA padding: look_back = TRUE works with different K", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(0.024, 0.01, NA),
    H2 = c(0.015, 0.005, 0.001)
  )
  info_frac <- rbind(
    c(0.5, 1, NA),
    c(1 / 3, 2 / 3, 1)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = info_frac,
    spending_fn = spending_of,
    look_back = TRUE
  )

  expect_s3_class(result, "gsd_graph_report")
  expect_true(is.na(result$outputs$repeated_p["H1", 3]))
  expect_true(is.na(result$outputs$sequential_p["H1", 3]))
})

test_that("NA padding: test_values works with different K", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(0.024, 0.01, NA),
    H2 = c(0.015, 0.005, 0.001)
  )
  info_frac <- rbind(
    c(0.5, 1, NA),
    c(1 / 3, 2 / 3, 1)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = info_frac,
    spending_fn = spending_of,
    test_values = TRUE
  )

  expect_type(result$test_values, "list")
  expect_length(result$test_values, 3)
})

test_that("NA padding: no NA gives same results as before", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(0.5, 1),
    spending_fn = spending_of
  )

  # No NAs should appear in output
  expect_false(anyNA(result$outputs$repeated_p))
  expect_false(anyNA(result$outputs$sequential_p))
})

# =============================================================================
# New feature tests: decision_at, first_rejected_at, rejection_sequence,
# per-hypothesis look_back, verbose boundary_table, test_values look_back rows
# =============================================================================

test_that("decision_at reports last tested analysis for non-rejected hypotheses", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of
  )

  # H4 is not rejected but tested at analysis 2
  expect_false(result$outputs$rejected[["H4"]])
  expect_equal(result$outputs$decision_at[["H4"]], 2L)

  # All decision_at should be non-NA
  expect_false(anyNA(result$outputs$decision_at))
})

test_that("first_rejected_at is NA for non-rejected hypotheses", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of
  )

  expect_true(is.na(result$outputs$first_rejected_at[["H4"]]))

  # For rejected hypotheses, first_rejected_at should be non-NA
  rejected_hyps <- names(which(result$outputs$rejected))
  expect_false(anyNA(result$outputs$first_rejected_at[rejected_hyps]))
})

test_that("first_rejected_at equals decision_at when look_back = FALSE", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = FALSE
  )

  rejected_hyps <- names(which(result$outputs$rejected))
  expect_equal(
    result$outputs$first_rejected_at[rejected_hyps],
    result$outputs$decision_at[rejected_hyps]
  )
})

test_that("first_rejected_at can differ from decision_at with look_back = TRUE", {
  g <- gsd_graph()
  p <- gsd_p()
  p["H3", ] <- c(0.0008, 0.006) # strong at IA1, weak at IA2

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_pocock,
    look_back = TRUE
  )

  # H3 should be rejected with first_rejected_at < decision_at
  expect_true(result$outputs$rejected[["H3"]])
  expect_equal(result$outputs$decision_at[["H3"]], 2L)
  expect_equal(result$outputs$first_rejected_at[["H3"]], 1L)
})

test_that("rejection_sequence is in outputs and has correct length", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of
  )

  expect_true("rejection_sequence" %in% names(result$outputs))
  expect_equal(
    length(result$outputs$rejection_sequence),
    sum(result$outputs$rejected)
  )
  # All rejected hypotheses should appear in the sequence
  expect_true(all(
    names(which(result$outputs$rejected)) %in% result$outputs$rejection_sequence
  ))
})

test_that("rejection_sequence is empty when nothing is rejected", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.5, 0.5), H2 = c(0.5, 0.5))

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(0.5, 1), spending_fn = spending_of
  )

  expect_equal(length(result$outputs$rejection_sequence), 0)
})

test_that("per-hypothesis look_back as vector works", {
  g <- gsd_graph()
  p <- gsd_p()
  p["H3", ] <- c(0.0008, 0.006)

  # Only H3 and H4 use look_back
  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_pocock,
    look_back = c(FALSE, FALSE, TRUE, TRUE)
  )

  expect_s3_class(result, "gsd_graph_report")
  # look_back should be stored as a named vector
  expect_equal(length(result$inputs$look_back), 4)
  expect_equal(result$inputs$look_back[["H3"]], TRUE)
  expect_equal(result$inputs$look_back[["H1"]], FALSE)
})

test_that("per-hypothesis look_back: only look_back hypotheses can have first_rejected_at < decision_at", {
  g <- gsd_graph()
  p <- gsd_p()
  p["H3", ] <- c(0.0008, 0.006)

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_pocock,
    look_back = c(FALSE, FALSE, TRUE, TRUE)
  )

  # H3 has look_back = TRUE and should have first_rejected_at = 1
  if (result$outputs$rejected[["H3"]]) {
    expect_true(result$outputs$first_rejected_at[["H3"]] <= result$outputs$decision_at[["H3"]])
  }

  # H1 and H2 have look_back = FALSE, so first_rejected_at == decision_at
  for (h in c("H1", "H2")) {
    if (result$outputs$rejected[[h]]) {
      expect_equal(result$outputs$first_rejected_at[[h]], result$outputs$decision_at[[h]])
    }
  }
})

test_that("scalar look_back = FALSE is equivalent to vector of all FALSE", {
  g <- gsd_graph()
  p <- gsd_p()

  r_scalar <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = FALSE
  )
  r_vector <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = c(FALSE, FALSE, FALSE, FALSE)
  )

  expect_equal(r_scalar$outputs$rejected, r_vector$outputs$rejected)
  expect_equal(r_scalar$outputs$adjusted_p, r_vector$outputs$adjusted_p)
  expect_equal(r_scalar$outputs$decision_at, r_vector$outputs$decision_at)
})

test_that("scalar look_back = TRUE is equivalent to vector of all TRUE", {
  g <- gsd_graph()
  p <- gsd_p()

  r_scalar <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = TRUE
  )
  r_vector <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = c(TRUE, TRUE, TRUE, TRUE)
  )

  expect_equal(r_scalar$outputs$rejected, r_vector$outputs$rejected)
  expect_equal(r_scalar$outputs$adjusted_p, r_vector$outputs$adjusted_p)
  expect_equal(r_scalar$outputs$decision_at, r_vector$outputs$decision_at)
  expect_equal(r_scalar$outputs$first_rejected_at, r_vector$outputs$first_rejected_at)
})

test_that("verbose = TRUE produces boundary_table", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    verbose = TRUE
  )

  expect_true("boundary_table" %in% names(result))
  expect_true(is.list(result$boundary_table))
  expect_equal(length(result$boundary_table), 4)
  expect_equal(names(result$boundary_table), c("H1", "H2", "H3", "H4"))

  # Each entry should be a data frame with Weight, Alpha.Allocated, Boundary.k
  for (h in names(result$boundary_table)) {
    bt <- result$boundary_table[[h]]
    expect_true(is.data.frame(bt))
    expect_true("Weight" %in% names(bt))
    expect_true("Alpha.Allocated" %in% names(bt))
    expect_true("Boundary.1" %in% names(bt))
    expect_true("Boundary.2" %in% names(bt))
  }
})

test_that("verbose = FALSE does not produce boundary_table", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    verbose = FALSE
  )

  expect_null(result$boundary_table)
})

test_that("boundary_table has correct values", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(0.5, 1), spending_fn = spending_of,
    verbose = TRUE
  )

  bt_h1 <- result$boundary_table$H1

  # Weight 0 should have boundary 0
  expect_equal(bt_h1$Boundary.1[bt_h1$Weight == 0], 0)
  expect_equal(bt_h1$Boundary.2[bt_h1$Weight == 0], 0)

  # Alpha.Allocated should equal Weight * alpha
  expect_equal(bt_h1$Alpha.Allocated, bt_h1$Weight * 0.025)

  # Boundaries should be non-negative
  expect_true(all(bt_h1$Boundary.1 >= 0))
  expect_true(all(bt_h1$Boundary.2 >= 0))

  # Boundaries should increase with weight
  expect_true(all(diff(bt_h1$Boundary.1) >= 0))
  expect_true(all(diff(bt_h1$Boundary.2) >= 0))
})

test_that("boundary_table matches test_values boundaries", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    verbose = TRUE, test_values = TRUE
  )

  # For each rejection in test_values, the boundary should match the
  # boundary_table entry for that hypothesis at that weight and analysis
  for (k in seq_along(result$test_values)) {
    tv <- result$test_values[[k]]
    if (is.null(tv)) next
    tv <- tv[!tv$Look_back, ] # skip look_back rows

    for (i in seq_len(nrow(tv))) {
      hyp <- tv$Hypothesis[i]
      w <- tv$Weight[i]
      boundary <- tv$Boundary[i]

      bt <- result$boundary_table[[hyp]]
      bt_row <- bt[abs(bt$Weight - w) < 1e-9, ]

      if (nrow(bt_row) > 0) {
        col_name <- paste0("Boundary.", k)
        expect_equal(boundary, bt_row[[col_name]],
          tolerance = 1e-6,
          label = paste(hyp, "weight", w, "analysis", k)
        )
      }
    }
  }
})

test_that("test_values include Look_back rows when look_back = TRUE", {
  g <- gsd_graph()
  p <- gsd_p()
  p["H3", ] <- c(0.0008, 0.006)

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_pocock,
    look_back = TRUE, test_values = TRUE
  )

  # Analysis 2 should have Look_back rows for H3
  tv2 <- result$test_values[[2]]
  expect_true("Look_back" %in% names(tv2))
  expect_true(any(tv2$Look_back))

  # The look_back row should be for H3 at analysis 1
  lb_rows <- tv2[tv2$Look_back, ]
  expect_true("H3" %in% lb_rows$Hypothesis)
  expect_equal(lb_rows$Analysis[lb_rows$Hypothesis == "H3"], 1L)

  # The look_back row should show Reject = TRUE
  expect_true(lb_rows$Reject[lb_rows$Hypothesis == "H3"])

  # The non-look_back row for H3 at analysis 2 should show Reject = FALSE
  h3_rows <- tv2[tv2$Hypothesis == "H3" & !tv2$Look_back, ]
  expect_false(h3_rows$Reject)
})

test_that("test_values have no Look_back rows when look_back = FALSE", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = FALSE, test_values = TRUE
  )

  for (k in seq_along(result$test_values)) {
    tv <- result$test_values[[k]]
    if (is.null(tv)) next
    if ("Look_back" %in% names(tv)) {
      expect_false(any(tv$Look_back))
    }
  }
})

test_that("print method works with verbose = TRUE", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    verbose = TRUE
  )

  expect_output(print(result), "Boundary table")
})

test_that("print method works with per-hypothesis look_back", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_pocock,
    look_back = c(FALSE, FALSE, TRUE, TRUE)
  )

  expect_output(print(result), "Look back")
})

test_that("print method shows look_back footnote in test_values", {
  g <- gsd_graph()
  p <- gsd_p()
  p["H3", ] <- c(0.0008, 0.006)

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_pocock,
    look_back = TRUE, test_values = TRUE
  )

  expect_output(print(result), "Rejected via look_back")
})

test_that("print method shows rejection sequence", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025,
    info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of
  )

  expect_output(print(result), "Rejection sequence")
})

# =============================================================================
# Look_back tests: H4-type (data at both analyses, crosses only at earlier)
# and H5-type (data at analysis 1 only, look_back from later analysis)
# =============================================================================

# Helper: oncology-like graph for look_back tests
gsd_onc_graph <- function() {
  alpha_allocation <- c(0.01, 0.01, 0.004, 0, 0.0005, 0.0005)
  hypotheses <- alpha_allocation / sum(alpha_allocation)
  names(hypotheses) <- c("H1", "H2", "H3", "H4", "H5", "H6")
  transitions <- rbind(
    c(0, 1, 0, 0, 0, 0),
    c(0, 0, 0.5, 0.5, 0, 0),
    c(0, 0, 0, 1, 0, 0),
    c(0, 0, 0, 0, 0.5, 0.5),
    c(0, 0, 0, 0, 0, 1),
    c(0.5, 0.5, 0, 0, 0, 0)
  )
  graph_create(hypotheses, transitions)
}

gsd_onc_info_frac <- function() {
  rbind(
    H1 = c(185 / 295, 245 / 295, 1),
    H2 = c(529 / 800, 700 / 800, 1),
    H3 = c(265 / 310, 1, NA),
    H4 = c(675 / 750, 1, NA),
    H5 = c(1, NA, NA),
    H6 = c(1, NA, NA)
  )
}

test_that("H4-type look_back: data at both analyses, crosses only at earlier", {
  g <- gsd_onc_graph()
  info_frac <- gsd_onc_info_frac()

  # H4 has strong evidence at analysis 1, weak at analysis 2
  # H4 starts with weight 0, gets weight after H3 rejection at analysis 2
  p <- rbind(
    H1 = c(0.03, 0.0001, 0.000001),
    H2 = c(0.2, 0.15, 0.1),
    H3 = c(0.2, 0.001, NA),
    H4 = c(0.0001, 0.02, NA),
    H5 = c(0.00001, NA, NA),
    H6 = c(0.1, NA, NA)
  )

  r_no <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    spending_fn = spending_of, look_back = FALSE
  )
  r_yes <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    spending_fn = spending_of, look_back = TRUE, test_values = TRUE
  )

  # H4 not rejected without look_back
  expect_false(r_no$outputs$rejected[["H4"]])

  # H4 rejected with look_back, attributed to analysis 1
  expect_true(r_yes$outputs$rejected[["H4"]])
  expect_equal(r_yes$outputs$decision_at[["H4"]], 2L)
  expect_equal(r_yes$outputs$first_rejected_at[["H4"]], 1L)

  # test_values at analysis 2: H4 has standard row (Reject=FALSE) and
  # look_back row (Reject=TRUE)
  tv2 <- r_yes$test_values[[2]]
  h4_standard <- tv2[tv2$Hypothesis == "H4" & !tv2$Look_back, ]
  h4_lb <- tv2[tv2$Hypothesis == "H4" & tv2$Look_back, ]

  expect_equal(nrow(h4_standard), 1)
  expect_false(h4_standard$Reject)
  expect_equal(h4_standard$Analysis, 2L)

  expect_equal(nrow(h4_lb), 1)
  expect_true(h4_lb$Reject)
  expect_equal(h4_lb$Analysis, 1L)
})

test_that("H5-type look_back: single-analysis hypothesis, rejected via look_back at later analysis", {
  g <- gsd_onc_graph()
  info_frac <- gsd_onc_info_frac()

  # H5 has 1 analysis, p=0.0008 > initial boundary (0.0005)
  # After graph propagation at analysis 2, H5 gets more weight and
  # look_back finds p=0.0008 crosses the new boundary
  p <- rbind(
    H1 = c(0.03, 0.0001, 0.000001),
    H2 = c(0.2, 0.15, 0.1),
    H3 = c(0.2, 0.001, NA),
    H4 = c(0.0001, 0.02, NA),
    H5 = c(0.0008, NA, NA),
    H6 = c(0.1, NA, NA)
  )

  r_no <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    spending_fn = spending_of, look_back = FALSE
  )
  r_yes <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    spending_fn = spending_of, look_back = TRUE, test_values = TRUE
  )

  # H5 not rejected without look_back
  expect_false(r_no$outputs$rejected[["H5"]])

  # H5 rejected with look_back, attributed to analysis 1
  expect_true(r_yes$outputs$rejected[["H5"]])
  expect_equal(r_yes$outputs$decision_at[["H5"]], 2L)
  expect_equal(r_yes$outputs$first_rejected_at[["H5"]], 1L)

  # test_values at analysis 2: H5 has a standard row with p=NA (no data)
  # and a look_back row at analysis 1 (Reject=TRUE)
  tv2 <- r_yes$test_values[[2]]
  h5_standard <- tv2[tv2$Hypothesis == "H5" & !tv2$Look_back, ]
  h5_lb <- tv2[tv2$Hypothesis == "H5" & tv2$Look_back, ]

  # Standard row: p is NA, Reject is FALSE
  expect_equal(nrow(h5_standard), 1)
  expect_true(is.na(h5_standard$p))
  expect_false(h5_standard$Reject)

  # Look_back row: analysis 1, p=0.0008, Reject=TRUE
  expect_equal(nrow(h5_lb), 1)
  expect_true(h5_lb$Reject)
  expect_equal(h5_lb$Analysis, 1L)
  expect_equal(h5_lb$p, 0.0008)
})

test_that("H5-type: without look_back, single-analysis hypothesis not reconsidered", {
  g <- gsd_onc_graph()
  info_frac <- gsd_onc_info_frac()

  p <- rbind(
    H1 = c(0.03, 0.0001, 0.000001),
    H2 = c(0.2, 0.15, 0.1),
    H3 = c(0.2, 0.001, NA),
    H4 = c(0.0001, 0.02, NA),
    H5 = c(0.0008, NA, NA),
    H6 = c(0.1, NA, NA)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    spending_fn = spending_of, look_back = FALSE, test_values = TRUE
  )

  # H5 not rejected
  expect_false(result$outputs$rejected[["H5"]])
  # H5 decision_at should be 1 (only analysis with data)
  expect_equal(result$outputs$decision_at[["H5"]], 1L)

  # H5 should not appear in test_values at analysis 2
  tv2 <- result$test_values[[2]]
  expect_false("H5" %in% tv2$Hypothesis)
})

test_that("look_back with single-analysis hypothesis: not rejected when p too large", {
  g <- gsd_onc_graph()
  info_frac <- gsd_onc_info_frac()

  # H5 p=0.01 is too large even after propagation
  p <- rbind(
    H1 = c(0.03, 0.0001, 0.000001),
    H2 = c(0.2, 0.15, 0.1),
    H3 = c(0.2, 0.001, NA),
    H4 = c(0.0001, 0.02, NA),
    H5 = c(0.01, NA, NA),
    H6 = c(0.1, NA, NA)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    spending_fn = spending_of, look_back = TRUE
  )

  # H5 still not rejected — p=0.01 too large even with increased weight
  expect_false(result$outputs$rejected[["H5"]])
})

test_that("look_back carries forward sequential p for hypotheses with no current data", {
  g <- gsd_onc_graph()
  info_frac <- gsd_onc_info_frac()

  p <- rbind(
    H1 = c(0.03, 0.0001, 0.000001),
    H2 = c(0.2, 0.15, 0.1),
    H3 = c(0.2, 0.001, NA),
    H4 = c(0.0001, 0.02, NA),
    H5 = c(0.0008, NA, NA),
    H6 = c(0.1, NA, NA)
  )

  result <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    spending_fn = spending_of, look_back = TRUE
  )

  # Sequential p for H5 should be the same at analysis 1
  # (only one analysis with data)
  expect_equal(
    result$outputs$sequential_p["H5", 1],
    result$outputs$repeated_p["H5", 1]
  )
  # Analysis 2 and 3 should be NA for H5
  expect_true(is.na(result$outputs$sequential_p["H5", 2]))
  expect_true(is.na(result$outputs$sequential_p["H5", 3]))
})
