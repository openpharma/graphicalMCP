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
    H2 = c(0.017,  0.0035),
    H3 = c(0.009,  0.002),
    H4 = c(0.13,   0.06)
  )
}

test_that("graph_test_shortcut_gsd returns correct class and structure", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_of
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
  expect_length(result$outputs$rejected_at, 4)
  expect_s3_class(result$outputs$graph, "initial_graph")

  # test_values should be NULL by default
  expect_null(result$test_values)
})

test_that("test_values output has correct structure", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_of,
    test_values = TRUE
  )

  expect_type(result$test_values, "list")
  expect_length(result$test_values, 2)

  for (k in 1:2) {
    tv <- result$test_values[[k]]
    if (!is.null(tv)) {
      expect_s3_class(tv, "data.frame")
      expect_true(all(c("Analysis", "Hypothesis", "Weight", "p",
                        "Boundary", "Reject") %in% names(tv)))
    }
  }
})

test_that("Maurer-Bretz case study: look_back = FALSE", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_of,
    look_back = FALSE
  )

  # H1, H2, H3 rejected; H4 not
  expect_equal(
    result$outputs$rejected,
    c(H1 = TRUE, H2 = TRUE, H3 = TRUE, H4 = FALSE)
  )

  # All rejections at analysis 2
  expect_equal(result$outputs$rejected_at[["H1"]], 2L)
  expect_equal(result$outputs$rejected_at[["H2"]], 2L)
  expect_equal(result$outputs$rejected_at[["H3"]], 2L)
  expect_true(is.na(result$outputs$rejected_at[["H4"]]))
})

test_that("Maurer-Bretz case study: look_back = TRUE", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_of,
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
    H1 = c(0.02,   0.0002),
    H2 = c(0.02,   0.003),
    H3 = c(0.0008, 0.006),
    H4 = c(0.3,    0.2)
  )

  result_no_lb <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_pocock,
    look_back = FALSE
  )

  result_lb <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_pocock,
    look_back = TRUE
  )

  # look_back = FALSE: H1, H2 rejected; H3 not
  expect_false(result_no_lb$outputs$rejected[["H3"]])

  # look_back = TRUE: H3 also rejected (via look-back to analysis 1)
  expect_true(result_lb$outputs$rejected[["H3"]])
})

test_that("look_back = TRUE: rejected_at looks back to earlier analysis", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))

  p <- rbind(
    H1 = c(0.010, 0.020),
    H2 = c(0.010, 0.005)
  )

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(0.5, 1), spending_fn = spending_linear,
    look_back = TRUE
  )

  # H2 rejected at analysis 2, H1 rejected but attributed to analysis 1
  expect_true(result$outputs$rejected[["H1"]])
  expect_true(result$outputs$rejected[["H2"]])
  expect_equal(result$outputs$rejected_at[["H1"]], 1L)
  expect_equal(result$outputs$rejected_at[["H2"]], 2L)
})

test_that("repeated_p and sequential_p matrices are correct dimensions", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_of
  )

  expect_equal(dim(result$outputs$repeated_p), c(4, 2))
  expect_equal(dim(result$outputs$sequential_p), c(4, 2))
  expect_equal(rownames(result$outputs$repeated_p), c("H1", "H2", "H3", "H4"))
})

test_that("sequential_p <= repeated_p elementwise", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_of
  )

  expect_true(all(result$outputs$sequential_p <= result$outputs$repeated_p + 1e-6))
})

test_that("sequential_p is cummin of repeated_p per hypothesis", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_of
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
    g, p, alpha = 0.025,
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
    g, p, alpha = 0.025,
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
    g, p, alpha = 0.025,
    info_frac = c(0.5, 1), spending_fn = spending_of
  )

  expect_equal(
    result$outputs$rejected,
    c(H1 = FALSE, H2 = FALSE)
  )
  expect_true(all(is.na(result$outputs$rejected_at)))
})

test_that("all rejected when p-values are very small", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.0001, 0.0001), H2 = c(0.0001, 0.0001))

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
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
    g, p, alpha = 0.025,
    info_frac = c(0.5, 1), spending_fn = spending_of,
    look_back = FALSE
  )
  r_lb <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
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
    g, p, 0.025, c(0.5, 1), spending_of, look_back = "yes"
  ))

  # Invalid test_values
  expect_error(graph_test_shortcut_gsd(
    g, p, 0.025, c(0.5, 1), spending_of, test_values = 1
  ))
})

test_that("print method works without error", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_of,
    test_values = TRUE
  )

  expect_output(print(result))
})

test_that("print method works for look_back = TRUE", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_of,
    look_back = TRUE, test_values = TRUE
  )

  expect_output(print(result))
})

test_that("print method works without test_values", {
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3), spending_fn = spending_of
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
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3, 1), spending_fn = spending_of
  )

  expect_equal(dim(result$outputs$repeated_p), c(2, 3))
  expect_equal(dim(result$outputs$sequential_p), c(2, 3))
  expect_s3_class(result, "gsd_graph_report")
})

test_that("info_frac not ending at 1 works (interim-only)", {
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  result <- graph_test_shortcut_gsd(
    g, p, alpha = 0.025,
    info_frac = c(1/3, 2/3),
    spending_fn = spending_of
  )

  expect_s3_class(result, "gsd_graph_report")
})
