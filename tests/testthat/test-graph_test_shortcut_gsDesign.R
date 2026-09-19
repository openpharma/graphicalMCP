# Fixtures (test files are evaluated in isolation, so these mirror the ones in
# test-graph_test_shortcut_gsd.R)
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

# Maximum absolute difference, ignoring NA (NA positions are compared
# separately). The two engines integrate differently (deterministic grid vs
# randomized quasi-Monte Carlo), so numeric outputs agree only to ~1e-5.
expect_close <- function(new, old, tol = 1e-4) {
  expect_identical(is.na(new), is.na(old))
  expect_lt(max(abs(new - old), na.rm = TRUE), tol)
}

# The gsDesign-backed result must make the same decisions as the
# mvtnorm-backed one, with numerically close p-values and boundaries
expect_gsd_equivalent <- function(new, old, tol = 1e-4) {
  expect_identical(new$outputs$rejected, old$outputs$rejected)
  expect_identical(new$outputs$decision_at, old$outputs$decision_at)
  expect_identical(new$outputs$first_rejected_at, old$outputs$first_rejected_at)
  expect_identical(new$outputs$last_rejected_at, old$outputs$last_rejected_at)
  expect_identical(new$outputs$rejection_sequence, old$outputs$rejection_sequence)
  expect_equal(new$outputs$graph, old$outputs$graph)
  expect_close(new$outputs$repeated_p, old$outputs$repeated_p, tol)
  expect_close(new$outputs$sequential_p, old$outputs$sequential_p, tol)
  expect_close(new$outputs$adjusted_p, old$outputs$adjusted_p, tol)

  if (!is.null(old$boundary_table)) {
    expect_identical(names(new$boundary_table), names(old$boundary_table))
    for (h in names(old$boundary_table)) {
      expect_close(
        as.matrix(new$boundary_table[[h]]),
        as.matrix(old$boundary_table[[h]]),
        tol
      )
    }
  }

  if (!is.null(old$test_values)) {
    tv_new <- do.call(rbind, new$test_values)
    tv_old <- do.call(rbind, old$test_values)
    expect_identical(tv_new$Analysis, tv_old$Analysis)
    expect_identical(tv_new$Hypothesis, tv_old$Hypothesis)
    expect_identical(tv_new$Reject, tv_old$Reject)
    expect_identical(tv_new$Look_back, tv_old$Look_back)
    expect_close(tv_new$Weight, tv_old$Weight, tol)
    expect_close(tv_new$Boundary, tv_old$Boundary, tol)
  }
  invisible(TRUE)
}

# Structure -------------------------------------------------------------------

test_that("graph_test_shortcut_gsDesign returns the gsd_graph_report structure", {
  skip_if_not_installed("gsDesign")
  g <- gsd_graph()
  p <- gsd_p()

  result <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3)
  )

  expect_s3_class(result, "gsd_graph_report")
  expect_identical(result$inputs$engine, "gsDesign")
  expect_null(result$inputs$spending_fn)
  expect_length(result$inputs$sfu, 4)
  expect_named(result$inputs$sfu, c("H1", "H2", "H3", "H4"))
  expect_true(all(vapply(result$inputs$sfu, is.function, logical(1))))
  expect_length(result$inputs$sfupar, 4)
  expect_null(result$inputs$usTime)

  expect_true(is.matrix(result$outputs$repeated_p))
  expect_equal(dim(result$outputs$repeated_p), c(4, 2))
  expect_equal(dim(result$outputs$sequential_p), c(4, 2))
  expect_length(result$outputs$adjusted_p, 4)
  expect_length(result$outputs$rejected, 4)
  expect_s3_class(result$outputs$graph, "initial_graph")
  expect_null(result$test_values)
  expect_null(result$boundary_table)
})

test_that("results are deterministic (no Monte Carlo integration)", {
  skip_if_not_installed("gsDesign")
  g <- gsd_graph()
  p <- gsd_p()

  r1 <- graph_test_shortcut_gsDesign(g, p, 0.025, c(1 / 3, 2 / 3), verbose = TRUE)
  r2 <- graph_test_shortcut_gsDesign(g, p, 0.025, c(1 / 3, 2 / 3), verbose = TRUE)

  expect_identical(r1$outputs, r2$outputs)
  expect_identical(r1$boundary_table, r2$boundary_table)
})

# Equivalence with graph_test_shortcut_gsd() ----------------------------------

test_that("Maurer-Bretz case study matches graph_test_shortcut_gsd (look_back = FALSE)", {
  skip_if_not_installed("gsDesign")
  g <- gsd_graph()
  p <- gsd_p()

  old <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = FALSE, verbose = TRUE, test_values = TRUE
  )
  new <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), sfu = gsDesign::sfLDOF,
    look_back = FALSE, verbose = TRUE, test_values = TRUE
  )

  expect_gsd_equivalent(new, old)
  expect_equal(
    new$outputs$rejected,
    c(H1 = TRUE, H2 = TRUE, H3 = TRUE, H4 = FALSE)
  )
})

test_that("Maurer-Bretz case study matches graph_test_shortcut_gsd (look_back = TRUE)", {
  skip_if_not_installed("gsDesign")
  g <- gsd_graph()
  p <- gsd_p()

  old <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), spending_fn = spending_of,
    look_back = TRUE, verbose = TRUE, test_values = TRUE
  )
  new <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), sfu = gsDesign::sfLDOF,
    look_back = TRUE, verbose = TRUE, test_values = TRUE
  )

  expect_gsd_equivalent(new, old)
})

test_that("look_back = TRUE rejects via an earlier analysis, as in graph_test_shortcut_gsd", {
  skip_if_not_installed("gsDesign")
  g <- gsd_graph()
  p <- gsd_p()
  p["H3", ] <- c(0.0008, 0.006) # strong at IA1, weak at IA2

  old <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), spending_fn = spending_pocock,
    look_back = TRUE, test_values = TRUE
  )
  new <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), sfu = gsDesign::sfLDPocock,
    look_back = TRUE, test_values = TRUE
  )

  expect_gsd_equivalent(new, old)
  expect_true(new$outputs$rejected[["H3"]])
  expect_equal(new$outputs$first_rejected_at[["H3"]], 1L)
  expect_equal(new$outputs$decision_at[["H3"]], 2L)
})

test_that("per-hypothesis look_back matches graph_test_shortcut_gsd", {
  skip_if_not_installed("gsDesign")
  g <- gsd_graph()
  p <- gsd_p()
  p["H3", ] <- c(0.0008, 0.006)

  old <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), spending_fn = spending_pocock,
    look_back = c(FALSE, FALSE, TRUE, TRUE)
  )
  new <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), sfu = gsDesign::sfLDPocock,
    look_back = c(FALSE, FALSE, TRUE, TRUE)
  )

  expect_gsd_equivalent(new, old)
  expect_equal(new$inputs$look_back[["H3"]], TRUE)
})

test_that("built-in spending functions map onto their gsDesign counterparts", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))
  info_frac <- c(0.5, 1)

  # O'Brien-Fleming, Pocock, Hwang-Shih-DeCani, and linear spending
  pairs <- list(
    list(fn = spending_of, sfu = gsDesign::sfLDOF, sfupar = NULL),
    list(fn = spending_pocock, sfu = gsDesign::sfLDPocock, sfupar = NULL),
    list(
      fn = function(a, t) spending_hsd(a, t, gamma = -2),
      sfu = gsDesign::sfHSD, sfupar = -2
    ),
    list(fn = spending_linear, sfu = gsDesign::sfPower, sfupar = 1)
  )
  for (pr in pairs) {
    old <- graph_test_shortcut_gsd(
      g, p,
      alpha = 0.025, info_frac = info_frac, spending_fn = pr$fn,
      verbose = TRUE, test_values = TRUE
    )
    new <- graph_test_shortcut_gsDesign(
      g, p,
      alpha = 0.025, info_frac = info_frac, sfu = pr$sfu, sfupar = pr$sfupar,
      verbose = TRUE, test_values = TRUE
    )
    expect_gsd_equivalent(new, old)
  }
})

test_that("different spending functions per hypothesis match graph_test_shortcut_gsd", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  old <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = c(0.5, 1),
    spending_fn = list(spending_of, spending_pocock),
    verbose = TRUE
  )
  new <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = c(0.5, 1),
    sfu = list(gsDesign::sfLDOF, gsDesign::sfLDPocock),
    verbose = TRUE
  )

  expect_gsd_equivalent(new, old)
})

test_that("different information fractions per hypothesis match graph_test_shortcut_gsd", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))
  info_frac <- rbind(c(0.5, 1), c(0.6, 1))

  old <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac, spending_fn = spending_of
  )
  new <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = info_frac
  )

  expect_gsd_equivalent(new, old)
})

test_that("three analyses match graph_test_shortcut_gsd", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.05, 0.02, 0.01), H2 = c(0.1, 0.03, 0.004))
  info_frac <- c(1 / 3, 2 / 3, 1)

  old <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac, spending_fn = spending_of,
    verbose = TRUE, test_values = TRUE
  )
  new <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    verbose = TRUE, test_values = TRUE
  )

  expect_gsd_equivalent(new, old)
})

test_that("over-running information (info_frac > 1) matches graph_test_shortcut_gsd", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.05, 0.02, 0.01), H2 = c(0.1, 0.03, 0.004))
  # More information than planned at the final analysis; the spending is
  # capped at alpha there while the correlation uses the actual information
  info_frac <- c(1 / 3, 2 / 3, 1.1)

  old <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac, spending_fn = spending_of,
    verbose = TRUE
  )
  new <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    verbose = TRUE
  )

  expect_gsd_equivalent(new, old)
})

test_that("NA padding (different numbers of analyses) matches graph_test_shortcut_gsd", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(0.024, 0.01, NA),
    H2 = c(0.015, 0.005, 0.001)
  )
  info_frac <- rbind(
    c(0.5, 1, NA),
    c(1 / 3, 2 / 3, 1)
  )

  for (lb in c(FALSE, TRUE)) {
    old <- graph_test_shortcut_gsd(
      g, p,
      alpha = 0.025, info_frac = info_frac, spending_fn = spending_of,
      look_back = lb, verbose = TRUE, test_values = TRUE
    )
    new <- graph_test_shortcut_gsDesign(
      g, p,
      alpha = 0.025, info_frac = info_frac,
      look_back = lb, verbose = TRUE, test_values = TRUE
    )
    expect_gsd_equivalent(new, old)
    expect_true(is.na(new$outputs$repeated_p["H1", 3]))
  }
})

test_that("a hypothesis with a single analysis matches graph_test_shortcut_gsd", {
  skip_if_not_installed("gsDesign")
  # gsDesign::gsDesign() requires at least two analyses, so a single-analysis
  # hypothesis exercises the univariate special case of the engine
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(
    H1 = c(0.01, NA),
    H2 = c(0.015, 0.005)
  )
  info_frac <- rbind(
    c(0.6, NA),
    c(0.5, 1)
  )

  old <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac, spending_fn = spending_of,
    verbose = TRUE, test_values = TRUE
  )
  new <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    verbose = TRUE, test_values = TRUE
  )

  expect_gsd_equivalent(new, old)
})

test_that("usTime matches spending_with_time() in graph_test_shortcut_gsd", {
  skip_if_not_installed("gsDesign")
  g <- gsd_graph()
  p <- gsd_p()
  info_frac <- c(1 / 3, 2 / 3)
  spending_time <- c(0.5, 0.8)

  old <- graph_test_shortcut_gsd(
    g, p,
    alpha = 0.025, info_frac = info_frac,
    spending_fn = spending_with_time(spending_of, spending_time),
    verbose = TRUE, test_values = TRUE
  )
  new <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = info_frac, usTime = spending_time,
    verbose = TRUE, test_values = TRUE
  )

  expect_gsd_equivalent(new, old)
  expect_equal(dim(new$inputs$usTime), c(4, 2))
  expect_equal(unname(new$inputs$usTime[1, ]), spending_time)
})

# Classical boundary families -------------------------------------------------

test_that("classical boundary families run and give sensible results", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  for (fam in c("OF", "Pocock", "WT")) {
    result <- graph_test_shortcut_gsDesign(
      g, p,
      alpha = 0.025, info_frac = c(0.5, 1),
      sfu = fam, sfupar = if (fam == "WT") 0.25 else NULL,
      verbose = TRUE, test_values = TRUE
    )
    expect_s3_class(result, "gsd_graph_report")
    expect_false(anyNA(result$outputs$repeated_p))
    expect_true(all(result$outputs$repeated_p > 0 & result$outputs$repeated_p <= 1))
    # Sequential p-values are the running minimum of repeated p-values
    for (j in 1:2) {
      expect_equal(
        result$outputs$sequential_p[j, ],
        cummin(result$outputs$repeated_p[j, ])
      )
    }
  }
})

test_that("classical Pocock boundaries are constant across analyses", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  result <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = c(1 / 3, 1),
    sfu = "Pocock", verbose = TRUE
  )
  bt <- result$boundary_table$H1
  full <- bt[abs(bt$Weight - 1) < 1e-9, ]
  expect_equal(full$Boundary.1, full$Boundary.2, tolerance = 1e-6)
})

test_that("usTime is ignored with a warning for classical boundary families", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  expect_warning(
    with_ust <- graph_test_shortcut_gsDesign(
      g, p,
      alpha = 0.025, info_frac = c(0.5, 1),
      sfu = "WT", sfupar = 0.25, usTime = c(0.4, 1)
    ),
    "usTime does not apply"
  )
  without_ust <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = c(0.5, 1),
    sfu = "WT", sfupar = 0.25
  )
  expect_identical(with_ust$outputs$repeated_p, without_ust$outputs$repeated_p)
})

# Input handling --------------------------------------------------------------

test_that("sfupar given as a single non-list value applies to all hypotheses", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  scalar <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = c(0.5, 1), sfu = gsDesign::sfHSD, sfupar = -2
  )
  listed <- graph_test_shortcut_gsDesign(
    g, p,
    alpha = 0.025, info_frac = c(0.5, 1), sfu = gsDesign::sfHSD,
    sfupar = list(-2, -2)
  )
  expect_identical(scalar$outputs, listed$outputs)
  expect_equal(scalar$inputs$sfupar, list(H1 = -2, H2 = -2))
})

test_that("analysis names default to Analysis_k", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  result <- graph_test_shortcut_gsDesign(g, p, 0.025, c(0.5, 1))
  expect_equal(colnames(result$outputs$repeated_p), c("Analysis_1", "Analysis_2"))
  expect_equal(colnames(result$inputs$info_frac), c("Analysis_1", "Analysis_2"))
})

test_that("invalid gsDesign inputs throw errors", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  # Unknown boundary family
  expect_error(
    graph_test_shortcut_gsDesign(g, p, 0.025, c(0.5, 1), sfu = "foo"),
    "sfu must be"
  )
  # Wrong number of spending functions / parameters
  expect_error(
    graph_test_shortcut_gsDesign(
      g, p, 0.025, c(0.5, 1),
      sfu = list(gsDesign::sfLDOF, gsDesign::sfLDOF, gsDesign::sfLDOF)
    ),
    "Number of spending functions"
  )
  expect_error(
    graph_test_shortcut_gsDesign(
      g, p, 0.025, c(0.5, 1),
      sfu = gsDesign::sfHSD, sfupar = list(-2, -2, -2)
    ),
    "Number of spending function parameters"
  )
  # usTime shape and content
  expect_error(
    graph_test_shortcut_gsDesign(g, p, 0.025, c(0.5, 1), usTime = c(0.4, 1, 1)),
    "usTime"
  )
  expect_error(
    graph_test_shortcut_gsDesign(g, p, 0.025, c(0.5, 1), usTime = c(-0.1, 1)),
    "non-negative"
  )
  expect_error(
    graph_test_shortcut_gsDesign(g, p, 0.025, c(0.5, 1), usTime = c(1, 0.5)),
    "non-decreasing"
  )
  # usTime as a vector when p contains NA
  p_na <- rbind(H1 = c(0.024, 0.01, NA), H2 = c(0.015, 0.005, 0.001))
  info_na <- rbind(c(0.5, 1, NA), c(1 / 3, 2 / 3, 1))
  expect_error(
    graph_test_shortcut_gsDesign(
      g, p_na, 0.025, info_na,
      usTime = c(0.5, 1, 1)
    ),
    "usTime must be a matrix"
  )
  # usTime matrix with NA positions that do not match info_frac
  expect_error(
    graph_test_shortcut_gsDesign(
      g, p_na, 0.025, info_na,
      usTime = rbind(c(0.5, 1, 1), c(1 / 3, 2 / 3, 1))
    ),
    "NA positions"
  )
  # The shared validation still applies
  expect_error(graph_test_shortcut_gsDesign(unclass(g), p, 0.025, c(0.5, 1)))
  expect_error(graph_test_shortcut_gsDesign(g, p, 1.5, c(0.5, 1)))
})

test_that("the gsDesign guard is checked before anything else", {
  skip_if_not_installed("gsDesign")
  skip_if_not_installed("testthat", "3.2.0")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  expect_true(gsd_check_gsDesign())
  local_mocked_bindings(
    gsd_check_gsDesign = function() stop("mocked: gsDesign is not installed")
  )
  expect_error(
    graph_test_shortcut_gsDesign(g, p, 0.025, c(0.5, 1)),
    "mocked: gsDesign is not installed"
  )
})

# Print method ----------------------------------------------------------------

test_that("print method labels gsDesign spending functions and shows usTime", {
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  out <- capture.output(print(
    graph_test_shortcut_gsDesign(g, p, 0.025, c(0.5, 1), usTime = c(0.4, 1))
  ))
  expect_true(any(grepl("Group sequential engine: gsDesign", out)))
  expect_true(any(grepl("Spending functions", out)))
  expect_true(any(grepl("O'Brien-Fleming", out)))
  expect_true(any(grepl("Spending time \\(usTime\\)", out)))

  out_wt <- capture.output(print(
    graph_test_shortcut_gsDesign(
      g, p, 0.025, c(0.5, 1),
      sfu = "WT", sfupar = 0.25
    )
  ))
  expect_true(any(grepl("Wang-Tsiatis \\(Delta = 0.25\\)", out_wt)))

  out_hsd <- capture.output(print(
    graph_test_shortcut_gsDesign(
      g, p, 0.025, c(0.5, 1),
      sfu = gsDesign::sfHSD, sfupar = -2, look_back = TRUE,
      verbose = TRUE, test_values = TRUE
    )
  ))
  expect_true(any(grepl("Hwang-Shih-DeCani", out_hsd)))
  expect_true(any(grepl("parameter = -2", out_hsd)))
  expect_true(any(grepl("Boundary table", out_hsd)))
})
