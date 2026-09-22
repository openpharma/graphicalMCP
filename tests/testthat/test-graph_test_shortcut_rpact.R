# graph_test_shortcut_rpact() is validated against graph_test_shortcut_gsDesign(),
# which integrates deterministically; the two agree to the tolerance of the
# root-finding involved (about 1e-6 on the p-value scale).

rpact_graph <- function() {
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

rpact_p <- function() {
  rbind(
    H1 = c(0.0062, 0.0002),
    H2 = c(0.017, 0.0035),
    H3 = c(0.009, 0.002),
    H4 = c(0.13, 0.06)
  )
}

expect_close_rpact <- function(new, old, tol = 1e-5) {
  expect_identical(is.na(new), is.na(old))
  expect_lt(max(abs(new - old), na.rm = TRUE), tol)
}

# The two engines must make identical decisions and agree numerically
expect_rpact_equivalent <- function(new, old, tol = 1e-5) {
  for (field in c(
    "rejected", "decision_at", "first_rejected_at", "last_rejected_at",
    "rejection_sequence", "graph"
  )) {
    expect_identical(new$outputs[[field]], old$outputs[[field]])
  }
  for (field in c("repeated_p", "sequential_p", "adjusted_p")) {
    expect_close_rpact(new$outputs[[field]], old$outputs[[field]], tol)
  }
  if (!is.null(old$boundary_table)) {
    expect_identical(names(new$boundary_table), names(old$boundary_table))
    for (h in names(old$boundary_table)) {
      expect_close_rpact(
        as.matrix(new$boundary_table[[h]]), as.matrix(old$boundary_table[[h]]),
        tol
      )
    }
  }
  if (!is.null(old$test_values)) {
    expect_length(new$test_values, length(old$test_values))
    for (k in seq_along(old$test_values)) {
      tv_new <- new$test_values[[k]]
      tv_old <- old$test_values[[k]]
      if (is.null(tv_old)) {
        expect_null(tv_new)
        next
      }
      for (col in c("Analysis", "Hypothesis", "Reject", "Look_back")) {
        expect_identical(tv_new[[col]], tv_old[[col]])
      }
      expect_close_rpact(tv_new$Weight, tv_old$Weight, tol)
      expect_close_rpact(tv_new$Boundary, tv_old$Boundary, tol)
    }
  }
  invisible(TRUE)
}

# Structure ------------------------------------------------------------------

test_that("graph_test_shortcut_rpact returns a gsd_graph_report with rpact inputs", {
  skip_if_not_installed("rpact")
  result <- graph_test_shortcut_rpact(
    rpact_graph(), rpact_p(),
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3)
  )

  expect_s3_class(result, "gsd_graph_report")
  expect_identical(result$inputs$engine, "rpact")
  expect_null(result$inputs$spending_fn)
  expect_equal(result$inputs$typeOfDesign, list(H1 = "asOF", H2 = "asOF", H3 = "asOF", H4 = "asOF"))
  expect_length(result$inputs$gammaA, 4)
  expect_null(result$inputs$spending_time)
  expect_length(result$outputs$adjusted_p, 4)
  expect_identical(names(result$outputs$rejected), c("H1", "H2", "H3", "H4"))
})

test_that("graph_test_shortcut_rpact is deterministic", {
  skip_if_not_installed("rpact")
  args <- list(rpact_graph(), rpact_p(), alpha = 0.025, info_frac = c(1 / 3, 2 / 3), look_back = TRUE)
  expect_identical(
    do.call(graph_test_shortcut_rpact, args)$outputs,
    do.call(graph_test_shortcut_rpact, args)$outputs
  )
})

# Equivalence with graph_test_shortcut_gsDesign() ----------------------------

test_that("rpact matches gsDesign on the Maurer-Bretz example", {
  skip_if_not_installed("rpact")
  skip_if_not_installed("gsDesign")
  for (lb in c(FALSE, TRUE)) {
    new <- graph_test_shortcut_rpact(
      rpact_graph(), rpact_p(),
      alpha = 0.025, info_frac = c(1 / 3, 2 / 3),
      look_back = lb, verbose = TRUE, test_values = TRUE
    )
    old <- graph_test_shortcut_gsDesign(
      rpact_graph(), rpact_p(),
      alpha = 0.025, info_frac = c(1 / 3, 2 / 3), sfu = gsDesign::sfLDOF,
      look_back = lb, verbose = TRUE, test_values = TRUE
    )
    expect_rpact_equivalent(new, old)
    expect_identical(new$outputs$rejected, c(H1 = TRUE, H2 = TRUE, H3 = TRUE, H4 = FALSE))
  }
})

test_that("rpact matches gsDesign for look-back rejections and per-hypothesis look_back", {
  skip_if_not_installed("rpact")
  skip_if_not_installed("gsDesign")
  p <- rpact_p()
  p["H3", ] <- c(0.0008, 0.006)

  new <- graph_test_shortcut_rpact(
    rpact_graph(), p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), typeOfDesign = "asP",
    look_back = TRUE, test_values = TRUE
  )
  old <- graph_test_shortcut_gsDesign(
    rpact_graph(), p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), sfu = gsDesign::sfLDPocock,
    look_back = TRUE, test_values = TRUE
  )
  expect_rpact_equivalent(new, old)
  expect_true(any(new$test_values[[2]]$Look_back))

  new_mixed <- graph_test_shortcut_rpact(
    rpact_graph(), p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3),
    look_back = c(TRUE, FALSE, TRUE, FALSE), test_values = TRUE
  )
  old_mixed <- graph_test_shortcut_gsDesign(
    rpact_graph(), p,
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3),
    look_back = c(TRUE, FALSE, TRUE, FALSE), test_values = TRUE
  )
  expect_rpact_equivalent(new_mixed, old_mixed)
})

test_that("rpact design types match gsDesign's spending functions", {
  skip_if_not_installed("rpact")
  skip_if_not_installed("gsDesign")
  pairs <- list(
    list(type = "asOF", gammaA = NULL, sfu = gsDesign::sfLDOF, sfupar = NULL),
    list(type = "asP", gammaA = NULL, sfu = gsDesign::sfLDPocock, sfupar = NULL),
    list(type = "asHSD", gammaA = -2, sfu = gsDesign::sfHSD, sfupar = -2),
    list(type = "asKD", gammaA = 1, sfu = gsDesign::sfPower, sfupar = 1)
  )
  for (pair in pairs) {
    new <- graph_test_shortcut_rpact(
      rpact_graph(), rpact_p(),
      alpha = 0.025, info_frac = c(1 / 3, 2 / 3),
      typeOfDesign = pair$type, gammaA = pair$gammaA, test_values = TRUE
    )
    old <- graph_test_shortcut_gsDesign(
      rpact_graph(), rpact_p(),
      alpha = 0.025, info_frac = c(1 / 3, 2 / 3),
      sfu = pair$sfu, sfupar = pair$sfupar, test_values = TRUE
    )
    expect_rpact_equivalent(new, old)
  }
})

test_that("rpact matches gsDesign with per-hypothesis designs and information", {
  skip_if_not_installed("rpact")
  skip_if_not_installed("gsDesign")
  info <- rbind(c(0.3, 0.7), c(0.5, 1), c(0.4, 0.8), c(0.25, 0.75))

  new <- graph_test_shortcut_rpact(
    rpact_graph(), rpact_p(),
    alpha = 0.025, info_frac = info,
    typeOfDesign = list("asOF", "asP", "asHSD", "asKD"),
    gammaA = list(NULL, NULL, -2, 1),
    look_back = TRUE, test_values = TRUE
  )
  old <- graph_test_shortcut_gsDesign(
    rpact_graph(), rpact_p(),
    alpha = 0.025, info_frac = info,
    sfu = list(gsDesign::sfLDOF, gsDesign::sfLDPocock, gsDesign::sfHSD, gsDesign::sfPower),
    sfupar = list(NULL, NULL, -2, 1),
    look_back = TRUE, test_values = TRUE
  )
  expect_rpact_equivalent(new, old)
})

test_that("rpact matches gsDesign with NA padding and single-analysis hypotheses", {
  skip_if_not_installed("rpact")
  skip_if_not_installed("gsDesign")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p_na <- rbind(H1 = c(0.024, 0.01, NA), H2 = c(0.015, 0.005, 0.001))
  info_na <- rbind(c(0.5, 1, NA), c(1 / 3, 2 / 3, 1))
  for (lb in c(FALSE, TRUE)) {
    new <- graph_test_shortcut_rpact(g, p_na, 0.025, info_na, look_back = lb, test_values = TRUE)
    old <- graph_test_shortcut_gsDesign(g, p_na, 0.025, info_na, look_back = lb, test_values = TRUE)
    expect_rpact_equivalent(new, old)
  }

  # H1 has a single analysis at 60% information, H2 two analyses
  p_one <- rbind(H1 = c(0.0009, NA), H2 = c(0.02, 0.004))
  info_one <- rbind(c(0.6, NA), c(0.5, 1))
  new <- graph_test_shortcut_rpact(g, p_one, 0.025, info_one, look_back = TRUE, verbose = TRUE)
  old <- graph_test_shortcut_gsDesign(g, p_one, 0.025, info_one, look_back = TRUE, verbose = TRUE)
  expect_rpact_equivalent(new, old)
  # A single analysis at full information is a fixed-sample test
  p_full <- rbind(H1 = c(0.01, NA), H2 = c(0.02, 0.004))
  info_full <- rbind(c(1, NA), c(0.5, 1))
  new <- graph_test_shortcut_rpact(g, p_full, 0.025, info_full)
  expect_equal(unname(new$outputs$repeated_p["H1", 1]), 0.01)
})

test_that("rpact spending time and over-running information match gsDesign's usTime", {
  skip_if_not_installed("rpact")
  skip_if_not_installed("gsDesign")
  # Spending time differs from the information fraction
  new <- graph_test_shortcut_rpact(
    rpact_graph(), rpact_p(),
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), spending_time = c(0.5, 0.8),
    look_back = TRUE, test_values = TRUE
  )
  old <- graph_test_shortcut_gsDesign(
    rpact_graph(), rpact_p(),
    alpha = 0.025, info_frac = c(1 / 3, 2 / 3), usTime = c(0.5, 0.8),
    look_back = TRUE, test_values = TRUE
  )
  expect_rpact_equivalent(new, old)
  expect_identical(dim(new$inputs$spending_time), c(4L, 2L))

  # Over-running information at the final analysis, spending time capped at 1.
  # H4's p-values are kept away from rpact's level ceiling (see the engine
  # tests), which its original first-analysis p-value of 0.13 would exceed
  # under Hwang-Shih-DeCani spending.
  p3 <- cbind(rpact_p(), c(0.0001, 0.001, 0.0015, 0.03))
  p3["H4", ] <- c(0.03, 0.02, 0.01)
  for (type in c("asOF", "asHSD")) {
    new <- graph_test_shortcut_rpact(
      rpact_graph(), p3,
      alpha = 0.025, info_frac = c(1 / 3, 2 / 3, 1.1),
      typeOfDesign = type, gammaA = if (type == "asHSD") -2,
      look_back = TRUE, test_values = TRUE
    )
    old <- graph_test_shortcut_gsDesign(
      rpact_graph(), p3,
      alpha = 0.025, info_frac = c(1 / 3, 2 / 3, 1.1),
      sfu = if (type == "asOF") gsDesign::sfLDOF else gsDesign::sfHSD,
      sfupar = if (type == "asHSD") -2,
      look_back = TRUE, test_values = TRUE
    )
    expect_rpact_equivalent(new, old)
  }

  # Monitoring: spending at the planned schedule, correlation from the actual
  new <- graph_test_shortcut_rpact(
    rpact_graph(), p3,
    alpha = 0.025, info_frac = c(0.3, 0.6, 1), spending_time = c(1 / 3, 2 / 3, 1),
    look_back = TRUE
  )
  old <- graph_test_shortcut_gsDesign(
    rpact_graph(), p3,
    alpha = 0.025, info_frac = c(0.3, 0.6, 1), usTime = c(1 / 3, 2 / 3, 1),
    look_back = TRUE
  )
  expect_rpact_equivalent(new, old)
})

test_that("rpact's classical families match gsDesign's", {
  skip_if_not_installed("rpact")
  skip_if_not_installed("gsDesign")
  # rpact needs the full planned schedule for the classical families
  p3 <- cbind(rpact_p(), c(0.0001, 0.001, 0.0015, 0.03))
  t3 <- c(1 / 3, 2 / 3, 1)

  # Both packages compute Wang-Tsiatis boundaries exactly
  new <- graph_test_shortcut_rpact(
    rpact_graph(), p3,
    alpha = 0.025, info_frac = t3, typeOfDesign = "WT", deltaWT = 0.25,
    look_back = TRUE, test_values = TRUE, verbose = TRUE
  )
  old <- graph_test_shortcut_gsDesign(
    rpact_graph(), p3,
    alpha = 0.025, info_frac = t3, sfu = "WT", sfupar = 0.25,
    look_back = TRUE, test_values = TRUE, verbose = TRUE
  )
  expect_rpact_equivalent(new, old, tol = 1e-6)

  new <- graph_test_shortcut_rpact(
    rpact_graph(), p3,
    alpha = 0.025, info_frac = t3, typeOfDesign = list("OF", "P", "OF", "P"),
    test_values = TRUE
  )
  old <- graph_test_shortcut_gsDesign(
    rpact_graph(), p3,
    alpha = 0.025, info_frac = t3, sfu = list("OF", "Pocock", "OF", "Pocock"),
    test_values = TRUE
  )
  expect_rpact_equivalent(new, old, tol = 1e-6)

  # spending_time is ignored for classical families, with a warning
  expect_warning(
    with_st <- graph_test_shortcut_rpact(
      rpact_graph(), p3,
      alpha = 0.025, info_frac = t3, typeOfDesign = "WT", deltaWT = 0.25,
      spending_time = c(0.4, 0.9, 1)
    ),
    "spending_time does not apply"
  )
  without_st <- graph_test_shortcut_rpact(
    rpact_graph(), p3,
    alpha = 0.025, info_frac = t3, typeOfDesign = "WT", deltaWT = 0.25
  )
  expect_identical(with_st$outputs$repeated_p, without_st$outputs$repeated_p)
})

test_that("asUser reproduces asOF at the full level and scales proportionally", {
  skip_if_not_installed("rpact")
  t2 <- c(1 / 3, 2 / 3)
  spent <- rpact::getDesignGroupSequential(
    kMax = 2, informationRates = t2, alpha = 0.025, sided = 1, typeOfDesign = "asOF"
  )$alphaSpent
  user <- graph_test_shortcut_rpact(
    rpact_graph(), rpact_p(),
    alpha = 0.025, info_frac = t2, typeOfDesign = "asUser",
    userAlphaSpending = spent, verbose = TRUE
  )
  obf <- graph_test_shortcut_rpact(
    rpact_graph(), rpact_p(),
    alpha = 0.025, info_frac = t2, verbose = TRUE
  )
  # At weight 1 the two designs coincide (up to rpact's rounding of the
  # cumulative spending it reports)
  bt_user <- user$boundary_table$H1
  bt_obf <- obf$boundary_table$H1
  expect_lt(
    max(abs(
      unlist(bt_user[bt_user$Weight == 1, c("Boundary.1", "Boundary.2")]) -
        unlist(bt_obf[bt_obf$Weight == 1, c("Boundary.1", "Boundary.2")])
    )),
    1e-7
  )
  # At other weights the user spending is scaled proportionally, which is
  # not how O'Brien-Fleming spending scales
  expect_false(isTRUE(all.equal(
    bt_user$Boundary.1[bt_user$Weight == 0.5], bt_obf$Boundary.1[bt_obf$Weight == 0.5]
  )))
  expect_equal(user$inputs$userAlphaSpending$H1, spent)
})

# Engine ---------------------------------------------------------------------

test_that("the rpact engine reproduces rpact's own repeated p-values below 0.5", {
  skip_if_not_installed("rpact")
  t3 <- c(1 / 3, 2 / 3, 1)
  engine <- gsd_engine_rpact(
    0.025, matrix(t3, 1), list("asOF"), list(NULL), list(NULL), list(NULL), NULL
  )
  design <- rpact::getDesignGroupSequential(
    kMax = 3, informationRates = t3, alpha = 0.025, sided = 1, typeOfDesign = "asOF"
  )
  p <- c(0.05, 0.02, 0.01)
  for (k in 1:3) {
    cum_n <- c(1e6, 2e6, 3e6)[1:k]
    data <- rpact::getDataset(
      cumMeans = stats::qnorm(1 - p[1:k]) / sqrt(cum_n), cumStDevs = rep(1, k), cumN = cum_n
    )
    reference <- as.numeric(rpact::getRepeatedPValues(
      rpact::getStageResults(design, data, normalApproximation = TRUE)
    ))[k]
    # rpact's own search runs at an absolute tolerance of 1e-6
    expect_lt(abs(engine$repeated_p(p[1:k], t3[1:k], 1) - reference), 1e-5)
  }
})

test_that("the rpact engine reports 1 above the level rpact can compute", {
  skip_if_not_installed("rpact")
  skip_if_not_installed("gsDesign")
  info <- matrix(c(0.1, 0.5, 1), 1)
  rp <- gsd_engine_rpact(0.025, info, list("asOF"), list(NULL), list(NULL), list(NULL), NULL)
  gs <- gsd_engine_gsDesign(0.025, info, list(gsDesign::sfLDOF), list(NULL), NULL)

  # Weak evidence at 10% information is crossed only at a level (about 0.83)
  # above rpact's ceiling: reported as 1
  expect_gt(gs$repeated_p(0.5, 0.1, 1), 0.7)
  expect_equal(rp$repeated_p(0.5, 0.1, 1), 1)
  # Below the ceiling the engines agree
  expect_equal(rp$repeated_p(0.05, 0.1, 1), gs$repeated_p(0.05, 0.1, 1), tolerance = 1e-5)
  # Boundaries at levels 0 and 1 are trivial
  expect_equal(rp$nominal_bounds(0, info[1, ], 1), c(0, 0, 0))
  expect_equal(rp$nominal_bounds(1, info[1, ], 1), c(1, 1, 1))
  # rpact's warnings about its validated range are not surfaced
  expect_silent(rp$nominal_bounds(1e-7, info[1, ], 1))
  expect_silent(rp$repeated_p(c(1e-9, 1e-9), info[1, 1:2], 1))
})

# Input handling --------------------------------------------------------------

test_that("invalid rpact inputs throw errors", {
  skip_if_not_installed("rpact")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  expect_error(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1), typeOfDesign = "foo"),
    "typeOfDesign must be"
  )
  expect_error(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1), typeOfDesign = list("asOF")),
    "Number of design types"
  )
  expect_error(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1), typeOfDesign = "asHSD"),
    "gammaA must be a single number"
  )
  expect_error(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1), typeOfDesign = "WT"),
    "deltaWT must be a single number"
  )
  expect_error(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1), typeOfDesign = "asUser"),
    "userAlphaSpending must give"
  )
  expect_error(
    graph_test_shortcut_rpact(
      g, p, 0.025, c(0.5, 1),
      typeOfDesign = "asUser", userAlphaSpending = c(0.02, 0.01)
    ),
    "non-decreasing"
  )
  expect_error(
    graph_test_shortcut_rpact(
      g, p, 0.025, c(0.5, 1),
      typeOfDesign = "asUser", userAlphaSpending = c(0.01, 0.03)
    ),
    "between 0 and alpha"
  )
  # Classical families need the full planned schedule ending at 1
  expect_error(
    graph_test_shortcut_rpact(
      g, p, 0.025, c(0.5, 1.1),
      typeOfDesign = "WT", deltaWT = 0.25
    ),
    "exactly 1"
  )
  expect_error(
    graph_test_shortcut_rpact(
      g, p, 0.025, c(1 / 3, 2 / 3),
      typeOfDesign = "OF"
    ),
    "exactly 1"
  )
  # spending_time shape and content
  expect_error(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1), spending_time = c(0.4, 1, 1)),
    "spending_time"
  )
  expect_error(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1), spending_time = c(-0.1, 1)),
    "non-negative"
  )
  expect_error(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1), spending_time = c(1, 0.5)),
    "non-decreasing"
  )
  p_na <- rbind(H1 = c(0.024, 0.01, NA), H2 = c(0.015, 0.005, 0.001))
  info_na <- rbind(c(0.5, 1, NA), c(1 / 3, 2 / 3, 1))
  expect_error(
    graph_test_shortcut_rpact(g, p_na, 0.025, info_na, spending_time = c(0.5, 1, 1)),
    "spending_time must be a matrix"
  )
  expect_error(
    graph_test_shortcut_rpact(
      g, p_na, 0.025, info_na,
      spending_time = rbind(c(0.5, 1, 1), c(1 / 3, 2 / 3, 1))
    ),
    "NA positions"
  )
  # The shared validation still applies
  expect_error(graph_test_shortcut_rpact(unclass(g), p, 0.025, c(0.5, 1)))
  expect_error(graph_test_shortcut_rpact(g, p, 1.5, c(0.5, 1)))
})

test_that("the rpact guard is checked before anything else", {
  skip_if_not_installed("rpact")
  skip_if_not_installed("testthat", "3.2.0")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  expect_true(gsd_check_rpact())
  local_mocked_bindings(
    gsd_check_rpact = function() stop("mocked: rpact is not installed")
  )
  expect_error(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1)),
    "mocked: rpact is not installed"
  )
})

# Print method ----------------------------------------------------------------

test_that("print method labels rpact designs and shows the spending time", {
  skip_if_not_installed("rpact")
  g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
  p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))

  out <- capture.output(print(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1), spending_time = c(0.4, 1))
  ))
  expect_true(any(grepl("Group sequential engine: rpact", out)))
  expect_true(any(grepl("asOF \\(Lan-DeMets O'Brien-Fleming\\)", out)))
  expect_true(any(grepl("Spending time$", out)))

  out_wt <- capture.output(print(
    graph_test_shortcut_rpact(g, p, 0.025, c(0.5, 1), typeOfDesign = "WT", deltaWT = 0.25)
  ))
  expect_true(any(grepl("WT \\(Wang-Tsiatis, deltaWT = 0.25\\)", out_wt)))

  out_hsd <- capture.output(print(
    graph_test_shortcut_rpact(
      g, p, 0.025, c(0.5, 1),
      typeOfDesign = "asHSD", gammaA = -2, look_back = TRUE,
      verbose = TRUE, test_values = TRUE
    )
  ))
  expect_true(any(grepl("asHSD \\(Hwang-Shih-DeCani, gammaA = -2\\)", out_hsd)))
  expect_true(any(grepl("Boundary table", out_hsd)))
})
