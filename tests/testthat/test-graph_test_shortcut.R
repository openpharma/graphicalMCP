test_that("results match graph_test_closure()", {
  rando <- random_graph(4)
  p <- pnorm(rnorm(4, 2), lower.tail = FALSE)
  names(p) <- names(rando$hypotheses)

  expect_equal(
    graph_test_shortcut(rando, p),
    graph_test_closure(rando, p),
    ignore_attr = TRUE
  )

  expect_equal(
    graph_test_shortcut(rando, p)$outputs$rejected,
    graph_test_shortcut_fast(
      p,
      0.025,
      graph_generate_weights(rando)[, 5:8, drop = FALSE]
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    graph_test_shortcut(rando, p)$outputs$rejected,
    graph_test_closure_fast(
      p,
      0.025,
      graph_generate_weights(rando)[, 5:8, drop = FALSE],
      graph_generate_weights(rando)[, 1:4, drop = FALSE]
    ),
    ignore_attr = TRUE
  )

  expect_s3_class(
    graph_test_shortcut(rando, p, test_values = TRUE)$test_values$results,
    "data.frame"
  )

  expect_type(
    graph_test_shortcut(rando, p, verbose = TRUE)$details$results,
    "list"
  )
})

test_that("adjusted p-values are capped at 1", {
  expect_equal(
    graph_test_shortcut(random_graph(2), c(1, 1))$outputs$adjusted_p,
    c(1, 1),
    ignore_attr = TRUE
  )
})

test_that("fast shortcut testing matches closure testing", {
  g <- simple_successive_2()
  p <- c(
    3.51345772970616e-08,
    7.35572350934915e-06,
    3.31393509575894e-07,
    1.23186608577966e-05
  )

  adjusted_weights <- graph_generate_weights(g)[, 5:8, drop = FALSE]

  expect_equal(
    as.logical(graph_test_shortcut_fast(p, .025, adjusted_weights)),
    unname(graph_test_closure(g, p)$outputs$rejected)
  )
})

test_that("shortcut testing handles 0 cases", {
  g_zero_1 <- graph_create(c(.5, .5, 0), matrix(0, 3, 3))
  g_zero_2 <- graph_create(rep(0, 3), matrix(0, 3, 3))

  p_zero_1 <- c(1, 0, 0)
  p_zero_2 <- rep(0, 3)

  expect_error(
    graph_test_shortcut(g_zero_1, p_zero_1),
    regexp = "Calculation of adjusted p-values stops when all remaining"
  )
  expect_error(
    graph_test_shortcut(g_zero_1, p_zero_1),
    regexp = "Calculation of adjusted p-values stops when all remaining"
  )
  expect_error(
    graph_test_shortcut(g_zero_1, p_zero_1),
    regexp = "Calculation of adjusted p-values stops when all remaining"
  )
  expect_error(
    graph_test_shortcut(g_zero_1, p_zero_1),
    regexp = "Calculation of adjusted p-values stops when all remaining"
  )

  expect_equal(
    graph_test_shortcut(g_zero_2, rep(.001, 3))$outputs$adjusted_p,
    rep(1, 3),
    ignore_attr = TRUE
  )

  expect_equal(
    graph_test_shortcut(bonferroni_holm(rep(1 / 3, 3)),
                        p_zero_2)$outputs$adjusted_p,
    rep(0, 3),
    ignore_attr = TRUE
  )

  expect_no_error(graph_test_shortcut(bonferroni_holm(rep(1 / 3, 3)), p_zero_2))
})

test_that("shortcut testing rejects none when adjusted p-values exceed 1", {
  expect_equal(
    graph_test_shortcut(random_graph(6), rep(1, 6), 1)$outputs$rejected,
    rep(FALSE, 6),
    ignore_attr = TRUE
  )
})

test_that("shortcut internal consistency", {
  rando <- random_graph(6)
  p <- pnorm(rnorm(6, 2), lower.tail = FALSE)

  shortcut_results <- graph_test_shortcut(rando, p, .025, TRUE, TRUE)

  expect_equal(
    shortcut_results$inputs$graph,
    shortcut_results$details$results[[1]],
    ignore_attr = TRUE
  )

  test_values_sequence <- shortcut_results$test_values$results$Hypothesis
  expect_equal(
    shortcut_results$inputs$p[test_values_sequence],
    shortcut_results$test_values$results$p,
    ignore_attr = TRUE
  )

  expect_equal(
    shortcut_results$inputs$alpha,
    shortcut_results$test_values$results$Alpha[[1]],
    ignore_attr = TRUE
  )

  expect_equal(
    shortcut_results$outputs$rejected[test_values_sequence],
    shortcut_results$test_values$results$Inequality_holds,
    ignore_attr = TRUE
  )

  steps <- seq_along(which(shortcut_results$outputs$rejected))
  expect_equal(
    if (any(shortcut_results$outputs$rejected)) {
      c(steps, rep(max(steps) + 1, length(rando$hypotheses) - length(steps)))
    } else {
      rep(1, length(rando$hypotheses)) # All step 1 if non rejected
    },
    shortcut_results$test_values$results$Step
  )

  last_graph_index <- length(shortcut_results$details$results)
  expect_equal(
    shortcut_results$outputs$graph,
    shortcut_results$details$results[[last_graph_index]],
    ignore_attr = TRUE
  )
})

test_that("adjusted p that exceeds alpha by floating point diff is rejected", {
  g <- graph_create(
    c(.5, .5, 0, 0, 0, 0),
    rbind(
      c(0.0000, 0.5000, 0.2500, 0.0000, 0.2500, 0.0000),
      c(0.5000, 0.0000, 0.0000, 0.2500, 0.0000, 0.2500),
      c(0.0000, 0.0000, 0.0000, 0.0000, 1.0000, 0.0000),
      c(0.0001, 0.0000, 0.0000, 0.0000, 0.0000, 0.9999),
      c(0.0000, 0.0001, 0.9999, 0.0000, 0.0000, 0.0000),
      c(0.0000, 0.0000, 0.0000, 1.0000, 0.0000, 0.0000)
    )
  )

  p <- c(0.001, 0.01875, 0.013, .0002, 0.03, 0.04)

  expect_equal(
    graph_test_shortcut(g, p)$outputs$rejected,
    c(H1 = TRUE, H2 = TRUE, H3 = FALSE, H4 = TRUE, H5 = FALSE, H6 = FALSE)
  )
})
