hypotheses <- c(0.5, 0.5)
transitions <- matrix(c(0, 1, 1, 0), nrow = 2)
g <- graph_create(hypotheses, transitions)

# A graph whose names have been dropped by direct assignment (Issue #110)
g_unnamed <- g
g_unnamed$hypotheses <- c(0.6, 0.4)

# A graph whose hypothesis names no longer match the transition matrix
g_mismatch <- g
names(g_mismatch$hypotheses) <- c("A", "B")

test_that("a valid graph passes validation unchanged", {
  expect_silent(test_graph_names(g))
  expect_identical(test_graph_names(g), g)
})

test_that("missing names raise an informative error, not a cryptic one", {
  expect_error(test_graph_names(g_unnamed), "missing hypothesis names")
  # the message should point the user at the real cause and the fix
  expect_error(test_graph_names(g_unnamed), "graph_create\\(\\)")
})

test_that("inconsistent names raise an informative error", {
  expect_error(test_graph_names(g_mismatch), "inconsistent hypothesis names")
})

test_that("warn_only downgrades the error to a warning and returns the graph", {
  expect_warning(out <- test_graph_names(g_unnamed, warn_only = TRUE))
  expect_identical(out, g_unnamed)
})

test_that("downstream functions give the informative error (Issue #110)", {
  expect_error(graph_generate_weights(g_unnamed), "missing hypothesis names")
  expect_error(
    graph_calculate_power(
      g_unnamed,
      alpha = 0.025,
      power_marginal = c(0.8, 0.7)
    ),
    "missing hypothesis names"
  )
  expect_error(
    graph_test_closure(g_unnamed, p = c(0.01, 0.02), alpha = 0.025),
    "missing hypothesis names"
  )
  expect_error(
    graph_test_shortcut(g_unnamed, p = c(0.01, 0.02), alpha = 0.025),
    "missing hypothesis names"
  )
  expect_error(graph_update(g_unnamed, delete = 1), "missing hypothesis names")
})

test_that("display functions warn but still proceed", {
  expect_warning(print(g_unnamed), "missing hypothesis names")
})
