test_that("round-trip graph coercion - gMCP", {
  skip_if_not(suppressWarnings(requireNamespace("gMCP", quietly = TRUE)), "gMCP not available")

  g <- random_graph(11)
  expect_equal(g, as_initial_graph(as_graphMCP(g)))
})

test_that("round-trip graph coercion - igraph", {
  skip_if_not_installed("igraph")

  g <- random_graph(11)
  expect_equal(g, as_initial_graph(as_igraph(g)))
})
