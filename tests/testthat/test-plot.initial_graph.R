test_that("plotting throws no error", {
  skip_if_not_installed("igraph")

  graph <- graph_create(
    c(pi / 10, 1 - pi / 10, 0, 0),
    rbind(
      c(0, .5, .5, 0),
      c(.5, 0, 0, .5),
      c(1e-5, 1 - 1e-5, 0, 0),
      c(1 - 1e-5, 1e-5, 0, 0)
    )
  )

  expect_no_error(
    plot(
      graph,
      edge_curves = c("pairs" = .05, "H1|H3" = .25),
      precision = 6,
      vertex.size = 35,
      eps = 1e-4,
      background_color = "green",
      margins = 1:4 / 5
    )
  )
})
