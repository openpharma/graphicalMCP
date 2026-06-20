test_that("plotting throws no error", {
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

test_that("ellipse layout places vertices around an ellipse", {
  layout <- graphicalMCP:::graph_ellipse_layout(
    4,
    xradius = 2,
    yradius = 1,
    radian_start = 0
  )

  expected_layout <- rbind(
    c(2, 0),
    c(0, -1),
    c(-2, 0),
    c(0, 1)
  )

  expect_equal(unname(layout), expected_layout, tolerance = 1e-12)

  counter_clockwise_layout <- graphicalMCP:::graph_ellipse_layout(
    4,
    xradius = 2,
    yradius = 1,
    radian_start = 0,
    clockwise = FALSE
  )

  expect_equal(
    unname(counter_clockwise_layout),
    expected_layout[c(1, 4, 3, 2), ],
    tolerance = 1e-12
  )
})

test_that("ellipse layout validates inputs", {
  expect_error(
    graphicalMCP:::graph_ellipse_layout(4, xradius = 0),
    "`xradius` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    graphicalMCP:::graph_ellipse_layout(4, yradius = 0),
    "`yradius` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    graphicalMCP:::graph_ellipse_layout(4, radian_start = c(0, 1)),
    "`radian_start` must be `NULL` or a numeric scalar",
    fixed = TRUE
  )

  expect_error(
    graphicalMCP:::graph_ellipse_layout(4, clockwise = NA),
    "`clockwise` must be a logical scalar",
    fixed = TRUE
  )
})

test_that("edge curve defaults preserve paired edge curvature", {
  graph <- graph_create(
    c(.5, .5, 0),
    rbind(
      c(0, .5, .5),
      c(.5, 0, 0),
      c(0, 1, 0)
    )
  )
  graph_igraph <- as_igraph(graph)

  curves <- graphicalMCP:::plot_edge_curves(graph_igraph, graph)

  expect_equal(unname(curves["H1|H2"]), .25)
  expect_equal(unname(curves["H2|H1"]), .25)
  expect_equal(unname(curves["H1|H3"]), 0)
  expect_equal(unname(curves["H3|H2"]), 0)

  custom_curves <- graphicalMCP:::plot_edge_curves(
    graph_igraph,
    graph,
    edge_curves = c(default = .1, pairs = .4, "H1|H3" = .6)
  )

  expect_equal(unname(custom_curves["H1|H2"]), .4)
  expect_equal(unname(custom_curves["H2|H1"]), .4)
  expect_equal(unname(custom_curves["H1|H3"]), .6)
  expect_equal(unname(custom_curves["H3|H2"]), .1)
})

test_that("ellipse plotting throws no error", {
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
      layout = "ellipse",
      xradius = 2,
      yradius = 1,
      radian_start = 0,
      vertex.size = 35,
      vertex.size2 = 20,
      edge_curves = c(default = .1, pairs = .4, "H1|H3" = .25),
      precision = 6,
      eps = 1e-4
    )
  )
})
