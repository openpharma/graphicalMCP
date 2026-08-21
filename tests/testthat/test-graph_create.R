# graph example inputs ---------------------------------------------------------

# trivial graph
g_1 <- matrix(0, nrow = 1)
w_1 <- 1

g_1_cnm <- g_1
g_1_rnm <- g_1
g_1_nm <- g_1
w_1_nm <- w_1
colnames(g_1_cnm) <- "col_node"
rownames(g_1_rnm) <- "row_node"
colnames(g_1_nm) <- "my_node"
rownames(g_1_nm) <- "my_node"
names(w_1_nm) <- "my_node"

# Bonferroni-Holm: 2
g_2 <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
w_2 <- c(.5, .5)

# weights outside 0-1
w_under <- c(-1, .5)
w_over <- c(.5, 1.5)

# weights sum to > 1
w_sum_over <- c(.5, .75)

# Bonferroni-Holm: 3
g_3 <- matrix(c(0, .5, .5, .5, 0, .5, .5, .5, 0), nrow = 3, byrow = TRUE)
w_3 <- c(.33333333, .33333333, .33333333)

# transitions outside 0-1
g_under <- g_3
g_under[1, 2] <- -1

g_over <- g_3
g_over[1, 2] <- 1.5

# transition diagonal != 0
g_diag <- g_3
g_diag[1, ] <- c(.5, 0, .5)

# transitions sum to > 1
g_sum_over <- g_3
g_sum_over[1, ] <- c(0, .75, .75)

# floating point differences
epsilon <- sqrt(.Machine$double.eps)

w_eps <- c(.5, 0 - .Machine$double.eps, 0)
g_eps <- rbind(
  c(0, 0, 0),
  c(1 + .Machine$double.eps, 0, 0),
  c(.5, 0, 0)
)

# tests ------------------------------------------------------------------------

test_that("create a trivial graph", {
  expect_true(inherits(graph_create(w_1, g_1), "initial_graph"))
})

test_that("size of w & g differ", {
  expect_error(graph_create(w_2, g_1))
})

test_that("transition/hypothesis weights must be numeric", {
  expect_error(graph_create("1", g_1))
  expect_error(graph_create(w_1, matrix("0")))
})

test_that("hypothesis weights range and sum", {
  expect_error(graph_create(w_under, g_2))
  expect_error(graph_create(w_over, g_2))
  expect_error(graph_create(w_sum_over, g_2))
})

test_that("transition weights range, sum, and diagonal", {
  expect_error(graph_create(w_3, g_under))
  expect_error(graph_create(w_3, g_over))
  expect_error(graph_create(w_3, g_diag))
  expect_error(graph_create(w_3, g_sum_over))
})

test_that("names validation", {
  expect_true(inherits(graph_create(w_1, g_1, "node"), "initial_graph"))
  expect_warning(graph_create(w_1_nm, g_1, "node"))
  expect_error(graph_create(w_1_nm, g_1_rnm))
  expect_equal(names(graph_create(w_1, g_1)$hypotheses), "H1")
  expect_equal(names(graph_create(w_1, g_1_cnm)$hypotheses), "col_node")
  expect_equal(names(graph_create(w_1, g_1_rnm)$hypotheses), "row_node")
  expect_equal(names(graph_create(w_1_nm, g_1)$hypotheses), "my_node")
  expect_equal(names(graph_create(w_1_nm, g_1_nm)$hypotheses), "my_node")
})

test_that("floating point accuracy - errors", {
  expect_error(graph_create(c(1 + 2 * epsilon), g_1))
  expect_error(graph_create(rep(.5 + epsilon, 2), g_2))
  expect_error(
    graph_create(
      w_2,
      matrix(c(0, 1, 1 + 2 * epsilon, 0), nrow = 2)
    )
  )
  expect_error(
    graph_create(
      w_3,
      matrix(
        c(
          0, .5, .5,
          .5 + 2 * epsilon, 0, .5 + 2 * epsilon,
          .5, .5, 0
        ),
        nrow = 3
      )
    )
  )
  expect_error(graph_create(w_1, matrix(0 + 2 * epsilon)))
})

test_that("floating point accuracy - passing", {
  expect_s3_class(graph_create(c(1 + epsilon^2 / 2), g_1), "initial_graph")
  expect_s3_class(graph_create(rep(.5 + epsilon / 2, 2), g_2), "initial_graph")
  expect_s3_class(
    graph_create(
      w_2,
      matrix(c(0, 1, 1 + epsilon^2 / 2, 0), nrow = 2)
    ),
    "initial_graph"
  )
  expect_s3_class(
    graph_create(
      w_3,
      matrix(
        c(
          0, .5, .5,
          .5 + epsilon / 2, 0, .5 + epsilon / 2,
          .5, .5, 0
        ),
        nrow = 3
      )
    ),
    "initial_graph"
  )
  expect_s3_class(
    suppressWarnings(graph_create(w_1, matrix(0 + epsilon / 2))),
    "initial_graph"
  )
})

test_that("rowSums() endpoint - mixed floating point and less than 1", {
  expect_s3_class(graph_create(w_eps, g_eps), "initial_graph")
})

test_that("floating point accuracy - errors", {
  expect_error(graph_create(c(1 + 2 * epsilon), g_1))
  expect_error(graph_create(rep(.5 + epsilon, 2), g_2))
  expect_error(
    graph_create(
      w_2,
      matrix(c(0, 1, 1 + 2 * epsilon, 0), nrow = 2)
    )
  )
  expect_error(
    graph_create(
      w_3,
      matrix(
        c(
          0, .5, .5,
          .5 + 2 * epsilon, 0, .5 + 2 * epsilon,
          .5, .5, 0
        ),
        nrow = 3
      )
    )
  )
  expect_error(graph_create(w_1, matrix(0 + 2 * epsilon)))
})

test_that("floating point accuracy - passing", {
  expect_s3_class(graph_create(c(1 + epsilon^2 / 2), g_1), "initial_graph")
  expect_s3_class(graph_create(rep(.5 + epsilon / 2, 2), g_2), "initial_graph")
  expect_s3_class(
    graph_create(
      w_2,
      matrix(c(0, 1, 1 + epsilon^2 / 2, 0), nrow = 2)
    ),
    "initial_graph"
  )
  expect_s3_class(
    graph_create(
      w_3,
      matrix(
        c(
          0, .5, .5,
          .5 + epsilon / 2, 0, .5 + epsilon / 2,
          .5, .5, 0
        ),
        nrow = 3
      )
    ),
    "initial_graph"
  )
  expect_s3_class(
    suppressWarnings(graph_create(w_1, matrix(0 + epsilon / 2))),
    "initial_graph"
  )
})

test_that("rowSums() endpoint - mixed floating point and less than 1", {
  expect_s3_class(graph_create(w_eps, g_eps), "initial_graph")
})
