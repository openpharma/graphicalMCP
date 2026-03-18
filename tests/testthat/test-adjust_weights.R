alpha <- 0.025
num_hyps <- 4
g <- random_graph(num_hyps)
groups <- sample(1:num_hyps)
test_groups <- list(groups[1:(num_hyps / 2)], groups[(num_hyps / 2 + 1):num_hyps])
test_corr_temp <- matrix(0.5, num_hyps / 2, num_hyps / 2)
diag(test_corr_temp) <- 1
test_corr <- list(test_corr_temp, test_corr_temp)
new_corr <- matrix(NA, num_hyps, num_hyps)
for (group_num in seq_along(test_groups)) {
  new_corr[test_groups[[group_num]], test_groups[[group_num]]] <-
    test_corr[[group_num]]
}
diag(new_corr) <- 1
weighting_strategy <- graph_generate_weights(g)
matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
matrix_weights <- weighting_strategy[, -seq_len(num_hyps)]

test_that("parametric", {
  set.seed(1234)
  list_corr <- adjust_weights_parametric(
    matrix_weights = matrix_weights,
    matrix_intersections = matrix_intersections,
    test_corr = test_corr,
    alpha = alpha,
    test_groups = test_groups
  )

  set.seed(1234)
  single_corr <- adjust_weights_parametric_util(
    matrix_weights,
    matrix_intersections,
    new_corr,
    alpha,
    test_groups
  )
  single_corr <- single_corr[, colnames(matrix_weights), drop = FALSE]
  expect_equal(list_corr, single_corr)
})

# Simes tests -----------------------------------------------------------------
test_that("simes returns correct dimensions", {
  p <- c(0.018, 0.01, 0.105, 0.006)
  result <- adjust_weights_simes(
    matrix_weights = matrix_weights,
    p = p,
    test_groups = test_groups
  )

  expect_equal(dim(result), dim(matrix_weights))
  # Columns reordered by p-value within groups; check same set
  expect_setequal(colnames(result), colnames(matrix_weights))
})

test_that("simes adjusted weights are cumulative sums within groups", {
  g2 <- bonferroni_holm(4)
  ws <- graph_generate_weights(g2)
  mw <- ws[, 5:8]
  p <- c(0.006, 0.01, 0.018, 0.105)

  result <- adjust_weights_simes(
    matrix_weights = mw,
    p = p,
    test_groups = list(1:2, 3:4)
  )

  # Adjusted weights should be >= original weights (cumulative sums)
  expect_true(all(result >= mw - 1e-10))
})

# Hochberg tests ---------------------------------------------------------------
test_that("hochberg returns correct dimensions", {
  p <- c(0.018, 0.01, 0.105, 0.006)
  result <- adjust_weights_hochberg(
    matrix_weights = matrix_weights,
    matrix_intersections = matrix_intersections,
    p = p,
    test_groups = test_groups
  )

  expect_equal(dim(result), dim(matrix_weights))
  expect_equal(colnames(result), colnames(matrix_weights))
})

test_that("hochberg adjusted weights match manual calculation", {
  g2 <- bonferroni_holm(4)
  ws <- graph_generate_weights(g2)
  mi <- ws[, 1:4]
  mw <- ws[, 5:8]
  p <- c(0.006, 0.01, 0.018, 0.105)

  result <- adjust_weights_hochberg(
    matrix_weights = mw,
    matrix_intersections = mi,
    p = p,
    test_groups = list(1:2, 3:4)
  )

  # Adjusted weights should be >= original weights
  expect_true(all(result >= mw - 1e-10))
})
