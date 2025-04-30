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
