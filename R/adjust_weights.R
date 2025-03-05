#' Calculate adjusted hypothesis weights
#'
#' @description An intersection hypothesis can be rejected if its p-values are
#' less than or equal to their adjusted significance levels, which are their
#' adjusted hypothesis weights times \eqn{\alpha}. For Bonferroni tests, their
#' adjusted hypothesis weights are their hypothesis weights of the intersection
#' hypothesis. Additional adjustment is needed for parametric, Simes, and
#' Hochberg tests:
#' * Parametric tests for [adjust_weights_parametric()],
#'     - Note that one-sided tests are required for parametric tests.
#' * Simes tests for [adjust_weights_simes()],
#' * Hochberg tests for [adjust_weights_hochberg()].
#'
#' @param matrix_weights (Optional) A matrix of hypothesis weights of all
#'   intersection hypotheses. This can be obtained as the second half of columns
#'   from the output of [graph_generate_weights()].
#' @param matrix_intersections (Optional) A matrix of hypothesis indicators of
#'   all intersection hypotheses. This can be obtained as the first half of
#'   columns from the output of [graph_generate_weights()].
#' @param alpha (Optional) A numeric value of the overall significance level,
#'   which should be between 0 & 1. The default is 0.025 for one-sided
#'   hypothesis testing problems; another common choice is 0.05 for two-sided
#'   hypothesis testing problems. Note when parametric tests are used, only
#'   one-sided tests are supported.
#' @param p (Optional) A numeric vector of p-values (unadjusted, raw), whose
#'   values should be between 0 & 1. The length should match the number of
#'   columns of `matrix_weights`.
#' @param test_corr (Optional) A numeric matrix of correlations between test
#'   statistics, which is needed to perform parametric tests using
#'   [adjust_weights_parametric()]. The number of rows and columns of this
#'   correlation matrix should match the length of `p`.
#' @param test_groups (Optional) A list of numeric vectors specifying hypotheses
#'   to test together. Grouping is needed to correctly perform Simes and
#'   parametric tests.
#' @param maxpts (Optional) An integer scalar for the maximum number of function
#'   values, which is needed to perform parametric tests using the
#'   `mvtnorm::GenzBretz` algorithm. The default is 25000.
#' @param abseps (Optional) A numeric scalar for the absolute error tolerance,
#'   which is needed to perform parametric tests using the `mvtnorm::GenzBretz`
#'   algorithm. The default is 1e-6.
#' @param releps (Optional) A numeric scalar for the relative error tolerance as
#'   double, which is needed to perform parametric tests using the
#'   `mvtnorm::GenzBretz` algorithm. The default is 0.
#'
#' @return
#' * [adjust_weights_parametric()] returns a matrix with the same
#' dimensions as `matrix_weights`, whose hypothesis weights have been adjusted
#' according to parametric tests.
#' * [adjust_weights_simes()] returns a matrix with the same
#' dimensions as `matrix_weights`, whose hypothesis weights have been adjusted
#' according to Simes tests.
#' * [adjust_weights_hochberg()] returns a matrix with the same
#' dimensions as `matrix_weights`, whose hypothesis weights have been adjusted
#' according to Hochberg tests.
#'
#' @seealso [adjust_p_parametric()] for adjusted p-values using parametric
#' tests, [adjust_p_simes()] for adjusted p-values using Simes tests,
#' [adjust_p_hochberg()] for adjusted p-values using Hochberg tests.
#'
#' @rdname adjust_weights
#'
#' @export
#'
#' @references Lu, K. (2016). Graphical approaches using a Bonferroni mixture of
#' weighted Simes tests. \emph{Statistics in Medicine}, 35(22), 4041-4055.
#'
#' Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework for
#' weighted parametric multiple test procedures. \emph{Biometrical Journal},
#' 59(5), 918-931.
#'
#' Xi, D., and Bretz, F. (2019). Symmetric graphs for equally weighted tests,
#' with application to the Hochberg procedure. \emph{Statistics in Medicine},
#' 38(27), 5268-5282.
#'
#' @examples
#' alpha <- 0.025
#' p <- c(0.018, 0.01, 0.105, 0.006)
#' num_hyps <- length(p)
#' g <- bonferroni_holm(rep(1 / 4, 4))
#' weighting_strategy <- graph_generate_weights(g)
#' matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
#' matrix_weights <- weighting_strategy[, -seq_len(num_hyps)]
#'
#' set.seed(1234)
#' adjust_weights_parametric(
#'   matrix_weights = matrix_weights,
#'   matrix_intersections = matrix_intersections,
#'   test_corr = diag(4),
#'   alpha = alpha,
#'   test_groups = list(1:4)
#' )
adjust_weights_parametric <- function(matrix_weights,
                                      matrix_intersections,
                                      test_corr,
                                      alpha,
                                      test_groups,
                                      maxpts = 25000,
                                      abseps = 1e-6,
                                      releps = 0) {
  c_values <- matrix(
    nrow = nrow(matrix_weights),
    ncol = ncol(matrix_weights),
    dimnames = dimnames(matrix_weights)
  )

  for (group in test_groups) {
    for (row in seq_len(nrow(matrix_weights))) {
      group_by_intersection <-
        group[as.logical(matrix_intersections[row, , drop = TRUE][group])]

      group_c_value <- solve_c_parametric(
        matrix_weights[row, group_by_intersection, drop = TRUE],
        test_corr[group_by_intersection, group_by_intersection, drop = FALSE],
        alpha,
        maxpts,
        abseps,
        releps
      )

      c_values[row, group] <-
        group_c_value * matrix_intersections[row, group, drop = TRUE]
    }
  }

  adjusted_weights <- c_values * matrix_weights

  adjusted_weights[, unlist(test_groups), drop = FALSE]
}

#' @rdname adjust_weights
#' @export
#' @examples
#' alpha <- 0.025
#' p <- c(0.018, 0.01, 0.105, 0.006)
#' num_hyps <- length(p)
#' g <- bonferroni_holm(rep(1 / 4, 4))
#' weighting_strategy <- graph_generate_weights(g)
#' matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
#' matrix_weights <- weighting_strategy[, -seq_len(num_hyps)]
#'
#' adjust_weights_simes(
#'   matrix_weights = matrix_weights,
#'   p = p,
#'   test_groups = list(1:4)
#' )
adjust_weights_simes <- function(matrix_weights, p, test_groups) {
  ordered_p <- order(p)

  matrix_weights <- matrix_weights[, ordered_p, drop = FALSE]

  group_adjusted_weights <- vector("list", length(test_groups))
  for (i in seq_along(test_groups)) {
    group_adjusted_weights[[i]] <- matrixStats::rowCumsums(
      matrix_weights[, ordered_p %in% test_groups[[i]], drop = FALSE],
      useNames = TRUE
    )
  }

  do.call(cbind, group_adjusted_weights)
}

#' @rdname adjust_weights
#' @export
#' @examples
#' alpha <- 0.025
#' p <- c(0.018, 0.01, 0.105, 0.006)
#' num_hyps <- length(p)
#' g <- bonferroni_holm(rep(1 / 4, 4))
#' weighting_strategy <- graph_generate_weights(g)
#' matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
#' matrix_weights <- weighting_strategy[, -seq_len(num_hyps)]
#'
#' adjust_weights_hochberg(
#'   matrix_weights = matrix_weights,
#'   matrix_intersections = matrix_intersections,
#'   p = p,
#'   test_groups = list(1:4)
#' )
adjust_weights_hochberg <- function(matrix_weights,
                                    matrix_intersections,
                                    p,
                                    test_groups) {
  ordered_p <- order(p)

  ordered_matrix_weights <- matrix_weights[, ordered_p, drop = FALSE]

  ordered_matrix_intersections <-
    matrix_intersections[, ordered_p, drop = FALSE]

  group_lengths <- lengths(test_groups)
  group_adjusted_weights <- vector("list", length(test_groups))
  for (i in seq_along(test_groups)) {
    test_group <- ordered_p %in% test_groups[[i]]
    rev_group <- rev(seq_len(group_lengths[[i]]))

    group_weights <- ordered_matrix_weights[, test_group, drop = FALSE]

    group_total_weights <- matrixStats::rowSums2(group_weights)

    group_intersections <-
      ordered_matrix_intersections[, test_group, drop = FALSE]

    group_intersection_sums <-
      matrixStats::rowCumsums(
        group_intersections[, rev_group, drop = FALSE]
      )[, rev_group, drop = FALSE]

    group_adjusted_weights[[i]] <- group_total_weights / group_intersection_sums
  }

  do.call(cbind, group_adjusted_weights)
}
