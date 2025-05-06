#' Calculate adjusted hypothesis weights for parametric tests
#'
#' @description
#' An intersection hypothesis can be rejected if its p-values are less than or
#' equal to their adjusted significance levels, which are their adjusted
#' hypothesis weights times \eqn{\alpha}. For Bonferroni tests, their adjusted
#' hypothesis weights are their hypothesis weights of the intersection
#' hypothesis. Additional adjustment is needed for parametric tests:
#' * Parametric tests for [adjust_weights_parametric()],
#'     - Note that one-sided tests are required for parametric tests.
#'
#' @param x The root to solve for with `stats::uniroot()`.
#' @param alpha (Optional) A numeric value of the overall significance level,
#'   which should be between 0 & 1. The default is 0.025 for one-sided
#'   hypothesis testing problems; another common choice is 0.05 for two-sided
#'   hypothesis testing problems. Note when parametric tests are used, only
#'   one-sided tests are supported.
#' @param hypotheses A numeric vector of hypothesis weights. Must be a vector of
#'   values between 0 & 1 (inclusive). The sum of hypothesis weights should not
#'   exceed 1.
#' @param test_corr (Optional) A numeric matrix of correlations between test
#'   statistics, which is needed to perform parametric tests using
#'   [adjust_weights_parametric()]. The number of rows and columns of
#'   this correlation matrix should match the length of `p`.
#' @param maxpts (Optional) An integer scalar for the maximum number of function
#'   values, which is needed to perform parametric tests using the
#'   `mvtnorm::GenzBretz` algorithm. The default is 25000.
#' @param abseps (Optional) A numeric scalar for the absolute error tolerance,
#'   which is needed to perform parametric tests using the `mvtnorm::GenzBretz`
#'   algorithm. The default is 1e-6.
#' @param releps (Optional) A numeric scalar for the relative error tolerance
#'   as double, which is needed to perform parametric tests using the
#'   `mvtnorm::GenzBretz` algorithm. The default is 0.
#'
#' @return
#' * `c_value_function()` returns the difference between
#'   \eqn{\alpha} and the Type I error of the parametric test with the \eqn{c}
#'   value of `x`, adjusted for the correlation between test statistics using
#'   parametric tests based on equation (6) of Xi et al. (2017).
#' * `solve_c_parametric()` returns the c value adjusted for the
#'   correlation between test statistics using parametric tests based on
#'   equation (6) of Xi et al. (2017).
#'
#' @seealso
#'   [adjust_weights_parametric()] for adjusted hypothesis weights using
#'   parametric tests.
#'
#' @rdname adjust_weights_parametric_util
#'
#' @keywords internal
#'
#' @references
#'   Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
#'   for weighted parametric multiple test procedures.
#'   \emph{Biometrical Journal}, 59(5), 918-931.
c_value_function <- function(x,
                             hypotheses,
                             test_corr,
                             alpha,
                             maxpts = 25000,
                             abseps = 1e-6,
                             releps = 0) {
  hyps_nonzero <- which(hypotheses > 0)
  z <- stats::qnorm(x * hypotheses[hyps_nonzero] * alpha, lower.tail = FALSE)
  y <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = test_corr[hyps_nonzero, hyps_nonzero, drop = FALSE],
      algorithm = mvtnorm::GenzBretz(
        maxpts = maxpts,
        abseps = abseps,
        releps = releps
      )
    )[[1]]
  )

  y - alpha * sum(hypotheses)
}

#' @rdname adjust_weights_parametric_util
#' @keywords internal
solve_c_parametric <- function(hypotheses,
                               test_corr,
                               alpha,
                               maxpts = 25000,
                               abseps = 1e-6,
                               releps = 0) {
  num_hyps <- seq_along(hypotheses)
  c_value <- ifelse(
    length(num_hyps) == 1 || sum(hypotheses) == 0,
    1,
    stats::uniroot(
      c_value_function,
      lower = 0, # Why is this not -Inf? Ohhhh because c_value >= 1
      # upper > 40 errors when w[i] ~= 1 && w[j] = epsilon
      # upper = 2 errors when w = c(.5, .5) && all(test_corr == 1)
      # furthermore, even under perfect correlation & with balanced weights, the
      # c_function_to_solve does not exceed `length(hypotheses)`
      upper = length(hypotheses) + 1,
      hypotheses = hypotheses,
      test_corr = test_corr,
      alpha = alpha,
      maxpts = maxpts,
      abseps = abseps,
      releps = releps
    )$root
  )

  # Occasionally has floating point differences
  round(c_value, 10)
}

#' @rdname adjust_weights_parametric_util
#' @keywords internal
#' This function only allows `test_corr` to be a single correlation matrix. This
#' is different from `adjust_weights_parametric()` which allows a list of
#' correlation matrices.
adjust_weights_parametric_util <- function(matrix_weights,
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
