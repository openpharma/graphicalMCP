#' Calculate adjusted p-values
#'
#' @description
#' For an intersection hypothesis, an adjusted p-value is the smallest
#' significance level at which the intersection hypothesis can be rejected.
#' The intersection hypothesis can be rejected if its adjusted p-value is less
#' than or equal to \eqn{\alpha}. Currently, there are three test types
#' supported:
#' * Bonferroni tests for [adjust_p_bonferroni()],
#' * Parametric tests for [adjust_p_parametric()],
#'     - Note that one-sided tests are required for parametric tests.
#' * Simes tests for [adjust_p_simes()],
#' * Hochberg tests for [adjust_p_hochberg()].
#'
#' @param p A numeric vector of p-values (unadjusted, raw), whose values should
#'   be between 0 & 1. The length should match the length of `hypotheses`.
#' @param hypotheses A numeric vector of hypothesis weights. Must be a vector of
#'   values between 0 & 1 (inclusive). The length should match the length of
#'   `p`. The sum of hypothesis weights should not exceed 1.
#' @param test_corr (Optional) A numeric matrix of correlations between test
#'   statistics, which is needed to perform parametric tests using
#'   [adjust_p_parametric()]. The number of rows and columns of
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
#' @return A single adjusted p-value for the intersection hypothesis.
#'
#' @seealso
#'   [adjust_weights_parametric()] for adjusted hypothesis weights using
#'   parametric tests, [adjust_weights_simes()] for adjusted hypothesis weights
#'   using Simes tests, [adjust_weights_hochberg()] for adjusted hypothesis
#'   weights using Hochberg tests.
#'
#' @rdname adjust_p
#'
#' @export
#'
#' @references
#'   Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
#'   approach to sequentially rejective multiple test procedures.
#'   \emph{Statistics in Medicine}, 28(4), 586-604.
#'
#'   Lu, K. (2016). Graphical approaches using a Bonferroni mixture of weighted
#'   Simes tests. \emph{Statistics in Medicine}, 35(22), 4041-4055.
#'
#'   Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
#'   for weighted parametric multiple test procedures.
#'   \emph{Biometrical Journal}, 59(5), 918-931.
#'
#'   Xi, D., and Bretz, F. (2019). Symmetric graphs for equally weighted tests,
#'   with application to the Hochberg procedure. \emph{Statistics in Medicine},
#'   38(27), 5268-5282.
#'
#' @examples
#' hypotheses <- c(H1 = 0.5, H2 = 0.25, H3 = 0.25)
#' p <- c(0.019, 0.025, 0.05)
#' adjust_p_bonferroni(p, hypotheses)
adjust_p_bonferroni <- function(p, hypotheses) {
  if (sum(hypotheses) == 0) {
    return(Inf)
  }

  # We need na.rm = TRUE to handle the 0 / 0 case. This may be too blunt a way
  # to handle it, but I suspect it's the fastest. Another option is to reduce p
  # and weights by keeping only indices where `!(p == 0 & weights == 0)`.
  # Considering that p-values are validated in the test function, this should be
  # safe
  round(min(p / hypotheses, na.rm = TRUE), 10)
}

#' @rdname adjust_p
#' @export
#' @examples
#' set.seed(1234)
#' hypotheses <- c(H1 = 0.5, H2 = 0.25, H3 = 0.25)
#' p <- c(0.019, 0.025, 0.05)
#' # Using the `mvtnorm::GenzBretz` algorithm
#' corr <- matrix(0.5, nrow = 3, ncol = 3)
#' diag(corr) <- 1
#' adjust_p_parametric(p, hypotheses, corr)
adjust_p_parametric <- function(p,
                                hypotheses,
                                test_corr = NULL,
                                maxpts = 25000,
                                abseps = 1e-6,
                                releps = 0) {
  if (sum(hypotheses) == 0) {
    return(Inf)
  }

  w_nonzero <- hypotheses > 0
  q <- min(p[w_nonzero] / hypotheses[w_nonzero])
  q <- q * hypotheses[w_nonzero]
  z <- stats::qnorm(q, lower.tail = FALSE)
  prob_less_than_z <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = test_corr[w_nonzero, w_nonzero, drop = FALSE],
      algorithm = mvtnorm::GenzBretz(
        maxpts = maxpts,
        abseps = abseps,
        releps = releps
      )
    )[[1]]
  )

  # Occasionally off by floating point differences, so round at some high detail
  # This level of detail should always remove floating point differences
  # appropriately, as they're typically 10^(<=-15). Conversely, this
  round(1 / sum(hypotheses) * prob_less_than_z, 10)
}

#' @rdname adjust_p
#' @export
#' @examples
#' hypotheses <- c(H1 = 0.5, H2 = 0.25, H3 = 0.25)
#' p <- c(0.019, 0.025, 0.05)
#' adjust_p_simes(p, hypotheses)
adjust_p_simes <- function(p, hypotheses) {
  if (sum(hypotheses) == 0) {
    return(Inf)
  }

  adjusted_p <- Inf
  for (i in seq_along(hypotheses)) {
    # This demonstrates a different and slightly more accurate way of
    # calculating Simes adjusted weights/adjusted p-values compared to the
    # method used in [adjust_weights_simes()]. In this function (and
    # [test_values_simes()]), we add all hypothesis weights for hypotheses with
    # a smaller p-value than hypothesis_j, for all j in J. In the case that two
    # p-values are identical, the corresponding hypotheses will get identical
    # adjusted weights/adjusted p-values. [adjust_weights_simes()], on the other
    # hand, uses an alternate method that's faster: First order hypotheses
    # according to their p-values in ascending order, then take the cumulative
    # sum. In the case that two p-values are identical, they will be sorted
    # sequentially, and the hypothesis that happens to come first will get a
    # smaller, incorrect adjusted weight (larger, incorrect adjusted p-value).
    # The hypothesis that comes second will be correct. [adjust_weights_simes()]
    # is only used in power calculations where it should not be possible to have
    # identical p-values, since they are sampled randomly (unless `all(test_corr
    # == 1)`). Furthermore, even when there are incorrect adjusted weights, it
    # cannot affect the hypothesis rejections. See Bonferroni function above for
    # na.rm reasoning
    adjusted_p <- min(
      adjusted_p,
      p[[i]] / sum(hypotheses[p <= p[[i]]]),
      na.rm = TRUE
    )
  }

  round(adjusted_p, 10)
}

#' @rdname adjust_p
#' @export
#' @examples
#' hypotheses <- c(H1 = 0.5, H2 = 0.25, H3 = 0.25)
#' p <- c(0.019, 0.025, 0.05)
#' adjust_p_hochberg(p, hypotheses)
adjust_p_hochberg <- function(p, hypotheses) {
  if (sum(hypotheses) == 0) {
    return(Inf)
  }

  adjusted_p <- Inf
  for (i in seq_along(hypotheses)) {
    # This demonstrates a different and slightly more accurate way of
    # calculating Hochberg adjusted weights/adjusted p-values compared to the
    # method used in [adjust_weights_hochberg()]. In this function (and
    # [test_values_hochberg()]), we count how many hypotheses with
    # a smaller p-value than hypothesis_j, for all j in J. In the case that two
    # p-values are identical, the corresponding hypotheses will get identical
    # adjusted weights/adjusted p-values. [adjust_weights_hochberg()], on the
    # other hand, uses an alternate method that's faster: First order hypotheses
    # according to their p-values in ascending order, then take the cumulative
    # sum. In the case that two p-values are identical, they will be sorted
    # sequentially, and the hypothesis that happens to come first will get a
    # smaller, incorrect adjusted weight (larger, incorrect adjusted p-value).
    # The hypothesis that comes second will be correct. [adjust_weights_hochberg()]
    # is only used in power calculations where it should not be possible to have
    # identical p-values, since they are sampled randomly (unless `all(test_corr
    # == 1)`). Furthermore, even when there are incorrect adjusted weights, it
    # cannot affect the hypothesis rejections. See Bonferroni function above for
    # na.rm reasoning.
    adjusted_p <- min(
      adjusted_p,
      p[[i]] / sum(hypotheses) * (length(hypotheses) - sum(p <= p[[i]]) + 1),
      na.rm = TRUE
    )
  }

  round(adjusted_p, 10)
}
