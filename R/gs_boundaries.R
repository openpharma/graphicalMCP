#' Compute group sequential boundaries from an alpha spending function
#'
#' @description
#' Given a significance level, information fractions, and a spending function,
#' compute the group sequential boundaries at each analysis. The boundaries are
#' computed on the Z-scale using the recursive relationship between cumulative
#' spending and the joint distribution of test statistics. The null hypothesis
#' is rejected at analysis \eqn{k} if the test statistic \eqn{Z_k \ge b_k}.
#'
#' At analysis \eqn{k}, the Z-scale boundary \eqn{b_k} satisfies
#' \deqn{P(Z_1 < b_1, \ldots, Z_k < b_k) = 1 - f(\alpha, t_k),}
#' where \eqn{f(\alpha, t_k)} is the cumulative spending at information
#' fraction \eqn{t_k}, and \eqn{(Z_1, \ldots, Z_k)} follows the canonical
#' joint distribution with mean zero and correlations given by [gs_corr()].
#'
#' @param alpha A numeric scalar of the significance level to be spent across
#'   analyses.
#' @param info_frac A numeric vector of information fractions at each analysis.
#'   Must be monotonically non-decreasing with values in (0, 1].
#' @param spending_fn A spending function that takes two arguments: `alpha`
#'   (significance level) and `info_frac` (information fraction), and returns
#'   the cumulative alpha spent. See [spending_of()].
#' @param maxpts An integer scalar for the maximum number of function values
#'   for `mvtnorm::GenzBretz`. The default is 25000.
#' @param abseps A numeric scalar for the absolute error tolerance for
#'   `mvtnorm::GenzBretz`. The default is 1e-6.
#'
#' @return A list with elements:
#'   * `bounds_z` - A numeric vector of Z-scale boundaries at each analysis.
#'   * `bounds_nominal` - A numeric vector of nominal p-value boundaries at
#'     each analysis, i.e., \eqn{c_k = 1 - \Phi(b_k)}.
#'
#' @seealso
#'   [spending_of()], [spending_pocock()], [spending_hsd()], [spending_linear()]
#'   for spending functions, [sequential_p()] for sequential p-values,
#'   [graph_test_shortcut_gsd()] for graphical MCPs with group sequential
#'   designs, [gs_corr()] for the correlation matrix.
#'
#' @rdname gs_boundaries
#'
#' @export
gs_boundaries <- function(alpha,
                          info_frac,
                          spending_fn,
                          maxpts = 25000,
                          abseps = 1e-6) {
  K <- length(info_frac)
  bounds_z <- numeric(K)

  # Compute cumulative spending at all analyses up front
  cum_spent <- spending_fn(alpha, info_frac)

  for (k in seq_len(K)) {
    # Treat cumulative spending below machine precision as zero
    if (cum_spent[k] < .Machine$double.eps) {
      bounds_z[k] <- Inf
      next
    }
    if (cum_spent[k] >= 1 - .Machine$double.eps) {
      bounds_z[k] <- -Inf
      next
    }

    target_prob <- 1 - cum_spent[k]

    if (k == 1) {
      # First analysis: simple univariate case
      bounds_z[k] <- stats::qnorm(target_prob)
      next
    }

    # All boundaries computed so far
    all_bounds <- bounds_z[seq_len(k - 1)]

    # If all previous bounds are Inf, the multivariate constraint reduces
    # to a univariate problem: P(Z_k < b_k) = target_prob
    if (all(is.infinite(all_bounds) & all_bounds > 0)) {
      bounds_z[k] <- stats::qnorm(target_prob)
      next
    }

    # For the multivariate case, replace any Inf bounds with a large finite
    # value to avoid numerical issues in pmvnorm (following gsDesign convention)
    finite_bounds <- pmin(all_bounds, 20)

    # Correlation matrix for analyses 1, ..., k
    corr_k <- gs_corr(info_frac[seq_len(k)])

    # Find b_k such that P(Z_1 < b_1, ..., Z_k < b_k) = target_prob
    f_root <- function(bk) {
      upper_vec <- c(finite_bounds, bk)

      prob <- mvtnorm::pmvnorm(
        upper = upper_vec,
        corr = corr_k,
        algorithm = mvtnorm::GenzBretz(
          maxpts = maxpts,
          abseps = abseps
        )
      )[[1]]

      prob - target_prob
    }

    # Determine a reasonable search interval
    # Start from the univariate approximation and expand
    b_approx <- stats::qnorm(target_prob)
    search_lower <- min(b_approx - 6, -8)
    search_upper <- max(b_approx + 6, 8)

    # Verify that the root is bracketed; widen if necessary
    f_lower <- f_root(search_lower)
    f_upper <- f_root(search_upper)

    if (f_lower > 0) {
      search_lower <- search_lower - 5
      f_lower <- f_root(search_lower)
    }
    if (f_upper < 0) {
      search_upper <- search_upper + 5
      f_upper <- f_root(search_upper)
    }

    if (f_lower * f_upper > 0) {
      # Fallback: if still not bracketed, use the univariate approximation
      bounds_z[k] <- b_approx
      next
    }

    result <- stats::uniroot(
      f_root,
      interval = c(search_lower, search_upper),
      tol = abseps
    )
    bounds_z[k] <- result$root
  }

  list(
    bounds_z = bounds_z,
    bounds_nominal = stats::pnorm(bounds_z, lower.tail = FALSE)
  )
}
