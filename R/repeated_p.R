#' Calculate the repeated p-value for a single hypothesis at a given analysis
#'
#' @description
#' A repeated p-value at analysis \eqn{k} is the minimum significance level at
#' which the group sequential boundary at analysis \eqn{k} would be crossed.
#' Unlike the sequential p-value, which considers all analyses up to \eqn{k},
#' the repeated p-value only considers the boundary at analysis \eqn{k} itself.
#'
#' The sequential p-value equals the minimum of repeated p-values across
#' analyses: \eqn{\tilde{p}_k = \min_{l=1}^{k} \hat{p}_l}, where
#' \eqn{\hat{p}_l} is the repeated p-value at analysis \eqn{l}.
#'
#' @param p A numeric vector of p-values at each analysis for a single
#'   hypothesis. The length must match the length of `info_frac`. All values
#'   must be non-missing and between 0 and 1.
#' @param info_frac A numeric vector of information fractions at each analysis.
#'   Values must be in (0, 1] and monotonically non-decreasing. The length
#'   should match the length of `p`.
#' @param spending_fn A spending function. Must accept two arguments:
#'   `alpha` (total significance level) and `info_frac` (information fraction),
#'   and return the cumulative alpha spent. Built-in options include
#'   [spending_of()], [spending_pocock()], [spending_hsd()], and
#'   [spending_linear()].
#' @param tol A numeric scalar for the tolerance of the root-finding
#'   algorithm. The default is `1e-6`.
#' @param maxpts An integer scalar for the maximum number of function values
#'   for `mvtnorm::GenzBretz`. The default is 25000.
#' @param abseps A numeric scalar for the absolute error tolerance for
#'   `mvtnorm::GenzBretz`. The default is 1e-6.
#'
#' @return A numeric scalar of the repeated p-value at the last analysis in
#'   the input vectors.
#'
#' @details
#' For a hypothesis tested at analyses \eqn{k = 1, \ldots, K} with p-values
#' \eqn{p^{(k)}} and information fractions \eqn{t^{(k)}}, the repeated
#' p-value at analysis \eqn{K} is the minimum \eqn{\hat{p}} such that the
#' observed p-value \eqn{p^{(K)}} crosses the group sequential boundary
#' \eqn{c_K(\hat{p})} at that analysis:
#' \deqn{\hat{p}_K = \min\{\alpha : p^{(K)} \le c_K(\alpha)\}.}
#'
#' Note that computing the boundary \eqn{c_K(\alpha)} requires knowledge of
#' all previous information fractions \eqn{t^{(1)}, \ldots, t^{(K)}} because
#' the boundary at analysis \eqn{K} depends on the cumulative spending and
#' the joint distribution of test statistics.
#'
#' The repeated p-value is found using [stats::uniroot()] on the function
#' \eqn{g(\alpha) = z_K - b_K(\alpha)}, where \eqn{z_K =
#' \Phi^{-1}(1 - p^{(K)})} is the observed Z-statistic at analysis \eqn{K}
#' and \eqn{b_K(\alpha)} is the Z-scale boundary.
#'
#' @seealso
#'   [sequential_p()] for the sequential p-value (minimum repeated p-value),
#'   [gs_boundaries()] for computing group sequential boundaries,
#'   [graph_test_shortcut_gsd()] for graphical multiple comparison procedures
#'   with group sequential designs.
#'
#' @rdname repeated_p
#'
#' @export
#'
#' @references
#'   Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
#'   trials using graphical approaches. \emph{Statistics in Biopharmaceutical
#'   Research}, 5(4), 311-320.
#'
#' @examples
#' # Repeated p-value at the second analysis (interim at 50%, final at 100%)
#' repeated_p(
#'   p = c(0.024, 0.01),
#'   info_frac = c(0.5, 1),
#'   spending_fn = spending_of
#' )
#'
#' # Compare with sequential p-value (which is the minimum repeated p-value)
#' sequential_p(
#'   p = c(0.024, 0.01),
#'   info_frac = c(0.5, 1),
#'   spending_fn = spending_of
#' )
#'
#' # Repeated p-values at each analysis
#' # Analysis 1
#' repeated_p(
#'   p = 0.05,
#'   info_frac = 0.3,
#'   spending_fn = spending_of
#' )
#'
#' # Analysis 2
#' repeated_p(
#'   p = c(0.05, 0.02),
#'   info_frac = c(0.3, 0.7),
#'   spending_fn = spending_of
#' )
repeated_p <- function(p,
                       info_frac,
                       spending_fn,
                       tol = 1e-6,
                       maxpts = 25000,
                       abseps = 1e-6) {
  stopifnot(
    "p must be a numeric vector" = is.numeric(p),
    "p must not contain NA" = !anyNA(p),
    "p must be between 0 and 1" = all(p >= 0 & p <= 1),
    "info_frac must be a numeric vector" = is.numeric(info_frac),
    "info_frac must not contain NA" = !anyNA(info_frac),
    "p and info_frac must have the same length" = length(info_frac) == length(p)
  )

  K <- length(p)

  # Convert observed p-value at the current analysis to Z-statistic
  z_obs_K <- stats::qnorm(1 - p[K])

  # For a candidate alpha, compute boundaries and return the exceedance
  # at analysis K only: z_obs[K] - b_K(alpha).
  # Positive means the boundary at analysis K is crossed.
  exceedance_K <- function(alpha_candidate) {
    bounds <- gs_boundaries(
      alpha = alpha_candidate,
      info_frac = info_frac,
      spending_fn = spending_fn,
      maxpts = maxpts,
      abseps = abseps
    )

    z_obs_K - bounds$bounds_z[K]
  }

  # Check edge cases before root-finding.
  upper <- 1 - tol
  lower <- tol

  # If boundary at analysis K is not crossed even at alpha ~ 1,
  # the p-value at this analysis is too large. Return 1.
  exc_upper <- exceedance_K(upper)
  if (exc_upper <= 0) {
    message("Boundary at analysis K not crossed; returning 1 as an upper bound.")
    return(1)
  }

  # If boundary at analysis K is crossed even at alpha ~ 0,
  # the p-value is extremely small. Return the lower bound.
  # Use >= 0 to also handle the edge case where the exceedance is exactly 0
  # (boundary exactly crossed), which would cause uniroot to fail because
  # f(lower) and f(upper) would have the same sign.
  exc_lower <- exceedance_K(lower)
  if (exc_lower >= 0) {
    message(
      "Boundary crossed at alpha = ", lower,
      "; returning ", lower, " as a lower bound."
    )
    return(lower)
  }

  # Find the root: minimum alpha where boundary at analysis K is crossed
  result <- stats::uniroot(
    exceedance_K,
    interval = c(lower, upper),
    tol = tol
  )

  result$root
}
