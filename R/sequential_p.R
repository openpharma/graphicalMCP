#' Calculate the sequential p-value for a single hypothesis
#'
#' @description
#' A sequential p-value is the minimum significance level at which a group
#' sequential boundary would be crossed at any analysis up to and including the
#' current one. It is computed using the group sequential boundaries derived
#' from the spending function and the joint distribution of test statistics
#' across analyses.
#'
#' Sequential p-values are used in graphical multiple comparison procedures
#' for group sequential designs. They allow the separation of the group
#' sequential testing (handled by the spending function and boundaries) from
#' the multiplicity adjustment (handled by the graph). See Maurer and Bretz
#' (2013) for details.
#'
#' @param p A numeric vector of p-values at each analysis for a single
#'   hypothesis. The length must match the length of `info_frac`. All values
#'   must be non-missing and between 0 and 1.
#' @param info_frac A numeric vector of information fractions at each analysis.
#'   Values must be in (0, 1] and monotonically non-decreasing. The length
#'   should match the length of `p`.
#' @param spending_fn A spending function. Must accept two arguments:
#'   `alpha` (total significance level) and `info_frac` (information fraction), and
#'   return the cumulative alpha spent at information fraction `t`. Built-in
#'   options include [spending_of()], [spending_pocock()], [spending_hsd()],
#'   and [spending_linear()].
#' @param tol A numeric scalar for the tolerance of the root-finding
#'   algorithm. The default is `1e-6`.
#' @param maxpts An integer scalar for the maximum number of function values
#'   for `mvtnorm::GenzBretz`. The default is 25000.
#' @param abseps A numeric scalar for the absolute error tolerance for
#'   `mvtnorm::GenzBretz`. The default is 1e-6.
#'
#' @return A numeric scalar of the sequential p-value.
#'
#' @details
#' For a hypothesis tested at analyses \eqn{k = 1, \ldots, K} with p-values
#' \eqn{p^{(k)}} and information fractions \eqn{t^{(k)}}, the sequential
#' p-value is the minimum \eqn{\tilde{p}} such that for some analysis
#' \eqn{k}, the observed p-value \eqn{p^{(k)}} crosses the group sequential
#' boundary \eqn{c_k(\tilde{p})} derived from the spending function:
#' \deqn{\tilde{p} = \min\{\alpha : p^{(k)} \le c_k(\alpha)
#' \text{ for some } k\},}
#' where \eqn{c_k(\alpha)} is the nominal p-value boundary at analysis
#' \eqn{k} when the total significance level is \eqn{\alpha}.
#'
#' The boundary \eqn{c_k(\alpha)} is computed from the spending function
#' \eqn{f(\alpha, t)} using the joint distribution of test statistics.
#' Specifically, the Z-scale boundary \eqn{b_k} satisfies
#' \deqn{P(Z_1 < b_1, \ldots, Z_k < b_k) = 1 - f(\alpha, t_k),}
#' and \eqn{c_k = 1 - \Phi(b_k)}. Note that \eqn{c_k \neq
#' f(\alpha, t_k) - f(\alpha, t_{k-1})} for \eqn{k > 1} due to the
#' correlation between test statistics across analyses.
#'
#' The sequential p-value is found using [stats::uniroot()] on the function
#' \eqn{g(\alpha) = \max_k (z_k - b_k(\alpha))}, where \eqn{z_k =
#' \Phi^{-1}(1 - p^{(k)})} is the observed Z-statistic and \eqn{b_k(\alpha)}
#' is the Z-scale boundary. This function is monotonically increasing in
#' \eqn{\alpha} since boundaries become less stringent as \eqn{\alpha}
#' increases.
#'
#' @seealso
#'   [gs_boundaries()] for computing group sequential boundaries,
#'   [graph_test_shortcut_gsd()] for graphical multiple comparison procedures
#'   with group sequential designs, [spending_of()], [spending_pocock()],
#'   [spending_hsd()], [spending_linear()] for spending functions.
#'
#' @rdname sequential_p
#'
#' @export
#'
#' @references
#'   Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
#'   trials using graphical approaches. \emph{Statistics in Biopharmaceutical
#'   Research}, 5(4), 311-320.
#'
#'   Liu, Q., and Anderson, K. M. (2008). On adaptive extensions of group
#'   sequential trials for clinical investigations. \emph{Journal of the
#'   American Statistical Association}, 103(484), 1621-1630.
#'
#' @examples
#' # A hypothesis tested at two analyses (interim at 50% and final at 100%)
#' sequential_p(
#'   p = c(0.024, 0.01),
#'   info_frac = c(0.5, 1),
#'   spending_fn = spending_of
#' )
#'
#' # Sequential p-value with Pocock spending
#' sequential_p(
#'   p = c(0.024, 0.01),
#'   info_frac = c(0.5, 1),
#'   spending_fn = spending_pocock
#' )
#'
#' # Sequential p-value updates as more analyses are conducted
#' # After analysis 1 only
#' sequential_p(
#'   p = 0.05,
#'   info_frac = 0.3,
#'   spending_fn = spending_of
#' )
#'
#' # After analyses 1 and 2
#' sequential_p(
#'   p = c(0.05, 0.02),
#'   info_frac = c(0.3, 0.7),
#'   spending_fn = spending_of
#' )
#'
#' # After all three analyses
#' sequential_p(
#'   p = c(0.05, 0.02, 0.01),
#'   info_frac = c(0.3, 0.7, 1),
#'   spending_fn = spending_of
#' )
sequential_p <- function(p,
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

  # Convert observed p-values to Z-statistics
  z_obs <- stats::qnorm(1 - p)

  # For a candidate alpha, compute boundaries and return the maximum
  # exceedance: max_k(z_obs[k] - b_k(alpha)).
  # Positive means at least one boundary is crossed.
  # This function is monotonically increasing in alpha since boundaries
  # decrease (become less stringent) as alpha increases.
  max_exceedance <- function(alpha_candidate) {
    bounds <- gs_boundaries(
      alpha = alpha_candidate,
      info_frac = info_frac,
      spending_fn = spending_fn,
      maxpts = maxpts,
      abseps = abseps
    )

    max(z_obs - bounds$bounds_z)
  }

  # Check edge cases before root-finding.
  # The search interval is [lower, upper] = [tol, 1 - tol] to avoid
  # numerical issues at the exact boundaries 0 and 1.
  upper <- 1 - tol
  lower <- tol

  # If no boundary is crossed even at the most lenient alpha (~1),
  # the observed p-values are too large to ever be significant.
  # Return 1 (the largest possible sequential p-value).
  exc_upper <- max_exceedance(upper)
  if (exc_upper <= 0) {
    message("No boundary crossed; returning 1 as an upper bound.")
    return(1)
  }

  # If a boundary is already crossed at the most stringent alpha (~0),
  # the observed p-values are extremely small. Return the lower bound.
  # Use >= 0 to also handle the edge case where the exceedance is exactly 0
  # (boundary exactly crossed), which would cause uniroot to fail because
  # f(lower) and f(upper) would have the same sign.
  exc_lower <- max_exceedance(lower)
  if (exc_lower >= 0) {
    message(
      "Boundary crossed at alpha = ", lower,
      "; returning ", lower, " as a lower bound."
    )
    return(lower)
  }

  # Find the root: minimum alpha where boundary is crossed
  result <- stats::uniroot(
    max_exceedance,
    interval = c(lower, upper),
    tol = tol
  )

  result$root
}
