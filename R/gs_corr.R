#' Compute the correlation matrix for group sequential test statistics
#'
#' @description
#' In a group sequential design, test statistics \eqn{Z_1, \ldots, Z_K} at
#' analyses with information fractions \eqn{t_1, \ldots, t_K} follow the
#' canonical joint distribution with correlation
#' \deqn{\text{Cor}(Z_i, Z_j) = \sqrt{t_i / t_j}, \quad i \le j.}
#'
#' This correlation structure arises from the independent increments property
#' of the score process. It depends only on the information fractions, not on
#' the specific test or endpoint.
#'
#' @param info_frac A numeric vector of information fractions at each analysis.
#'   Must be positive and monotonically non-decreasing.
#'
#' @return A symmetric correlation matrix of dimension \eqn{K \times K}, where
#'   \eqn{K} is the length of `info_frac`. The diagonal entries are all 1.
#'
#' @seealso [gs_boundaries()] which uses this correlation matrix for computing
#'   group sequential boundaries.
#'
#' @rdname gs_corr
#'
#' @export
#'
#' @examples
#' # Three equally spaced analyses
#' gs_corr(c(1 / 3, 2 / 3, 1))
#'
#' # Two analyses at 50% and 100%
#' gs_corr(c(0.5, 1))
gs_corr <- function(info_frac) {
  outer(
    info_frac, info_frac,
    function(ti, tj) sqrt(pmin(ti, tj) / pmax(ti, tj))
  )
}
