#' Alpha spending functions for group sequential designs
#'
#' @description
#' Alpha spending functions determine how the total significance level (alpha)
#' is allocated across interim and final analyses in a group sequential design.
#' Given the total alpha and the information fraction(s) at one or more
#' analyses, a spending function returns the cumulative alpha spent at each
#' information fraction.
#'
#' Four commonly used spending functions are provided:
#' * [spending_of()] for the Lan-DeMets O'Brien-Fleming approximation,
#' * [spending_pocock()] for the Lan-DeMets Pocock approximation,
#' * [spending_hsd()] for the Hwang-Shih-DeCani family,
#' * [spending_linear()] for linear (uniform) spending.
#'
#' @param alpha A numeric scalar of the total significance level to be spent.
#'   Must be between 0 and 1.
#' @param info_frac A numeric scalar or vector of information fractions. Values
#'   must be in \[0, 1\]. When `info_frac = 0`, the spending is 0. When
#'   `info_frac = 1`, the spending equals `alpha`.
#' @param gamma A numeric scalar for the gamma parameter of the
#'   Hwang-Shih-DeCani spending function. Common choices are `gamma = -4`
#'   (approximates O'Brien-Fleming), `gamma = 1` (approximates Pocock), and
#'   `gamma = 0` (linear spending). The default is `gamma = -4`.
#'
#' @return A numeric vector the same length as `info_frac` of cumulative alpha
#'   spent at each information fraction.
#'
#' @details
#' All spending functions satisfy the following properties:
#' * \eqn{f(\alpha, 0) = 0},
#' * \eqn{f(\alpha, 1) = \alpha},
#' * \eqn{f(\alpha, t)} is non-decreasing in \eqn{t}.
#'
#' The cumulative alpha spent at analysis \eqn{k} is \eqn{f(\alpha, t_k)},
#' and the incremental spending is
#' \deqn{\Delta\alpha_k = f(\alpha, t_k) - f(\alpha, t_{k-1}).}
#'
#' Note that the incremental spending is \emph{not} the nominal significance
#' level (boundary) at analysis \eqn{k}. The boundary must be derived from the
#' spending using the joint distribution of test statistics across analyses.
#' See [sequential_p()] and [graph_test_shortcut_gsd()] for details.
#'
#' @section Spending function formulas:
#' * **O'Brien-Fleming** (`spending_of`):
#'   \deqn{f(\alpha, t) = 2\left(1 - \Phi\left(\frac{\Phi^{-1}(1 - \alpha/2)}
#'   {\sqrt{t}}\right)\right).}
#'   This is the Lan-DeMets approximation to O'Brien-Fleming boundaries.
#'   It is very conservative at early analyses and spends most of the alpha
#'   at the final analysis.
#'
#' * **Pocock** (`spending_pocock`):
#'   \deqn{f(\alpha, t) = \alpha \cdot \ln(1 + (e - 1) \cdot t).}
#'   This spends alpha more evenly across analyses compared to O'Brien-Fleming.
#'
#' * **Hwang-Shih-DeCani** (`spending_hsd`):
#'   \deqn{f(\alpha, t) = \alpha \cdot \frac{1 - e^{-\gamma t}}{1 -
#'   e^{-\gamma}}, \quad \gamma \neq 0,}
#'   \deqn{f(\alpha, t) = \alpha \cdot t, \quad \gamma = 0.}
#'   With `gamma = -4`, it approximates O'Brien-Fleming; with `gamma = 1`,
#'   it approximates Pocock.
#'
#' * **Linear** (`spending_linear`):
#'   \deqn{f(\alpha, t) = \alpha \cdot t.}
#'
#' @references
#'   Lan, K. K. G., and DeMets, D. L. (1983). Discrete sequential boundaries
#'   for clinical trials. \emph{Biometrika}, 70(3), 659-663.
#'
#'   Hwang, I. K., Shih, W. J., and De Cani, J. S. (1990). Group sequential
#'   designs using a family of type I error probability spending functions.
#'   \emph{Statistics in Medicine}, 9(12), 1439-1445.
#'
#' @rdname spending_functions
#'
#' @export
#'
#' @examples
#' # O'Brien-Fleming spending at 50% information
#' spending_of(0.025, 0.5)
#'
#' # Cumulative spending across analyses (vectorized)
#' spending_of(0.025, c(0, 0.5, 1))
#'
#' # Compare spending functions at information fractions (1/3, 2/3, 1)
#' spending_of(0.025, c(1/3, 2/3, 1))
#' spending_pocock(0.025, c(1/3, 2/3, 1))
#' spending_hsd(0.025, c(1/3, 2/3, 1), gamma = -4)
#' spending_linear(0.025, c(1/3, 2/3, 1))
spending_of <- function(alpha, info_frac) {
  stopifnot(
    "info_frac must be in [0, 1]" = all(info_frac >= 0 & info_frac <= 1)
  )
  result <- 2 * (1 - stats::pnorm(stats::qnorm(1 - alpha / 2) / sqrt(info_frac)))
  result[info_frac == 0] <- 0
  result
}

#' @rdname spending_functions
#' @export
#' @examples
#' # Pocock spending at 50% information
#' spending_pocock(0.025, 0.5)
spending_pocock <- function(alpha, info_frac) {
  stopifnot(
    "info_frac must be in [0, 1]" = all(info_frac >= 0 & info_frac <= 1)
  )
  result <- alpha * log(1 + (exp(1) - 1) * info_frac)
  result[info_frac == 0] <- 0
  result
}

#' @rdname spending_functions
#' @export
#' @examples
#' # Hwang-Shih-DeCani spending at 50% information
#' spending_hsd(0.025, 0.5, gamma = -4)
#' spending_hsd(0.025, 0.5, gamma = 1)
#' spending_hsd(0.025, 0.5, gamma = 0)
spending_hsd <- function(alpha, info_frac, gamma = -4) {
  stopifnot(
    "info_frac must be in [0, 1]" = all(info_frac >= 0 & info_frac <= 1)
  )
  if (gamma == 0) {
    result <- alpha * info_frac
  } else {
    result <- alpha * (1 - exp(-gamma * info_frac)) / (1 - exp(-gamma))
  }
  result[info_frac == 0] <- 0
  result
}

#' @rdname spending_functions
#' @export
#' @examples
#' # Linear spending at 50% information
#' spending_linear(0.025, 0.5)
spending_linear <- function(alpha, info_frac) {
  stopifnot(
    "info_frac must be in [0, 1]" = all(info_frac >= 0 & info_frac <= 1)
  )
  alpha * info_frac
}
