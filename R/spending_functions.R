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
#'   must be non-negative. When `info_frac = 0`, the spending is 0. When
#'   `info_frac >= 1`, the spending is capped at `alpha`.
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
    "info_frac must be non-negative" = all(info_frac >= 0),
    "At most one info_frac value can be >= 1" = sum(info_frac >= 1) <= 1
  )
  result <- 2 * (1 - stats::pnorm(stats::qnorm(1 - alpha / 2) / sqrt(info_frac)))
  result[info_frac == 0] <- 0
  result <- pmin(result, alpha)
  result
}

#' @rdname spending_functions
#' @export
#' @examples
#' # Pocock spending at 50% information
#' spending_pocock(0.025, 0.5)
spending_pocock <- function(alpha, info_frac) {
  stopifnot(
    "info_frac must be non-negative" = all(info_frac >= 0),
    "At most one info_frac value can be >= 1" = sum(info_frac >= 1) <= 1
  )
  result <- alpha * log(1 + (exp(1) - 1) * info_frac)
  result[info_frac == 0] <- 0
  result <- pmin(result, alpha)
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
    "info_frac must be non-negative" = all(info_frac >= 0),
    "At most one info_frac value can be >= 1" = sum(info_frac >= 1) <= 1
  )
  if (gamma == 0) {
    result <- alpha * info_frac
  } else {
    result <- alpha * (1 - exp(-gamma * info_frac)) / (1 - exp(-gamma))
  }
  result[info_frac == 0] <- 0
  result <- pmin(result, alpha)
  result
}

#' @rdname spending_functions
#' @export
#' @examples
#' # Linear spending at 50% information
#' spending_linear(0.025, 0.5)
spending_linear <- function(alpha, info_frac) {
  stopifnot(
    "info_frac must be non-negative" = all(info_frac >= 0),
    "At most one info_frac value can be >= 1" = sum(info_frac >= 1) <= 1
  )
  pmin(alpha * info_frac, alpha)
}


#' Create a spending function with a custom spending time
#'
#' @description
#' Wraps an existing spending function to use a fixed **spending time** instead
#' of the information fractions passed to it at runtime. This separates the
#' alpha allocation schedule (determined by spending time) from the correlation
#' structure (determined by information fractions in
#' [graph_test_shortcut_gsd()]).
#'
#' This is useful in two common scenarios:
#' * **Subgroup analyses**: all-subjects hypotheses use all-subjects event
#'   counts for the correlation structure but subgroup event counts for
#'   spending (see the spending time section of
#'   `vignette("group-sequential-testing")`).
#' * **Monitoring with changed final information**: when the actual total
#'   information at the final analysis differs from the planned total, the
#'   planned information fractions are used as spending time to preserve
#'   boundaries at earlier analyses, while the actual information fractions
#'   are used for the correlation structure (see the monitoring section of
#'   `vignette("group-sequential-testing")`).
#'
#' @param spending_fn A spending function to wrap. Must accept two arguments:
#'   `alpha` (significance level) and `info_frac` (information fraction), and
#'   return the cumulative alpha spent.
#' @param spending_time A numeric vector of spending time values. These replace
#'   the `info_frac` argument when the wrapped function is called. The vector
#'   is truncated to match the length of `info_frac` at runtime, which handles
#'   interim analyses where fewer analyses have been conducted.
#'
#' @return A function with the same signature as `spending_fn` —
#'   `function(alpha, info_frac)` — that internally uses `spending_time`
#'   instead of `info_frac` for alpha allocation.
#'
#' @seealso [spending_of()], [spending_pocock()], [spending_hsd()],
#'   [spending_linear()] for built-in spending functions,
#'   [graph_test_shortcut_gsd()] for the graphical procedure with group
#'   sequential designs.
#'
#' @export
#'
#' @examples
#' # Subgroup spending time: use subgroup event fractions for spending
#' # while info_frac uses all-subjects event fractions for correlation
#' spending_h2 <- spending_with_time(
#'   spending_of,
#'   spending_time = c(185 / 295, 245 / 295, 1)
#' )
#'
#' # The wrapped function has the standard (alpha, info_frac) signature
#' # but ignores info_frac and uses spending_time internally
#' spending_h2(0.01, c(0.5, 0.8, 1))
#'
#' # Monitoring: use planned info fractions for spending
#' # when actual final information differs from planned
#' spending_monitor <- spending_with_time(
#'   spending_of,
#'   spending_time = c(0.627, 0.831, 1)  # planned
#' )
#' # Call with actual info fractions (for correlation structure)
#' spending_monitor(0.01, c(0.597, 0.790, 1))  # actual
spending_with_time <- function(spending_fn, spending_time) {
  stopifnot(
    "spending_fn must be a function" = is.function(spending_fn),
    "spending_time must be a numeric vector" = is.numeric(spending_time),
    "spending_time must be non-negative" =
      all(spending_time >= 0),
    "At most one spending_time value can be >= 1" =
      sum(spending_time >= 1) <= 1
  )

  function(alpha, info_frac) {
    st <- spending_time[seq_along(info_frac)]
    spending_fn(alpha, st)
  }
}


#' Wang-Tsiatis spending function
#'
#' @description
#' Computes the implied cumulative alpha spending from the Wang-Tsiatis family
#' of group sequential boundaries. The Wang-Tsiatis boundaries at analysis
#' \eqn{k} with information fraction \eqn{t_k} are defined as:
#' \deqn{c_k = C \cdot t_k^{\Delta - 0.5},}
#' where \eqn{\Delta} is the shape parameter and \eqn{C} is a constant
#' calibrated so that the overall Type I error equals \eqn{\alpha}.
#'
#' Special cases:
#' * \eqn{\Delta = 0.5}: Pocock boundaries (equal Z-scale boundaries across
#'   analyses).
#' * \eqn{\Delta = 0}: O'Brien-Fleming boundaries (very conservative at
#'   early analyses).
#' * \eqn{0 < \Delta < 0.5}: intermediate between O'Brien-Fleming and Pocock.
#'
#' Unlike the Lan-DeMets approximations ([spending_of()], [spending_pocock()]),
#' this function computes the **exact** boundaries from the Wang-Tsiatis
#' family and derives the implied spending. It is computationally more
#' expensive because it requires root-finding and multivariate normal
#' integration at each call.
#'
#' @param alpha A numeric scalar of the total significance level.
#' @param info_frac A numeric vector of information fractions at each analysis.
#'   Must be non-negative, with at most one value \eqn{\geq 1}.
#' @param delta A numeric scalar for the shape parameter \eqn{\Delta}.
#'   The default is `0.5` (Pocock). Use `0` for O'Brien-Fleming.
#' @param maxpts An integer scalar for the maximum number of function values
#'   for [mvtnorm::GenzBretz()]. The default is 25000.
#' @param abseps A numeric scalar for the absolute error tolerance for
#'   [mvtnorm::GenzBretz()]. The default is 1e-6.
#'
#' @return A numeric vector the same length as `info_frac` of cumulative alpha
#'   spent at each information fraction.
#'
#' @seealso [spending_of()] and [spending_pocock()] for the Lan-DeMets
#'   approximations, [gs_boundaries()] for computing boundaries from spending
#'   functions, [graph_test_shortcut_gsd()] for the graphical procedure.
#'
#' @references
#'   Wang, S. K., and Tsiatis, A. A. (1987). Approximately optimal one-parameter
#'   boundaries for group sequential trials. \emph{Biometrics}, 43(1), 193-199.
#'
#' @export
#'
#' @examples
#' # Exact O'Brien-Fleming (delta = 0)
#' spending_wt(0.025, c(0.5, 1), delta = 0)
#'
#' # Exact Pocock (delta = 0.5)
#' spending_wt(0.025, c(0.5, 1), delta = 0.5)
#'
#' # Intermediate (delta = 0.25)
#' spending_wt(0.025, c(1/3, 2/3, 1), delta = 0.25)
#'
#' # Compare with Lan-DeMets approximations
#' spending_of(0.025, c(1/3, 2/3, 1))     # Lan-DeMets OBF approximation
#' spending_wt(0.025, c(1/3, 2/3, 1), 0)  # Exact OBF
#'
#' # Use in graph_test_shortcut_gsd (wrap to fix delta)
#' \donttest{
#' g <- graph_create(c(0.5, 0.5), rbind(c(0, 1), c(1, 0)))
#' p <- rbind(H1 = c(0.024, 0.01), H2 = c(0.015, 0.005))
#' graph_test_shortcut_gsd(
#'   graph = g, p = p, alpha = 0.025,
#'   info_frac = c(0.5, 1),
#'   spending_fn = function(a, t) spending_wt(a, t, delta = 0.25)
#' )
#' }
spending_wt <- function(alpha, info_frac, delta = 0.5,
                        maxpts = 25000, abseps = 1e-6) {
  stopifnot(
    "info_frac must be non-negative" = all(info_frac >= 0),
    "At most one info_frac value can be >= 1" = sum(info_frac >= 1) <= 1,
    "delta must be a numeric scalar" = is.numeric(delta) && length(delta) == 1
  )

  K <- length(info_frac)

  # Handle edge cases
  if (alpha <= 0) return(rep(0, K))
  if (K == 1) return(pmin(alpha, alpha))

  # Correlation matrix
  corr <- gs_corr(info_frac)

  # Wang-Tsiatis boundary shape: c_k = C * t_k^(delta - 0.5)
  # For info_frac = 0, the shape is Inf (or 0 depending on delta),
  # handle by setting those boundaries to Inf (never cross)
  shape <- ifelse(info_frac == 0, 0, info_frac^(delta - 0.5))

  algo <- mvtnorm::GenzBretz(maxpts = maxpts, abseps = abseps)

  # Find C such that P(cross at some k | H0) = alpha
  # P(cross) = 1 - P(Z_1 < c_1, ..., Z_K < c_K)
  find_C <- function(C_val) {
    bounds_z <- C_val * shape
    # Replace any Inf or very large bounds with 20 for numerical stability
    bounds_z <- pmin(bounds_z, 20)

    prob_no_cross <- mvtnorm::pmvnorm(
      upper = bounds_z,
      corr = corr,
      algorithm = algo
    )[[1]]

    (1 - prob_no_cross) - alpha
  }

  # Search for C. Boundaries are on the Z-scale, so C is typically 1-5
  C_root <- tryCatch(
    stats::uniroot(find_C, interval = c(0.1, 20), tol = abseps),
    error = function(e) {
      # Widen search if needed
      stats::uniroot(find_C, interval = c(0.01, 50), tol = abseps)
    }
  )
  C_val <- C_root$root
  bounds_z <- C_val * shape
  bounds_z <- pmin(bounds_z, 20)

  # Compute implied cumulative spending at each analysis k:
  # alpha_k = P(Z_1 >= c_1 or ... or Z_k >= c_k)
  #         = 1 - P(Z_1 < c_1, ..., Z_k < c_k)
  cum_spending <- numeric(K)
  for (k in seq_len(K)) {
    if (info_frac[k] == 0) {
      cum_spending[k] <- 0
      next
    }
    if (k == 1) {
      # Univariate case: P(Z >= c_1) = 1 - Phi(c_1)
      cum_spending[k] <- stats::pnorm(bounds_z[1], lower.tail = FALSE)
    } else {
      cum_spending[k] <- 1 - mvtnorm::pmvnorm(
        upper = bounds_z[seq_len(k)],
        corr = corr[seq_len(k), seq_len(k)],
        algorithm = algo
      )[[1]]
    }
  }

  # Cap at alpha for numerical stability
  cum_spending <- pmin(cum_spending, alpha)
  cum_spending
}
