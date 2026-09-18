#' Group sequential engines for the graphical procedure
#'
#' @description
#' The graphical procedure in [graph_test_shortcut_gsd()] relies on exactly two
#' group sequential computations: repeated p-values and nominal p-value
#' boundaries. An *engine* bundles these two computations so that the
#' graphical logic in `gsd_test()` and its helpers is independent of how they
#' are carried out.
#'
#' An engine is a list of two functions. Each takes the hypothesis index `j`
#' and closes over that hypothesis's spending specification:
#' * `repeated_p(p, info_frac, j)` - the repeated p-value for hypothesis `j`
#'   at the last of the supplied analyses, given the p-values and information
#'   fractions at all analyses up to and including it.
#' * `nominal_bounds(alpha, info_frac, j)` - the nominal p-value boundaries
#'   for hypothesis `j` at each supplied analysis when the significance level
#'   `alpha` is allocated to it.
#'
#' `gsd_engine_mvtnorm()` builds the engine backed by [repeated_p()] and
#' [gs_boundaries()], which evaluate the joint distribution of the test
#' statistics with mvtnorm.
#'
#' @param spending_fn A list of spending functions, one per hypothesis. Each
#'   must accept two arguments, `alpha` and `info_frac`, and return the
#'   cumulative alpha spent.
#'
#' @return A list with the functions `repeated_p` and `nominal_bounds`.
#'
#' @keywords internal
gsd_engine_mvtnorm <- function(spending_fn) {
  list(
    repeated_p = function(p, info_frac, j) {
      repeated_p(p = p, info_frac = info_frac, spending_fn = spending_fn[[j]])
    },
    nominal_bounds = function(alpha, info_frac, j) {
      gs_boundaries(
        alpha = alpha,
        info_frac = info_frac,
        spending_fn = spending_fn[[j]]
      )$bounds_nominal
    }
  )
}
