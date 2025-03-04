#' Validate inputs for testing and power simulations
#'
#' @param graph An initial graph as returned by [graph_create()].
#' @param p A numeric vector of p-values (unadjusted, raw), whose values should
#'   be between 0 & 1. The length should match the number of hypotheses in
#'   `graph`.
#' @param alpha A numeric value of the overall significance level, which should
#'   be between 0 & 1. The default is 0.025 for one-sided hypothesis testing
#'   problems; another common choice is 0.05 for two-sided hypothesis testing
#'   problems. Note when parametric tests are used, only one-sided tests are
#'   supported.
#' @param test_groups A list of numeric vectors specifying hypotheses to test
#'   together. Grouping is needed to correctly perform Simes and parametric
#'   tests.
#' @param test_types A character vector of test types to apply to each test
#'   group. This is needed to correctly perform Simes and parametric
#'   tests. The length should match the number of elements in `test_groups`.
#' @param test_corr (Optional) A list of numeric correlation matrices. Each
#'   entry in the list should correspond to each test group. For a test group
#'   using Bonferroni or Simes tests, its corresponding entry in `test_corr`
#'   should be `NA`. For a test group using parametric tests, its
#'   corresponding entry in `test_corr` should be a numeric correlation matrix
#'   specifying the correlation between test statistics for hypotheses in this
#'   test group. The length should match the number of elements in
#'   `test_groups`.
#' @param verbose A logical scalar specifying whether the details of the
#'   adjusted p-value calculations should be included in results. When
#'   `verbose = TRUE`, adjusted p-values are provided for each intersection
#'   hypothesis. The default is `verbose = FALSE`.
#' @param test_values A logical scalar specifying whether adjusted significance
#'   levels should be provided for each hypothesis. When `test_values = TRUE`,
#'   it provides an equivalent way of performing graphical multiple comparison
#'   procedures by comparing each p-value with its significance level. If the
#'   p-value of a hypothesis is less than or equal to its significance level,
#'   the hypothesis is rejected. The default is `test_values = FALSE`.
#' @param sim_n An integer scalar specifying the number of simulations. The
#'   default is 1e5.
#' @param power_marginal A numeric vector of marginal power values to use when
#'   simulating p-values. See Details for more on the simulation process.
#' @param success A list of user-defined functions to specify the success
#'   criteria. Functions must take one simulation's logical vector of results as
#'   an input, and return a length-one logical vector. For instance, if
#'   "success" means rejecting hypotheses 1 and 2, use `sim_success = list("1
#'   and 2" = function(x) x[1] && x[2])`. If the list is not named, the function
#'   body will be used as the name. Lambda functions also work starting with R
#'   4.1, e.g. `sim_success = list(\(x) x[3] || x[4])`.
#'
#' @return Returns `graph` invisibly
#'
#' @rdname input_val
#'
#' @keywords internal
test_input_val <- function(graph,
                           p,
                           alpha,
                           test_groups = list(seq_along(graph$hypotheses)),
                           test_types = c("bonferroni"),
                           test_corr,
                           verbose,
                           test_values) {
  test_opts <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    hochberg = "hochberg",
    b = "bonferroni",
    p = "parametric",
    s = "simes",
    h = "hochberg"
  )

  corr_is_matrix_list <- is.list(test_corr) &&
    all(
      vapply(test_corr, function(elt) is.matrix(elt) || is.na(elt), logical(1))
    )

  stopifnot(
    "Please test an `initial_graph` object" = class(graph) == "initial_graph",
    "P-values must be numeric" = is.numeric(p),
    "P-values must be between 0 & 1" = all(p >= 0 & p <= 1),
    "Alpha must be numeric" = is.numeric(alpha),
    "Please choose a single alpha level for testing" = length(alpha) == 1,
    "Alpha must be between 0 & 1" = alpha >= 0 && alpha <= 1,
    "Only Bonferroni, parametric, Simes, or Hochberg tests are currently supported" =
      all(test_types %in% test_opts),
    "Groups specification must be a list" = is.list(test_groups),
    "Please include each hypothesis in exactly one group" =
      setequal(seq_along(graph$hypotheses), unlist(test_groups)) &&
        length(graph$hypotheses) == length(unlist(test_groups)),
    "Correlation matrix should be a list of matrices or missing values" =
      corr_is_matrix_list,
    "Number of test types, groups, and correlation matrices should match" =
      unique(length(test_types), length(test_groups)) == length(test_corr),
    "Length of p-values & groups must match the number of hypotheses" =
      unique(length(p), length(unlist(test_groups))) ==
        length(graph$hypotheses),
    "Verbose flag must be a length one logical" =
      is.logical(verbose) && length(verbose) == 1,
    "Test values flag must be a length one logical" =
      is.logical(test_values) && length(test_values) == 1
  )

  # Additional correlation matrix checks ---------------------------------------
  if (is.null(names(test_types))) {
    corr_parametric <- test_corr[test_types == "parametric"]
  } else {
    corr_parametric <- test_corr[names(test_types)[test_types == "parametric"]]
  }

  missing_corr <- any(
    vapply(corr_parametric, function(cr) any(is.na(cr)), logical(1))
  )

  symmetric_corr <- all(vapply(corr_parametric, isSymmetric.matrix, logical(1)))

  bounded_corr <- all(
    vapply(corr_parametric, function(cr) all(cr >= 0 & cr <= 1), logical(1))
  )

  # Positive definite-ness is irrelevant if there are missing values, and
  # testing for it will throw an error
  positive_definite_corr <- ifelse(
    !missing_corr,
    all(
      vapply(
        corr_parametric,
        function(cr) all(round(eigen(cr)$values, 10) >= 0),
        logical(1)
      )
    ),
    TRUE
  )

  stopifnot(
    "Correlation matrix for parametric test groups must be fully specified" =
      !missing_corr,
    "Correlation matrix must be symmetric" = symmetric_corr,
    "Dimensions of correlation matrices must match the parametric test groups" =
      all(
        lengths(test_corr[names(test_types)[test_types == "parametric"]]) ==
          lengths(test_groups[names(test_types)[test_types == "parametric"]])^2
      ),
    "Correlation values must be between 0 & 1" = bounded_corr,
    "Correlation matrix must be positive definite for parametric test groups" =
      positive_definite_corr
  )

  # Additional Hochberg checks -------------------------------------------------
  if (is.null(names(test_types))) {
    groups_hochberg <- test_groups[test_types == "hochberg"]
  } else {
    groups_hochberg <- test_groups[names(test_types)[test_types == "hochberg"]]
  }

  num_hyps <- length(graph$hypotheses)
  weighting_strategy <- graph_generate_weights(graph)
  matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
  matrix_weights <- weighting_strategy[, -seq_len(num_hyps)]
  weighting_strategy_compact <- ifelse(
    matrix_intersections,
    matrix_weights,
    NA_real_
  )

  hochberg_weights_all_equal <- TRUE
  for (row in seq_len(nrow(weighting_strategy_compact))) {
    for (group in groups_hochberg) {
      hypotheses <- weighting_strategy_compact[row, group, drop = TRUE]
      hypotheses <- hypotheses[!is.na(hypotheses)]

      hypotheses_all_equal <- length(unique(hypotheses)) <= 1

      hochberg_weights_all_equal <-
        hochberg_weights_all_equal && hypotheses_all_equal
    }
  }

  stopifnot("Each Hochberg group must have uniform weights in all subgraphs" =
              hochberg_weights_all_equal)

  invisible(graph)
}

#' @rdname input_val
#' @keywords internal
power_input_val <- function(graph, sim_n, power_marginal, test_corr, success) {
  num_hyps <- length(graph$hypotheses)

  stopifnot(
    "Number of simulations must be a length one integer" =
      is.numeric(sim_n) && as.integer(sim_n) == sim_n && length(sim_n) == 1,
    "Marginal power must be between 0 and 1" =
      all(power_marginal >= 0 & power_marginal <= 1),
    "Marginal power and correlation parameters must be numeric" =
      is.numeric(power_marginal) && is.numeric(test_corr),
    "Lengths of marginal power must match number of hypotheses" =
      length(power_marginal) == num_hyps,
    "Correlation matrix for simulating p-values must match no. of hypotheses" =
      unique(nrow(test_corr), ncol(test_corr)) == num_hyps,
    "Correlation matrix for simulating p-values cannot have missing values" =
      !any(is.na(test_corr)),
    "Correlation matrix for simulating p-values must be symmetric" =
      isSymmetric.matrix(test_corr),
    "Correlation matrix for simulating p-values must have diagonal all 1" =
      all(diag(test_corr) == 1),
    "Correlation matrix for simulating p-values must be positive definite" =
      all(round(eigen(test_corr)$values, 10) >= 0),
    "'sim_success' must be a list of functions" =
      all(vapply(success, is.function, logical(1)))
  )

  invisible(graph)
}
