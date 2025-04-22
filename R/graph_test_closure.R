#' Perform closed graphical multiple comparison procedures
#'
#' @description
#' Closed graphical multiple comparison procedures, or graphical multiple
#' comparison procedures based on the closure, generate the closure based on a
#' graph consisting of all intersection hypotheses. It tests each intersection
#' hypothesis and rejects an individual hypothesis if all intersection
#' hypotheses involving it have been rejected. An intersection hypothesis
#' represents the parameter space where individual null hypotheses involved are
#' true simultaneously.
#'
#' For a graphical multiple comparison procedure with $m$ hypotheses, there are
#' \eqn{2^m-1} intersection hypotheses. For each intersection hypothesis, a test
#' type could be chosen to determine how to reject the intersection hypothesis.
#' Current choices of test types include Bonferroni, Simes and parametric. This
#' implementation offers a more general framework covering Bretz et al. (2011),
#' Lu (2016), and Xi et al. (2017). See `vignette("closed-testing")` for more
#' illustration of closed test procedures and interpretation of their outputs.
#'
#' @inheritParams graph_update
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
#'
#' @return A `graph_report` object with a list of 4 elements:
#'   * `inputs` - Input parameters, which is a list of:
#'     * `graph` - Initial graph,
#'     * `p` - (Unadjusted or raw) p-values,
#'     * `alpha` - Overall significance level,
#'     * `test_groups` - Groups of hypotheses for different types of tests,
#'     * `test_types` - Different types of tests,
#'     * `test_corr` - Correlation matrices for parametric tests.
#'   * `outputs` - Output parameters, which is a list of:
#'     * `adjusted_p` - Adjusted p-values,
#'     * `rejected` - Rejected hypotheses,
#'     * `graph` - Updated graph after deleting all rejected hypotheses.
#'   * `details` - Verbose outputs with adjusted p-values for intersection
#'   hypotheses, if `verbose = TRUE`.
#'   * `test_values` - Adjusted significance levels, if `test_values = TRUE`.
#'
#' @section Details for test specification:
#' Test specification includes three components: `test_groups`, `test_types`,
#' and `test_corr`. Alignment among entries in these components is important
#' for correct implementation. There are two ways to provide test specification.
#' The first approach is the "unnamed" approach, which assumes that all 3
#' components are ordered the same way, i.e., the $n$-th element of `test_types`
#' and `test_corr` should apply to the $n$-th group in `test_groups`. The
#' second "named" approach uses the name of each element of each component to
#' connect the element of `test_types` and `test_corr` with the correct element
#' of `test_groups`. Consistency should be ensured for correct implementation.
#'
#' @seealso
#'   [graph_test_shortcut()] for shortcut graphical multiple comparison
#'   procedures.
#'
#' @rdname graph_test_closure
#'
#' @export
#'
#' @references
#'   Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
#'   Rohmeyer, K. (2011). Graphical approaches for multiple comparison
#'   procedures using weighted Bonferroni, Simes, or parametric tests.
#'   \emph{Biometrical Journal}, 53(6), 894-913.
#'
#'   Lu, K. (2016). Graphical approaches using a Bonferroni mixture of weighted
#'   Simes tests. \emph{Statistics in Medicine}, 35(22), 4041-4055.
#'
#'   Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
#'   for weighted parametric multiple test procedures.
#'   \emph{Biometrical Journal}, 59(5), 918-931.
#'
#' @examples
#' # A graphical multiple comparison procedure with two primary hypotheses
#' # (H1 and H2) and two secondary hypotheses (H3 and H4)
#' # See Figure 4 in Bretz et al. (2011).
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' delta <- 0.5
#' transitions <- rbind(
#'   c(0, delta, 1 - delta, 0),
#'   c(delta, 0, 0, 1 - delta),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- graph_create(hypotheses, transitions)
#'
#' p <- c(0.018, 0.01, 0.105, 0.006)
#' alpha <- 0.025
#'
#' # Closed graphical multiple comparison procedure using Bonferroni tests
#' # Same results as `graph_test_shortcut(g, p, alpha)`
#' graph_test_closure(g, p, alpha)
#'
#' # Closed graphical multiple comparison procedure using parametric tests for
#' # H1 and H2, and Bonferroni tests for H3 and H4
#' set.seed(1234)
#' corr_list <- list(matrix(c(1, 0.5, 0.5, 1), nrow = 2), NA)
#' graph_test_closure(
#'   graph = g,
#'   p = p,
#'   alpha = alpha,
#'   test_groups = list(1:2, 3:4),
#'   test_types = c("parametric", "bonferroni"),
#'   test_corr = corr_list
#' )
#' # The "named" approach to obtain the same results
#' # Note that "group2" appears before "group1" in `test_groups`
#' set.seed(1234)
#' corr_list <- list(group1 = matrix(c(1, 0.5, 0.5, 1), nrow = 2), group2 = NA)
#' graph_test_closure(
#'   graph = g,
#'   p = p,
#'   alpha = alpha,
#'   test_groups = list(group1 = 1:2, group2 = 3:4),
#'   test_types = c(group2 = "bonferroni", group1 = "parametric"),
#'   test_corr = corr_list
#' )
#'
#' # Closed graphical multiple comparison procedure using parametric tests for
#' # H1 and H2, and Simes tests for H3 and H4
#' set.seed(1234)
#' graph_test_closure(
#'   graph = g,
#'   p = p,
#'   alpha = alpha,
#'   test_groups = list(group1 = 1:2, group2 = 3:4),
#'   test_types = c(group1 = "parametric", group2 = "simes"),
#'   test_corr = corr_list
#' )
graph_test_closure <- function(graph,
                               p,
                               alpha = 0.025,
                               test_groups = list(seq_along(graph$hypotheses)),
                               test_types = c("bonferroni"),
                               test_corr = rep(list(NA), length(test_types)),
                               verbose = FALSE,
                               test_values = FALSE) {
  # Input validation & sanitization --------------------------------------------
  # Test types should be specified as full names or first initial,
  # case-insensitive. A single provided test type should be applied to all
  # groups.
  test_types_names <- names(test_types)
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
  test_types <- test_opts[tolower(test_types)]
  names(test_types) <- test_types_names
  if (length(test_types) == 1) {
    test_types <- rep(test_types, length(test_groups))
  }

  test_input_val(
    graph,
    p,
    alpha,
    test_groups,
    test_types,
    test_corr,
    verbose,
    test_values
  )

  # The test specification arguments can be named or not. However, if
  # `test_groups` is named, all of them must be named. The other two are
  # re-ordered to match `test_groups`
  if (!is.null(names(test_groups))) {
    if (!all(c(names(test_types), names(test_corr)) %in% names(test_groups))) {
      stop("If `test_groups` is named, `test_types` and `test_corr` must use the
           same names")
    } else {
      test_types <- test_types[names(test_groups)]
      test_corr <- test_corr[names(test_groups)]
    }
  } else {
    names(test_groups) <-
      names(test_types) <-
      names(test_corr) <-
      paste0("grp", seq_along(test_groups))
  }

  num_hyps <- length(graph$hypotheses)
  num_groups <- length(test_groups)

  hyp_names <- names(graph$hypotheses)
  names(p) <- hyp_names

  # Correlation matrix input is easier for end users to input as a list, but
  # it's easier to work with internally as a full matrix, potentially with
  # missing values. This puts all the correlation pieces into one matrix
  if (any(test_types == "parametric")) {
    test_corr <- parse_parametric_corr(
      test_groups,
      test_corr
    )
  } else {
    test_corr <- NULL
  }

  if (!is.null(test_corr)) dimnames(test_corr) <- list(hyp_names, hyp_names)

  # Generate weights of the closure --------------------------------------------
  weighting_strategy <- graph_generate_weights(graph)
  matrix_intersections <- weighting_strategy[, seq_len(num_hyps), drop = FALSE]

  # "Compact" representation shows hypothesis weights where a hypothesis is
  # present (even when that weight is 0), and NA where a hypothesis is missing.
  # This form represents the closure with only `num_hyps` columns
  weighting_strategy_compact <- ifelse(
    matrix_intersections,
    weighting_strategy[, seq_len(num_hyps) + num_hyps, drop = FALSE],
    NA_real_
  )

  num_intersections <- nrow(matrix_intersections)

  adjusted_p <- matrix(
    NA_real_,
    nrow = num_intersections,
    ncol = num_groups,
    dimnames = list(NULL, paste0("adj_p_grp", seq_along(test_groups)))
  )

  # Calculate adjusted p-values ------------------------------------------------
  # Adjusted p-values are calculated for each group in each intersection of the
  # closure
  for (intersection_index in seq_len(num_intersections)) {
    vec_intersection <- matrix_intersections[intersection_index, , drop = TRUE]
    vec_weights <-
      weighting_strategy_compact[intersection_index, , drop = TRUE]

    for (group_index in seq_len(num_groups)) {
      group <- test_groups[[group_index]]
      test <- test_types[[group_index]]

      # Hypotheses to include in adjusted p-value calculations must be in both
      # the current group and the current intersection
      group_by_intersection <- group[as.logical(vec_intersection[group])]

      # The adjusted p-value for a *group* has varying rules depending on the
      # test type. Each `adjust_p_*` function expects a whole group as input and
      # returns a single value as output (adjusted p-value for the whole group)
      if (test == "bonferroni") {
        adjusted_p[[intersection_index, group_index]] <- adjust_p_bonferroni(
          p[group_by_intersection],
          vec_weights[group_by_intersection]
        )
      } else if (test == "simes") {
        adjusted_p[[intersection_index, group_index]] <- adjust_p_simes(
          p[group_by_intersection],
          vec_weights[group_by_intersection]
        )
      } else if (test == "hochberg") {
        adjusted_p[[intersection_index, group_index]] <- adjust_p_hochberg(
          p[group_by_intersection],
          vec_weights[group_by_intersection]
        )
      } else if (test == "parametric") {
        adjusted_p[[intersection_index, group_index]] <- adjust_p_parametric(
          p[group_by_intersection],
          vec_weights[group_by_intersection],
          test_corr[group_by_intersection, group_by_intersection, drop = FALSE]
        )
      } else {
        stop(paste(test, "testing is not supported at this time"))
      }
    }
  }

  # Adjusted p-value summaries -------------------------------------------------
  # The adjusted p-value for an *intersection* is the smallest adjusted p-value
  # for the groups it contains
  adjusted_p_intersection <- apply(adjusted_p, 1, min)
  reject_intersection <-
    adjusted_p_intersection <= (alpha + .Machine$double.eps)

  # The adjusted p-value for a *hypothesis* is the largest adjusted p-value for
  # the intersections containing that hypothesis
  adjusted_p_hypothesis <-
    apply(adjusted_p_intersection * matrix_intersections, 2, max, na.rm = TRUE)
  reject_hypothesis <- adjusted_p_hypothesis <= alpha # Hypothesis test results

  # Adjusted p-value details ---------------------------------------------------
  # Reported adjusted p-values shouldn't exceed 1
  intersections <- apply(matrix_intersections, 1, paste, collapse = "")

  detail_results <- list(
    results = cbind(
      data.frame(Intersection = intersections),
      weighting_strategy_compact,
      pmin(adjusted_p, 1 + 1e-14),
      data.frame(adj_p_inter = pmin(adjusted_p_intersection, 1 + 1e-14)),
      data.frame(reject_intersection = reject_intersection)
    )
  )

  # Adjusted weight details ----------------------------------------------------
  if (test_values) {
    # Adjusted weights are recorded in a dataframe, which doesn't store in a
    # matrix. So for the test values loops, each group's adjusted weight
    # dataframe is stored in a list. These are the initialized list and counter
    # for indexing into it.
    test_values_index <- 1
    test_values_list <- vector("list", num_intersections * num_groups)

    # Adjusted weights are calculated for each group in each intersection of the
    # closure
    for (intersection_index in seq_len(num_intersections)) {
      vec_intersection <-
        matrix_intersections[intersection_index, , drop = TRUE]
      vec_weights <-
        weighting_strategy_compact[intersection_index, , drop = TRUE]

      str_intersection <- paste(vec_intersection, collapse = "")

      for (group_index in seq_len(num_groups)) {
        group <- test_groups[[group_index]]
        test <- test_types[[group_index]]

        # Hypotheses to include in adjusted weight calculations must be in both
        # the current group and the current intersection
        group_by_intersection <- group[as.logical(vec_intersection[group])]

        # adjusted weights, like adjusted p-values, must be calculated at both
        # the group and intersection level. Inputs are for a single group, and
        # output is a dataframe containing adjusted weight test information at
        # the hypothesis/operand level.
        if (test == "bonferroni") {
          test_values_list[[test_values_index]] <- test_values_bonferroni(
            p[group_by_intersection],
            vec_weights[group_by_intersection],
            alpha,
            str_intersection
          )
        } else if (test == "simes") {
          test_values_list[[test_values_index]] <- test_values_simes(
            p[group_by_intersection],
            vec_weights[group_by_intersection],
            alpha,
            str_intersection
          )
        } else if (test == "hochberg") {
          test_values_list[[test_values_index]] <- test_values_hochberg(
            p[group_by_intersection],
            vec_weights[group_by_intersection],
            alpha,
            str_intersection
          )
        } else if (test == "parametric") {
          test_values_list[[test_values_index]] <- test_values_parametric(
            p[group_by_intersection],
            vec_weights[group_by_intersection],
            alpha,
            str_intersection,
            test_corr[group_by_intersection,
              group_by_intersection,
              drop = FALSE
            ]
          )
        } else {
          stop(paste(test, "testing is not supported at this time"))
        }

        test_values_index <- test_values_index + 1
      }
    }

    df_test_values <- do.call(rbind, test_values_list)
    rownames(df_test_values) <- NULL

    # sort by hypothesis natural order
    df_test_values$Hypothesis <-
      factor(df_test_values$Hypothesis, levels = hyp_names, ordered = TRUE)

    df_test_values <-
      df_test_values[with(df_test_values, order(desc(Intersection), Hypothesis)), ]

    # "c" value is only used in parametric testing, so there's no need to
    # include this column when there are no parametric groups
    if (!any(test_types == "parametric")) {
      df_test_values[c("c_value")] <- NULL
    }
  }

  # Build the report -----------------------------------------------------------
  # The core output of a test report is the adjusted p-values, rejection
  # decisions, and resulting graph after deleting all rejected hypotheses.
  # Inputs are recorded as well. Details about adjusted p-values and test
  # values are optionally available.
  structure(
    list(
      inputs = list(
        graph = graph,
        p = p,
        alpha = alpha,
        test_groups = test_groups,
        test_types = test_types,
        test_corr = test_corr
      ),
      outputs = list(
        adjusted_p = pmin(adjusted_p_hypothesis, 1 + 1e-14), # Cap reported at 1
        rejected = reject_hypothesis,
        graph = graph_update(graph, reject_hypothesis)$updated_graph
      ),
      details = if (verbose) detail_results,
      test_values = if (test_values) list(results = df_test_values)
    ),
    class = "graph_report"
  )
}
