#' Organize outputs for testing an intersection hypothesis
#'
#' @description
#' An intersection hypothesis can be tested by a mixture of test types including
#'   Bonferroni, parametric and Simes tests. This function organize outputs of
#'   testing and prepare them for `graph_report`.
#'
#' @inheritParams graph_test_closure
#' @inheritParams graph_create
#' @param intersection (optional) A numeric scalar used to name the
#'   intersection hypothesis in a weighting strategy.
#'
#' @return A data frame with rows corresponding to individual hypotheses
#'   involved in the intersection hypothesis with hypothesis weights
#'   `hypotheses`. There are following columns:
#'   * `Intersection` - Name of this intersection hypothesis,
#'   * `Hypothesis` - Name of an individual hypothesis,
#'   * `Test` - Test type for an individual hypothesis,
#'   * `p` - (Unadjusted or raw) p-values for a individual hypothesis,
#'   * `c_value`- C value for parametric tests,
#'   * `Weight` - Hypothesis weight for an individual hypothesis,
#'   * `Alpha` - Overall significance level \eqn{\alpha},
#'   * `Inequality_holds` - Indicator to show if the p-value is less than or
#'     equal to its significance level.
#'       - For Bonferroni and Simes tests, the significance level is the
#'         hypothesis weight times \eqn{\alpha}.
#'       - For parametric tests, the significance level is the c value times
#'         the hypothesis weight times \eqn{\alpha}.
#'
#' @rdname test_values
#'
#' @keywords internal
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
test_values_bonferroni <- function(p, hypotheses, alpha, intersection = NA) {
  if (length(p) == 0) {
    NULL
  } else {
    data.frame(
      Intersection = intersection,
      Hypothesis = names(hypotheses),
      Test = "bonferroni",
      p = p,
      "c_value" = "",
      "Weight" = hypotheses,
      Alpha = alpha,
      Inequality_holds = ifelse(
        p == 0 & hypotheses == 0,
        NA,
        p <= hypotheses * alpha
      ),
      check.names = FALSE
    )
  }
}

#' @rdname test_values
#' @keywords internal
test_values_parametric <- function(p,
                                   hypotheses,
                                   alpha,
                                   intersection = NA,
                                   test_corr) {
  if (length(p) == 0) {
    NULL
  } else {
    c_value <- solve_c_parametric(hypotheses, test_corr, alpha)

    data.frame(
      Intersection = intersection,
      Hypothesis = names(hypotheses),
      Test = "parametric",
      p = p,
      "c_value" = c_value,
      "Weight" = hypotheses,
      Alpha = alpha,
      Inequality_holds = ifelse(
        p == 0 & hypotheses == 0,
        NA,
        p <= c_value * hypotheses * alpha
      ),
      check.names = FALSE
    )
  }
}

#' @rdname test_values
#' @keywords internal
test_values_simes <- function(p, hypotheses, alpha, intersection = NA) {
  if (length(p) == 0) {
    NULL
  } else {
    vec_res <- vector(length = length(hypotheses))
    w_sum <- vector("numeric", length = length(hypotheses))

    for (i in seq_along(hypotheses)) {
      w_sum[[i]] <- sum(hypotheses[p <= p[[i]]])
      vec_res[[i]] <- p[[i]] <= alpha * w_sum[[i]]
    }

    data.frame(
      Intersection = intersection,
      Hypothesis = names(hypotheses),
      Test = "simes",
      p = p,
      "c_value" = "",
      "Weight" = w_sum,
      Alpha = alpha,
      Inequality_holds = ifelse(
        p == 0 & w_sum == 0,
        NA,
        vec_res
      ),
      check.names = FALSE
    )
  }
}

#' @rdname test_values
#' @keywords internal
test_values_hochberg <- function(p, hypotheses, alpha, intersection = NA) {
  if (length(p) == 0) {
    NULL
  } else {
    vec_res <- vector(length = length(hypotheses))
    w_quo <- vector("numeric", length = length(hypotheses))
    total_weight <- sum(hypotheses)

    for (i in seq_along(hypotheses)) {
      w_quo[[i]] <- total_weight / sum(p <= p[[i]])
      vec_res[[i]] <- p[[i]] <= alpha * w_quo[[i]]
    }

    data.frame(
      Intersection = intersection,
      Hypothesis = names(hypotheses),
      Test = "hochberg",
      p = p,
      "c_value" = "",
      "Weight" = w_quo,
      Alpha = alpha,
      Inequality_holds = ifelse(
        p == 0 & w_quo == 0,
        NA,
        vec_res
      ),
      check.names = FALSE
    )
  }
}
