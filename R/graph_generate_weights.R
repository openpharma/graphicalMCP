#' Generate the weighting strategy based on a graphical multiple comparison
#' procedure
#'
#' @description
#' A graphical multiple comparison procedure defines a closed test procedure,
#' which tests each intersection hypothesis and reject an individual hypothesis
#' if all intersection hypotheses involving it have been rejected. An
#' intersection hypothesis represents the parameter space where individual null
#' hypotheses involved are true simultaneously.
#'
#' The closure based on a graph consists of all updated graphs (corresponding
#' to intersection hypotheses) after all combinations of hypotheses are deleted.
#' For a graphical multiple comparison procedure with \eqn{m} hypotheses, there
#' are \eqn{2^{m}-1} updated graphs (intersection hypotheses), including the
#' initial graph (the overall intersection hypothesis). The weighting strategy
#' of this graph consists of hypothesis weights from all \eqn{2^{m}-1} updated
#' graphs (intersection hypotheses). The algorithm to derive the weighting
#' strategy is based on Algorithm 1 in Bretz et al. (2011).
#'
#' @inheritParams graph_update
#'
#' @return A numeric matrix of all intersection hypotheses and their hypothesis
#' weights. For a graphical multiple comparison procedure with \eqn{m} hypotheses,
#' the number of rows is \eqn{2^{m}-1}, each of which corresponds to an intersection
#' hypothesis. The number of columns is \eqn{2\cdot m}. The first \eqn{m} columns
#' indicate which individual hypotheses are included in a given intersection
#' hypothesis and the second half of columns provide hypothesis weights for each
#' individual hypothesis for a given intersection hypothesis.
#'
#' @section Performance:
#' Generation of intersection hypotheses is closely related to the power set
#' of a given set of indices. As the number of hypotheses increases, the memory
#' and time usage can grow quickly (e.g., at a rate of \eqn{O(2^n)}). There are also
#' multiple ways to implement Algorithm 1 in Bretz et al. (2011). See
#' `vignette("generate-closure")` for more information about generating
#' intersection hypotheses and comparisons of different approaches to calculate
#' weighting strategies.
#'
#' @seealso
#'   [graph_test_closure()] for graphical multiple comparison procedures using
#'   the closed test.
#'
#' @rdname graph_generate_weights
#'
#' @export
#'
#' @references
#'   Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
#'   Rohmeyer, K. (2011). Graphical approaches for multiple comparison
#'   procedures using weighted Bonferroni, Simes, or parametric tests.
#'   \emph{Biometrical Journal}, 53(6), 894-913.
#'
#' @examples
#' # A graphical multiple comparison procedure with two primary hypotheses (H1
#' # and H2) and two secondary hypotheses (H3 and H4)
#' # See Figure 1 in Bretz et al. (2011).
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- graph_create(hypotheses, transitions)
#'
#' graph_generate_weights(g)
graph_generate_weights <- function(graph) {
  test_graph_names(graph)

  hyp_names <- names(graph$hypotheses)
  num_hyps <- length(graph$hypotheses)

  parents <- do.call(c, lapply(2^(seq_len(num_hyps) - 1), seq_len))
  parents <- parents[-(2^num_hyps - 1)]

  delete <- rep(rev(seq_len(num_hyps)), 2^(seq_len(num_hyps) - 1))
  delete <- delete[-(2^num_hyps - 1)]

  graphs <- vector("list", length(parents))
  graphs[[1]] <- graph

  matrix_weights <- matrix(nrow = 2^num_hyps - 1, ncol = num_hyps)
  dimnames(matrix_weights) <- list(seq_len(2^num_hyps - 1), hyp_names)
  matrix_weights[1, ] <- graph$hypotheses

  for (i in seq_along(parents)) {
    parent <- graphs[[parents[[i]]]]
    del_index <- which(hyp_names[[delete[[i]]]] == names(parent$hypotheses))

    init_hypotheses <- parent$hypotheses
    init_transitions <- parent$transitions

    hypotheses <- parent$hypotheses
    transitions <- parent$transitions

    hyp_nums <- seq_along(hypotheses)[-del_index]

    for (hyp_num in hyp_nums) {
      hypotheses[[hyp_num]] <-
        init_hypotheses[[hyp_num]] +
        init_hypotheses[[del_index]] * init_transitions[[del_index, hyp_num]]

      denominator <- 1 - init_transitions[[hyp_num, del_index]] *
        init_transitions[[del_index, hyp_num]]

      for (end_num in hyp_nums) {
        if (hyp_num == end_num || denominator <= 0) {
          transitions[[hyp_num, end_num]] <- 0
        } else {
          transitions[[hyp_num, end_num]] <-
            (init_transitions[[hyp_num, end_num]] +
              init_transitions[[hyp_num, del_index]] *
                init_transitions[[del_index, end_num]]) / denominator
        }
      }
    }

    graphs[[i + 1]] <- structure(
      list(
        hypotheses = hypotheses[-del_index],
        transitions =
          as.matrix(transitions[-del_index, -del_index, drop = FALSE])
      ),
      class = "initial_graph"
    )

    matrix_weights[i + 1, ] <- hypotheses[-del_index][hyp_names]
  }

  matrix_intersections <- !is.na(matrix_weights)
  matrix_weights[is.na(matrix_weights)] <- 0

  cbind(matrix_intersections, matrix_weights)
}
