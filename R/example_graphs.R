#' Example graphs of commonly used multiple comparison procedures
#'
#' @description
#' Built-in functions to quickly generate select graphical multiple comparison
#' procedures.
#'
#' @param hypotheses (Optional) A numeric vector of hypothesis weights in a
#'   graphical multiple comparison procedure. Must be a vector of values
#'   between 0 & 1 (inclusive). The length should match `num_hyps` and the
#'   length of `hyp_names`. The sum of hypothesis weights should not exceed 1.
#' @param hyp_names (Optional) A character vector of hypothesis names. The
#'   length should match `num_hyps` and the length of `hypotheses`. If
#'   `hyp_names` are not specified, hypotheses will be named sequentially as
#'   H1, H2, .......
#' @param epsilon (Optional) A numeric scalar indicating the value of the
#'   \eqn{\epsilon} edge. This should be a much smaller value than hypothesis
#'   and transition weights. The default is 1e-4.
#' @param num_hyps (Optional) Number of hypotheses in a graphical multiple
#'   comparison procedure.
#'
#' @return An S3 object as returned by [graph_create()].
#'
#' @seealso
#'   [graph_create()] for a general way to create the initial graph.
#'
#' @rdname example_graphs
#'
#' @export
#'
#' @references
#'   Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
#'   approach to sequentially rejective multiple test procedures.
#'   \emph{Statistics in Medicine}, 28(4), 586-604.
#'
#'   Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
#'   Rohmeyer, K. (2011). Graphical approaches for multiple comparison
#'   procedures using weighted Bonferroni, Simes, or parametric tests.
#'   \emph{Biometrical Journal}, 53(6), 894-913.
#'
#'   Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of
#'   significance. \emph{Biometrika}, 75(4), 800-802.
#'
#'   Hommel, G. (1988). A stagewise rejective multiple test procedure based on a
#'   modified Bonferroni test. \emph{Biometrika}, 75(2), 383-386.
#'
#'   Huque, M. F., Alosh, M., and Bhore, R. (2011). Addressing multiplicity
#'   issues of a composite endpoint and its components in clinical trials.
#'   \emph{Journal of Biopharmaceutical Statistics}, 21(4), 610-634.
#'
#'   Maurer, W., Hothorn, L., and Lehmacher, W. (1995). Multiple comparisons in
#'   drug clinical trials and preclinical assays: a-priori ordered hypotheses.
#'   \emph{Biometrie in der chemisch-pharmazeutischen Industrie}, 6, 3-18.
#'
#'   Šidák, Z. (1967). Rectangular confidence regions for the means of
#'   multivariate normal distributions. \emph{Journal of the American Statistical
#'   Association}, 62(318), 626-633.
#'
#'   Westfall, P. H., and Krishen, A. (2001). Optimally weighted, fixed sequence
#'   and gatekeeper multiple testing procedures.
#'   \emph{Journal of Statistical Planning and Inference}, 99(1), 25-40.
#'
#'   Wiens, B. L. (2003). A fixed sequence Bonferroni procedure for testing
#'   multiple endpoints. \emph{Pharmaceutical Statistics}, 2(3), 211-215.
#'
#'   Wiens, B. L., and Dmitrienko, A. (2005). The fallback procedure for
#'   evaluating a single family of hypotheses.
#'   \emph{Journal of Biopharmaceutical Statistics}, 15(6), 929-942.
#'
#'   Xi, D., and Bretz, F. (2019). Symmetric graphs for equally weighted tests,
#'   with application to the Hochberg procedure. \emph{Statistics in Medicine},
#'   38(27), 5268-5282.
#'
#' @examples
#' # Bretz et al. (2009)
#' bonferroni(num_hyps = 3)
bonferroni <- function(num_hyps, hyp_names = NULL) {
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  hypotheses <- rep(1 / num_hyps, num_hyps)
  transitions <- matrix(0, num_hyps, num_hyps)

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Bretz et al. (2009)
#' hypotheses <- c(0.5, 0.3, 0.2)
#' bonferroni_weighted(hypotheses)
bonferroni_weighted <- function(hypotheses, hyp_names = NULL) {
  num_hyps <- length(hypotheses)
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  transitions <- matrix(0, num_hyps, num_hyps)

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Bretz et al. (2009)
#' bonferroni_holm(num_hyps = 3)
bonferroni_holm <- function(num_hyps, hyp_names = NULL) {
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  hypotheses <- rep(1 / num_hyps, num_hyps)
  transitions <- matrix(rep(1 / (num_hyps - 1), num_hyps^2), nrow = num_hyps)
  diag(transitions) <- rep(0, num_hyps)

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Bretz et al. (2009)
#' hypotheses <- c(0.5, 0.3, 0.2)
#' bonferroni_holm_weighted(hypotheses)
bonferroni_holm_weighted <- function(hypotheses, hyp_names = NULL) {
  num_hyps <- length(hypotheses)
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  transitions <- matrix(rep(1 / (num_hyps - 1), num_hyps^2), nrow = num_hyps)
  diag(transitions) <- rep(0, num_hyps)

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Xi et al. (2017)
#' dunnett_single_step(num_hyps = 3)
dunnett_single_step <- function(num_hyps, hyp_names = NULL) {
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  hypotheses <- rep(1 / num_hyps, num_hyps)
  transitions <- matrix(0, num_hyps, num_hyps)

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Xi et al. (2017)
#' hypotheses <- c(0.5, 0.3, 0.2)
#' dunnett_single_step_weighted(hypotheses)
dunnett_single_step_weighted <- function(hypotheses, hyp_names = NULL) {
  num_hyps <- length(hypotheses)
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  transitions <- matrix(0, num_hyps, num_hyps)

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Xi et al. (2009)
#' hypotheses <- c(0.5, 0.3, 0.2)
#' dunnett_closure_weighted(hypotheses)
dunnett_closure_weighted <- function(hypotheses, hyp_names = NULL) {
  num_hyps <- length(hypotheses)
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  transitions <- matrix(rep(1 / (num_hyps - 1), num_hyps^2), nrow = num_hyps)
  diag(transitions) <- rep(0, num_hyps)

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Hochberg (1988)
#' hochberg(num_hyps = 3)
hochberg <- function(num_hyps, hyp_names = NULL) {
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  hypotheses <- rep(1 / num_hyps, num_hyps)
  transitions <- matrix(rep(1 / (num_hyps - 1), num_hyps^2), nrow = num_hyps)
  diag(transitions) <- rep(0, num_hyps)

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Hommel (1988)
#' hommel(num_hyps = 3)
hommel <- function(num_hyps, hyp_names = NULL) {
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  hypotheses <- rep(1 / num_hyps, num_hyps)
  transitions <- matrix(rep(1 / (num_hyps - 1), num_hyps^2), nrow = num_hyps)
  diag(transitions) <- rep(0, num_hyps)

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Huque et al. (2011)
#' huque_etal()
huque_etal <- function(hyp_names = NULL) {
  graph_create(
    c(1, 0, 0, 0),
    matrix(
      c(
        0, 0.5, 0.5, 0,
        0, 0, 0, 1,
        0, 0.5, 0, 0.5,
        0, 1, 0, 0
      ),
      nrow = 4,
      byrow = TRUE
    ),
    hyp_names = hyp_names
  )
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Wiens (2003)
#' hypotheses <- c(0.5, 0.3, 0.2)
#' fallback(hypotheses)
fallback <- function(hypotheses, hyp_names = NULL) {
  num_hyps <- length(hypotheses)
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  transitions <- matrix(0, nrow = num_hyps, ncol = num_hyps)
  for (i in seq_len(num_hyps - 1)) {
    transitions[i, i + 1] <- 1
  }

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Wiens and Dmitrienko (2005)
#' hypotheses <- c(0.5, 0.3, 0.2)
#' fallback_improved_1(hypotheses)
fallback_improved_1 <- function(hypotheses, hyp_names = NULL) {
  num_hyps <- length(hypotheses)
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names)),
    "sum of all hypothesis weights excluding the last one should be greater
      than 0" = sum(hypotheses[seq_len(num_hyps - 1)]) > 0
  )
  transitions <- matrix(0, nrow = num_hyps, ncol = num_hyps)
  for (i in seq_len(num_hyps - 1)) {
    transitions[i, i + 1] <- 1
  }
  transitions[num_hyps, seq_len(num_hyps - 1)] <-
    hypotheses[seq_len(num_hyps - 1)] / sum(hypotheses[seq_len(num_hyps - 1)])

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Bretz et al. (2009)
#' hypotheses <- c(0.5, 0.3, 0.2)
#' fallback_improved_2(hypotheses)
fallback_improved_2 <- function(hypotheses, epsilon = 1e-4, hyp_names = NULL) {
  num_hyps <- length(hypotheses)
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  transitions <- matrix(0, nrow = num_hyps, ncol = num_hyps)
  if (num_hyps == 2) {
    transitions[1, 2] <- transition[2, 1] <- 1
  } else if (num_hyps > 2) {
    transitions[, 1] <- c(0, rep(1 - epsilon, num_hyps - 2), 1)
    transitions[1, 2] <- 1
    for (i in 2:(num_hyps - 1)) {
      transitions[i, i + 1] <- epsilon
    }
  }

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Maurer et al. (1995); Westfall and Krishen (2001)
#' fixed_sequence(num_hyps = 3)
fixed_sequence <- function(num_hyps, hyp_names = NULL) {
  hypotheses <- c(1, rep(0, num_hyps - 1))
  transitions <- matrix(0, nrow = num_hyps, ncol = num_hyps)
  for (i in seq_len(num_hyps - 1)) {
    transitions[i, i + 1] <- 1
  }

  graph_create(hypotheses, transitions, hyp_names)
}


#' @export
#' @rdname example_graphs
#' @examples
#' # sidak (1967)
#' sidak(num_hyps = 3)
sidak <- function(num_hyps, hyp_names = NULL) {
  stopifnot(
    "number of hypotheses must match number of names" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )
  hypotheses <- rep(1 / num_hyps, num_hyps)
  transitions <- matrix(0, num_hyps, num_hyps)

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Figure 1 in Bretz et al. (2011)
#' simple_successive_1()
simple_successive_1 <- function(hyp_names = NULL) {
  hypotheses <- c(0.5, 0.5, 0, 0)
  transitions <- rbind(
    c(0, 0, 1, 0),
    c(0, 0, 0, 1),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0)
  )

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Figure 4 in Bretz et al. (2011)
#' simple_successive_2()
simple_successive_2 <- function(hyp_names = NULL) {
  hypotheses <- c(0.5, 0.5, 0, 0)
  transitions <- rbind(
    c(0, 0.5, 0.5, 0),
    c(0.5, 0, 0, 0.5),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0)
  )

  graph_create(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Figure 6 in Xi and Bretz et al. (2019)
#' two_doses_two_primary_two_secondary()
two_doses_two_primary_two_secondary <- function(hyp_names = NULL) {
  eps <- 1e-4
  weights <- c(rep(c(1 / 2, 0, 0), 2))
  transitions <- rbind(
    c(0, 0.5, 0.5, 0, 0, 0),
    c(0, 0, 1, 0, 0, 0),
    c(0, 1 - eps, 0, eps, 0, 0),
    c(0, 0, 0, 0, 0.5, 0.5),
    c(0, 0, 0, 0, 0, 1),
    c(eps, 0, 0, 0, 1 - eps, 0)
  )

  graph_create(weights, transitions, hyp_names = hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Add another dose to Figure 6 in Xi and Bretz et al. (2019)
#' three_doses_two_primary_two_secondary()
three_doses_two_primary_two_secondary <- function(hyp_names = NULL) {
  eps <- 1e-4
  weights <- c(rep(c(1 / 3, 0, 0), 3))
  transitions <- rbind(
    c(0, 0.5, 0.5, 0, 0, 0, 0, 0, 0), # 1 --> 2 & 3
    c(0, 0, 1, 0, 0, 0, 0, 0, 0), # 2 --> 3
    c(0, 1 - eps, 0, eps / 2, 0, 0, eps / 2, 0, 0), # 3 --> 2, 3 - - > 4 & 7
    c(0, 0, 0, 0, 0.5, 0.5, 0, 0, 0), # 4 --> 5 & 6
    c(0, 0, 0, 0, 0, 1, 0, 0, 0), # 5 --> 6
    c(eps / 2, 0, 0, 0, 1 - eps, 0, eps / 2, 0, 0), # 6 --> 5, 6 - - > 1 & 7
    c(0, 0, 0, 0, 0, 0, 0, 0.5, 0.5), # 7 --> 8 & 9
    c(0, 0, 0, 0, 0, 0, 0, 0, 1), # 8 --> 9
    c(eps / 2, 0, 0, eps / 2, 0, 0, 0, 1 - eps, 0) # 9 --> 8, 9 - - > 1 & 4
  )

  graph_create(weights, transitions, hyp_names = hyp_names)
}

#' @export
#' @rdname example_graphs
#' @examples
#' # Create a random graph with three hypotheses
#' random_graph(num_hyps = 3)
random_graph <- function(num_hyps, hyp_names = NULL) {
  hypotheses <- sample(seq_len(num_hyps), replace = TRUE)
  hypotheses <- hypotheses / sum(hypotheses)
  transitions <- replicate(
    num_hyps,
    sample(seq_len(num_hyps), replace = TRUE),
    simplify = TRUE
  )
  diag(transitions) <- 0
  transitions <- transitions / rowSums(transitions)

  graph_create(hypotheses, transitions, hyp_names)
}
