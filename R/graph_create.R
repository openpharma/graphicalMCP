#' Create the initial graph for a multiple comparison procedure
#'
#' @description
#' A graphical multiple comparison procedure is represented by 1) a vector of
#' initial hypothesis weights `hypotheses`, and 2) a matrix of initial
#' transition weights `transitions`. This function creates the initial graph
#' object using hypothesis weights and transition weights.
#'
#' @param hypotheses A numeric vector of hypothesis weights in a graphical
#'   multiple comparison procedure. Must be a vector of values between 0 & 1
#'   (inclusive). The length should match the row and column lengths of
#'   `transitions`. The sum of hypothesis weights should not exceed 1.
#' @param transitions A numeric matrix of transition weights between hypotheses
#'   in a graphical multiple comparison procedure. Must be a square matrix of
#'   values between 0 & 1 (inclusive). The row and column lengths should match
#'   the length of `hypotheses`. Each row (Transition weights leaving a
#'   hypothesis) can sum to no more than 1. The diagonal entries (Transition
#'   weights from a hypothesis to itself) must be all 0s.
#' @param hyp_names (Optional) A character vector of hypothesis names. If not
#'   provided, names from `hypotheses` and `transitions` will be used. If names
#'   are not specified, hypotheses will be named sequentially as H1, H2, .......
#'
#' @return An S3 object of class `initial_graph` with a list of 2 elements:
#'   * Hypothesis weights `hypotheses`.
#'   * Transition weights `transitions`.
#'
#' @section Validation of inputs:
#'  Inputs are also validated to make sure of the validity of the graph:
#'   * Hypothesis weights `hypotheses` are numeric.
#'   * Transition weights `transitions` are numeric.
#'   * Length of `hypotheses` and dimensions of `transitions` match.
#'   * Hypothesis weights `hypotheses` must be non-negative and sum to no more
#'     than 1.
#'   * Transition weights `transitions`:
#'      + Values must be non-negative.
#'      + Rows must sum to no more than 1.
#'      + Diagonal entries must be all 0.
#'   * Hypothesis names `hyp_names` override names in `hypotheses` or
#'     `transitions`.
#'
#' @seealso
#'   [graph_update()] for the updated graph after hypotheses being deleted
#'   from the initial graph.
#'
#' @rdname graph_create
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
#' hyp_names <- c("H11", "H12", "H21", "H22")
#' g <- graph_create(hypotheses, transitions, hyp_names)
#' g
#'
#' # Explicit names override names in `hypotheses` (with a warning)
#' hypotheses <- c(h1 = 0.5, h2 = 0.5, h3 = 0, h4 = 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- graph_create(hypotheses, transitions, hyp_names)
#' g
#'
#' # Use names in `transitions`
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   H1 = c(0, 0, 1, 0),
#'   H2 = c(0, 0, 0, 1),
#'   H3 = c(0, 1, 0, 0),
#'   H4 = c(1, 0, 0, 0)
#' )
#' g <- graph_create(hypotheses, transitions)
#' g
#'
#' # Unmatched names in `hypotheses` and `transitions` (with an error)
#' hypotheses <- c(h1 = 0.5, h2 = 0.5, h3 = 0, h4 = 0)
#' transitions <- rbind(
#'   H1 = c(0, 0, 1, 0),
#'   H2 = c(0, 0, 0, 1),
#'   H3 = c(0, 1, 0, 0),
#'   H4 = c(1, 0, 0, 0)
#' )
#' try(
#' g <- graph_create(hypotheses, transitions)
#' )
#'
#' # When names are not specified, hypotheses are numbered sequentially as
#' # H1, H2, ...
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- graph_create(hypotheses, transitions)
#' g
graph_create <- function(hypotheses, transitions, hyp_names = NULL) {
  # Basic input validation -----------------------------------------------------
  stopifnot(
    "Hypothesis weights must be numeric" = is.numeric(hypotheses),
    "Transition weights must be numeric" = is.numeric(transitions)
  )

  if (
    any(
      nrow(transitions) != ncol(transitions),
      nrow(transitions) != length(hypotheses),
      ncol(transitions) != length(hypotheses)
    )
  ) {
    stop("Length of `hypotheses`, rows of `transitions`, and columns of
             `transitions` must all match")
  }

  # Validation of names of hypotheses ----------------------------------------
  explicit_names <- !is.null(hyp_names)

  implicit_names <- any(
    !is.null(names(hypotheses)),
    !is.null(colnames(transitions)),
    !is.null(rownames(transitions))
  )

  names_diff <- any(
    names(hypotheses) != colnames(transitions),
    names(hypotheses) != rownames(transitions),
    colnames(transitions) != rownames(transitions)
  )

  if (implicit_names && explicit_names) {
    warning("Hypothesis names specified - overriding names in
                    `hypotheses` and `transitions`")
  } else if (implicit_names && names_diff) {
    stop("Names provided in `hypotheses` and `transitions` should match")
  } else if (implicit_names) {
    hyp_names <- unique(
      c(names(hypotheses), colnames(transitions), rownames(transitions))
    )
  } else if (!explicit_names) {
    hyp_names <- paste0("H", seq_along(hypotheses))
  }

  names(hypotheses) <-
    colnames(transitions) <- rownames(transitions) <- hyp_names

  # Validation of numerical conditions for a valid graphical multiple
  # comparison procedure -----------------------------------------------------
  if (any(hypotheses < 0 | hypotheses > 1)) {
    offending <- hypotheses[hypotheses < 0 | hypotheses > 1]
    not_zero_float <- sapply(offending, function(x) !isTRUE(all.equal(0, x)))
    not_one_float <- sapply(offending, function(x) !isTRUE(all.equal(1, x)))

    if (any(not_zero_float & not_one_float)) {
      stop("Hypothesis weights must be between 0 and 1")
    }
  }

  if (sum(hypotheses) > 1 && !isTRUE(all.equal(sum(hypotheses), 1))) {
    stop("Hypothesis weights must sum to no more than 1")
  }

  if (any(transitions < 0 | transitions > 1)) {
    offending <- transitions[transitions < 0 | transitions > 1, drop = TRUE]
    not_zero_float <- sapply(offending, function(x) !isTRUE(all.equal(0, x)))
    not_one_float <- sapply(offending, function(x) !isTRUE(all.equal(1, x)))

    if (any(not_zero_float & not_one_float)) {
      stop("Transition weights must be between 0 and 1")
    }
  }

  not_zero_float <- sapply(
    diag(transitions),
    function(x) !isTRUE(all.equal(0, x))
  )

  if (any(not_zero_float)) {
    stop("Diagonal of transition weights must be all 0s")
  }

  if (any(rowSums(transitions) > 1)) {
    not_one_float <- sapply(
      rowSums(transitions[rowSums(transitions) > 1, , drop = FALSE]),
      function(x) !isTRUE(all.equal(1, x))
    )

    if (any(not_one_float)) {
      stop("Transition weights from each row must sum to no more than 1")
    }
  }

  # Create an initial graph object ---------------------------------------------
  new_graph <- structure(
    list(hypotheses = hypotheses, transitions = transitions),
    class = "initial_graph",
    title = "Initial graph",
    deleted = NULL
  )
  new_graph
}
