#' Obtain an updated graph by updating an initial graphical after deleting
#' hypotheses
#'
#' @description
#' After a hypothesis is deleted, an initial graph will be updated. The deleted
#' hypothesis will have the hypothesis weight of 0 and the transition weight of
#' 0. Remaining hypotheses will have updated hypothesis weights and transition
#' weights according to Algorithm 1 of Bretz et al. (2009).
#'
#' @param graph An initial graph as returned by [graph_create()].
#' @param delete A logical or integer vector, denoting which hypotheses to
#'   delete. A logical vector results in the "unordered mode", which means that
#'   hypotheses corresponding to `TRUE` in `delete` will be deleted. The
#'   sequence of deletion will follow the sequence of `TRUE`'s in `delete`. In
#'   this case, the length of the logical vector must match the number of
#'   hypotheses in `graph`. An integer vector results in the "ordered mode",
#'   which means that `delete` specifies the sequence in which hypotheses
#'   should be deleted by indicating the location of deleted hypotheses, e.g.,
#'   1st, 2nd, etc. In this case, the integer vector can have any length, but
#'   must only contain valid hypothesis numbers (greater than 0, and less than
#'   or equal to he number of hypotheses in `graph`).
#'
#' @return An S3 object of class `updated_graph` with a list of 4 elements:
#'   * `initial_graph`: The initial graph object.
#'   * `updated_graph`: The updated graph object with specified hypotheses
#'     deleted.
#'   * `deleted`: A numeric vector indicating which hypotheses were deleted.
#'   * `intermediate_graphs`: When using the ordered mode, a list of
#'     intermediate updated graphs after each hypothesis is deleted according
#'     to the sequence specified by `delete`.
#'
#' @section Sequence of deletion:
#' When there are multiple hypotheses to be deleted from a graph, there are many
#' sequences of deletion in which an initial graph is updated to an updated
#' graph. If the interest is in the updated graph after all hypotheses specified
#' by `delete` are deleted, this updated graph is the same no matter which
#' sequence of deletion is used. This property has been proved by Bretz et al.
#' (2009). If the interest is in the intermediate updated graph after each
#' hypothesis is deleted according to the sequence specified by `delete`, an
#' integer vector of `delete` should be specified and these detailed outputs
#' will be provided.
#'
#' @seealso
#'   * [graph_create()] for the initial graph.
#'   * [graph_rejection_orderings()] for possible sequences of rejections for a
#'     graphical multiple comparison procedure using shortcut testing.
#'
#' @rdname graph_update
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
#' g <- graph_create(hypotheses, transitions)
#'
#' # Delete the second and third hypotheses in the "unordered mode"
#' graph_update(g, delete = c(FALSE, TRUE, TRUE, FALSE))
#'
#' # Equivalent way in the "ordered mode" to obtain the updated graph after
#' # deleting the second and third hypotheses
#' # Additional intermediate updated graphs are also provided
#' graph_update(g, delete = 2:3)
graph_update <- function(graph, delete) {
  # Basic type checking
  stopifnot(
    "Please update an `initial_graph` object" = class(graph) == "initial_graph",
    "Hypothesis index must be a logical or integer vector" =
      is.logical(delete) || (all(as.integer(delete) == delete))
  )

  test_graph_names(graph)

  ordered <- !is.logical(delete)

  # Qualitative checking
  if (ordered) {
    stopifnot(
      "Ordered deletion index must contain only valid hypothesis numbers" =
        all(delete > 0 & delete <= length(graph$hypotheses)),
      "Ordered deletion index must have unique values" =
        length(delete) == length(unique(delete))
    )

    intermediate_graphs <- list(graph)
  } else {
    stopifnot(
      "Length of unordered deletion index must match size of graph" =
        length(graph$hypotheses) == length(delete),
      "Unordered deletion index must only contain TRUE or FALSE" =
        all(delete %in% c(TRUE, FALSE))
    )

    delete <- which(delete)
  }

  initial_graph <- graph
  cume_delete <- integer(0)

  # Iterate over the hypotheses to delete
  for (delete_num in delete) {
    cume_delete <- c(cume_delete, delete_num)

    # Save current state of the graph to use in calculations
    # Also make a copy of graph elements for storing new values in
    init_hypotheses <- hypotheses <- graph$hypotheses
    init_transitions <- transitions <- graph$transitions

    hyp_nums <- seq_along(hypotheses)

    # Loop over hypotheses, calculating new weights based on initial hypothesis
    # weights and storing in `hypotheses`
    for (hyp_num in hyp_nums) {
      hypotheses[[hyp_num]] <-
        init_hypotheses[[hyp_num]] +
        init_hypotheses[[delete_num]] * init_transitions[[delete_num, hyp_num]]

      denominator <- 1 - init_transitions[[hyp_num, delete_num]] *
        init_transitions[[delete_num, hyp_num]]

      # In this loop, hyp_num is the starting node of the transition, and
      # end_num is the ending node
      # Calculate new transition weights based on original transition weights,
      # and store in `transitions`
      for (end_num in hyp_nums) {
        if (hyp_num == end_num || denominator <= 0) {
          transitions[[hyp_num, end_num]] <- 0
        } else {
          transitions[[hyp_num, end_num]] <- (
            init_transitions[[hyp_num, end_num]] +
              init_transitions[[hyp_num, delete_num]] *
                init_transitions[[delete_num, end_num]]
          ) / denominator
        }
      }
    }

    # Make sure to zero out the deleted node's values
    hypotheses[delete_num] <- 0
    transitions[delete_num, ] <- 0
    transitions[, delete_num] <- 0

    # At this point, a single hypothesis has been removed from the graph.
    # Assign the newly calculated hypotheses and transitions to `graph`, and
    # loop to the next hypothesis to delete
    graph <- structure(
      list(hypotheses = hypotheses, transitions = transitions),
      class = "initial_graph",
      title = "Updated graph",
      deleted = cume_delete
    )

    if (ordered) intermediate_graphs <- c(intermediate_graphs, list(graph))
  }

  structure(
    list(
      initial_graph = initial_graph,
      updated_graph = graph,
      deleted = delete,
      intermediate_graphs = if (ordered) intermediate_graphs
    ),
    class = "updated_graph"
  )
}
