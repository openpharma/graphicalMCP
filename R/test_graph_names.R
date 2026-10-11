#' Validate that a graph's hypothesis names are present and consistent
#'
#' @description
#' An `initial_graph` object relies on hypothesis names being present on
#' `graph$hypotheses` and on the rows and columns of `graph$transitions`, and
#' on those names being identical across all three. [graph_create()] guarantees
#' this, but because an `initial_graph` is a plain list, a direct assignment
#' such as `graph$hypotheses <- c(0.6, 0.4)` silently drops the names and
#' leaves the object in an invalid state. Downstream functions then fail with
#' errors that give no hint about the real cause (see
#' <https://github.com/openpharma/graphicalMCP/issues/110>).
#'
#' This internal helper checks the names up front so callers can emit an
#' actionable message instead.
#'
#' @param graph An `initial_graph` as returned by [graph_create()].
#' @param warn_only A logical scalar. When `FALSE` (the default) an invalid
#'   graph triggers an error; when `TRUE` it triggers a warning and returns,
#'   which suits display functions such as [print()] and [plot()] that should
#'   still try to show the object.
#'
#' @return Returns `graph` invisibly.
#'
#' @noRd
test_graph_names <- function(graph, warn_only = FALSE) {
  hyp_names <- names(graph$hypotheses)
  row_names <- rownames(graph$transitions)
  col_names <- colnames(graph$transitions)

  msg <- NULL

  if (is.null(hyp_names) || is.null(row_names) || is.null(col_names)) {
    msg <- paste0(
      "`graph` is missing hypothesis names. This usually happens after ",
      "directly assigning an unnamed vector, e.g. ",
      "`graph$hypotheses <- c(0.6, 0.4)`, which drops the names that ",
      "downstream functions rely on. Recreate the graph with ",
      "`graph_create()`, or restore the names, e.g. ",
      "`names(graph$hypotheses) <- rownames(graph$transitions)`."
    )
  } else if (!identical(hyp_names, row_names) ||
    !identical(hyp_names, col_names)) {
    msg <- paste0(
      "`graph` has inconsistent hypothesis names between `hypotheses` and ",
      "the rows and columns of `transitions`. Recreate the graph with ",
      "`graph_create()` so the names stay in sync."
    )
  }

  if (!is.null(msg)) {
    if (warn_only) {
      warning(msg, call. = FALSE)
    } else {
      stop(msg, call. = FALSE)
    }
  }

  invisible(graph)
}
