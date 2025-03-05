#' Convert between graphicalMCP, gMCP, and igraph graph classes
#'
#' @description Graph objects have different structures and attributes in
#' `graphicalMCP`, `gMCP`, and `igraph` R packages. These functions convert
#' between different classes to increase compatibility.
#'
#' Note that `igraph` and `gMCP` have additional attributes for vertices, edges,
#' or a graph itself. These conversion functions only handle attributes related
#' to hypothesis names, hypothesis weights and transition weights. Other
#' attributes will be dropped when converting.
#'
#' @param graph An `initial_graph` object from the `graphicalMCP` package, a
#'   `graphMCP` object from the `gMCP` package, or an `igraph` object from the
#'   `igraph` package, depending on the conversion type.
#'
#' @return
#'   * `as_graphMCP()` returns a `graphMCP` object for the `gMCP` package.
#'   * `as_igraph()` returns an `igraph` object for the `igraph` package.
#'   * `as_initial_graph()` returns an `initial_graph` object for the
#' `graphicalMCP` package.
#'
#' @seealso [graph_create()] for the initial graph used in the `graphicalMCP`
#' package.
#'
#' @rdname as_graph
#'
#' @export
#'
#' @references Csardi, G., Nepusz, T., Traag, V., Horvat, S., Zanini, F., Noom,
#' D., and Mueller, K. (2024). \emph{igraph}: Network analysis and visualization
#' in R. R package version 2.0.3.
#' \url{https://CRAN.R-project.org/package=igraph}.
#'
#' Rohmeyer, K., and Klinglmueller, K. (2024). \emph{gMCP}: Graph based multiple
#' test procedures. R package version 0.8-17.
#' \url{https://cran.r-project.org/package=gMCP}.
#'
#' @examples
#' g_graphicalMCP <- random_graph(5)
#'
#' if (requireNamespace("gMCP", quietly = TRUE)) {
#'   g_gMCP <- as_graphMCP(g_graphicalMCP)
#'
#'   all.equal(g_graphicalMCP, as_initial_graph(g_gMCP))
#' }
#'
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g_igraph <- as_igraph(g_graphicalMCP)
#'
#'   all.equal(g_graphicalMCP, as_initial_graph(g_igraph))
#' }
as_initial_graph <- function(graph) {
  UseMethod("as_initial_graph", graph)
}

#' @rdname as_graph
#' @export
as_initial_graph.graphMCP <- function(graph) {
  graph_create(graph@weights, graph@m)
}

#' @rdname as_graph
#' @export
as_initial_graph.igraph <- function(graph) {
  hypotheses <- igraph::vertex_attr(graph, "weight")
  names(hypotheses) <- igraph::vertex_attr(graph, "name")

  transitions <- matrix(0, length(hypotheses), length(hypotheses))
  dimnames(transitions) <- rep(list(names(hypotheses)), 2)

  for (tail in seq_along(hypotheses)) {
    transitions[tail, ] <- graph[tail]
  }

  graph_create(hypotheses, transitions)
}

#' @rdname as_graph
#' @export
as_graphMCP <- function(graph) {
  UseMethod("as_graphMCP", graph)
}

#' @rdname as_graph
#' @export
as_graphMCP.initial_graph <- function(graph) {
  if (!requireNamespace("gMCP", quietly = TRUE)) {
    stop("Please install.packages('gMCP') before converting to a gMCP graph")
  } else {
    gMCP::matrix2graph(graph$transitions, graph$hypotheses)
  }
}

#' @rdname as_graph
#' @export
as_igraph <- function(graph) {
  UseMethod("as_igraph", graph)
}

#' @rdname as_graph
#' @export
as_igraph.initial_graph <- function(graph) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Please install.packages('igraph') before converting to an igraph")
  } else {
    num_hyps <- length(graph$hypotheses)
    hyp_names <- names(graph$hypotheses)

    empty_igraph <- igraph::make_empty_graph()

    vertex_igraph <- igraph::add_vertices(
      empty_igraph,
      num_hyps,
      name = hyp_names,
      weight = graph$hypotheses
    )

    matrix_edge_tails <- matrix(rep(hyp_names, num_hyps), nrow = num_hyps)
    matrix_edge_heads <-
      matrix(rep(hyp_names, num_hyps), nrow = num_hyps, byrow = TRUE)

    edge_tails <- matrix_edge_tails[graph$transitions != 0]
    edge_heads <- matrix_edge_heads[graph$transitions != 0]

    vector_edges <- as.vector(rbind(edge_tails, edge_heads))

    complete_igraph <- igraph::add_edges(
      vertex_igraph,
      vector_edges,
      weight = graph$transitions[graph$transitions != 0]
    )

    complete_igraph
  }
}
