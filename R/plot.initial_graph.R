#' S3 plot method for class `initial_graph`
#'
#' @description The plot of an `initial_graph` translates the `hypotheses` into
#' vertices and `transitions` into edges to create a network plot. Vertices are
#' labeled with hypothesis names and hypothesis weights, and edges are labeled
#' with transition weights. See `vignette("graph-examples")` for more
#' illustration of commonly used multiple comparison procedure using graphs.
#'
#' @param x An object of class `initial_graph` to plot.
#' @param ... Other arguments passed on to `igraph::plot.igraph()`.
#' @param v_palette A character vector of length two specifying the colors for
#'   retained and deleted hypotheses. More extensive color customization must be
#'   done with `vertex.color`.
#' @param layout An igraph layout specification (See `?igraph.plotting`), or
#'   `"grid"`, which lays out hypotheses left-to-right and top-to-bottom. `nrow`
#'   and `ncol` control the grid shape.
#' @param nrow An integer scalar specifying the number of rows in the vertex
#'   grid. If row and column counts are not specified, vertices will be laid out
#'   as close to a square as possible.
#' @param ncol An integer scalar specifying the number of columns in the vertex
#'   grid. If row and column counts are not specified, vertices will be laid out
#'   as close to a square as possible.
#' @param edge_curves A named numeric vector specifying the curvature of
#'   specific edges. Edge pairs (Where two vertices share an edge in each
#'   possible direction) are detected automatically and get 0.25 curvature.
#'   Adjust edges by adding an entry with name `"vertex1|vertex2`, and adjust
#'   default edge pairs curvature by adding an entry with name `"pairs"` -
#'   `edge_curves = c("pairs" = 0.5, "H1|H3" = 0.25, "H3|H4" = 0.75)`.
#' @param precision An integer scalar indicating the number of decimal places to
#'   to display.
#' @param eps A numeric scalar. The transition weight of `eps` will be displayed
#'   as \eqn{\epsilon}, which indicates edges with infinitesimally small
#'   weights. See Bretz et al. (2009) for more details.
#' @param background_color A character scalar specifying a background color for
#'   the whole plotting area. Passed directly to [graphics::par()] (`bg`).
#' @param margins A length 4 numeric vector specifying the margins for the plot.
#'   Defaults to all 0, since igraph plots tend to have large margins. It is
#'   passed directly to [graphics::par()] (`mar`).
#'
#' @return An object x of class `initial_graph`, after plotting the initial
#'   graph.
#'
#' @section Customization of graphs: There are a few values for
#'   [igraph::plot.igraph()] that get their defaults changed for graphicalMCP.
#'   These values can still be changed by passing them as arguments to
#'   `plot.initial_graph()`. Here are the new defaults:
#'   * `vertex.color = "#6baed6"`,
#'   * `vertex.label.color = "black"`,
#'   * `vertex.size = 20`,
#'   * `edge.arrow.size = 1`,
#'   * `edge.arrow.width = 1`,
#'   * `edge.label.color = "black"`
#'   * `asp = 0`.
#'
#'   Neither `graphicalMCP` nor `igraph` does anything about overlapping edge
#'   labels. If you run into this problem, and vertices can't practically be
#'   moved enough to avoid collisions of edge labels, using edge curves can
#'   help. `igraph` puts edge labels closer to the tail of an edge when an edge
#'   is straight, and closer to the head of an edge when it's curved. By setting
#'   an edge's curve to some very small value, an effectively straight edge can
#'   be shifted to a new position.
#'
#' @seealso [plot.updated_graph()] for the plot method for the updated graph
#' after hypotheses being deleted from the initial graph.
#'
#' @rdname plot.initial_graph
#'
#' @export
#'
#' @references Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W.,
#' and Rohmeyer, K. (2011). Graphical approaches for multiple comparison
#' procedures using weighted Bonferroni, Simes, or parametric tests.
#' \emph{Biometrical Journal}, 53(6), 894-913.
#'
#' Xi, D., and Bretz, F. (2019). Symmetric graphs for equally weighted tests,
#' with application to the Hochberg procedure. \emph{Statistics in Medicine},
#' 38(27), 5268-5282.
#'
#' @examples
#' # A graphical multiple comparison procedure with two primary hypotheses (H1
#' # and H2) and two secondary hypotheses (H3 and H4)
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
#' plot(g)
#'
#' # A graphical multiple comparison procedure with two primary hypotheses (H1
#' # and H2) and four secondary hypotheses (H31, H32, H41, and H42)
#' # See Figure 6 in Xi and Bretz (2019).
#' hypotheses <- c(0.5, 0.5, 0, 0, 0, 0)
#' epsilon <- 1e-5
#' transitions <- rbind(
#'   c(0, 0.5, 0.25, 0, 0.25, 0),
#'   c(0.5, 0, 0, 0.25, 0, 0.25),
#'   c(0, 0, 0, 0, 1, 0),
#'   c(epsilon, 0, 0, 0, 0, 1 - epsilon),
#'   c(0, epsilon, 1 - epsilon, 0, 0, 0),
#'   c(0, 0, 0, 1, 0, 0)
#' )
#' hyp_names <- c("H1", "H2", "H31", "H32", "H41", "H42")
#' g <- graph_create(hypotheses, transitions, hyp_names)
#'
#' plot_layout <- rbind(
#'   c(0.15, 0.5),
#'   c(0.65, 0.5),
#'   c(0, 0),
#'   c(0.5, 0),
#'   c(0.3, 0),
#'   c(0.8, 0)
#' )
#'
#' plot(g, layout = plot_layout, eps = epsilon, edge_curves = c(pairs = .5))
plot.initial_graph <- function(x,
                               ...,
                               v_palette = c("#6baed6", "#cccccc"),
                               layout = "grid",
                               nrow = NULL,
                               ncol = NULL,
                               edge_curves = NULL,
                               precision = 4,
                               eps = NULL,
                               background_color = "white",
                               margins = c(0, 0, 0, 0)) {
  oldpar <- graphics::par("bg", "mar")
  on.exit(suppressWarnings(graphics::par(oldpar)))

  if (length(v_palette) != 2) {
    stop("Choose 2 palette colors or use `vertex.color` for more customization")
  }

  graph_size <- length(x$hypotheses)
  graph_seq <- seq_along(x$hypotheses)

  graph_igraph <- as_igraph(x)

  v_attr <- igraph::vertex_attr(graph_igraph)
  e_attr <- igraph::edge_attr(graph_igraph)

  # Vertex colors --------------------------------------------------------------
  v_color <- rep(v_palette[[1]], length(x$hypotheses))
  v_color[attr(x, "deleted")] <- v_palette[[2]]

  # Make labels ----------------------------------------------------------------
  v_labels <- paste(v_attr$name, round(v_attr$weight, precision), sep = "\n")

  # Very small edges should display as epsilon
  edge_labels <- e_attr$weight
  if (is.null(edge_labels)) edge_labels <- numeric(0)

  near_0 <- edge_labels <= eps & edge_labels != 0
  near_1 <- edge_labels >= 1 - eps & edge_labels != 1

  if (length(near_0) == 0 || length(near_1) == 0) {
    edge_labels <- round(edge_labels, precision)
  } else {
    edge_labels[!near_0 & !near_1] <-
      round(edge_labels[!near_0 & !near_1], precision)
  }

  if (!is.null(eps)) {
    edge_labels[near_0] <- expression(epsilon)
    edge_labels[near_1] <- expression(1 - epsilon)
  }

  # Set curves -----------------------------------------------------------------
  curve <- rep(0, length(igraph::E(graph_igraph)))
  names(curve) <- attr(igraph::E(graph_igraph), "vnames")

  # Vertex pairs connected in both directions should get a small default so
  # their edges don't overlap each other
  if (!is.null(edge_curves)) {
    if (!is.na(edge_curves["pairs"])) {
      edge_pair_curve <- edge_curves["pairs"]
    } else {
      edge_pair_curve <- .25
    }
  } else {
    edge_pair_curve <- .25
  }

  edge_pair_locs <-
    attr(igraph::E(graph_igraph), "vnames") %in% edge_pairs(x)

  curve[edge_pair_locs] <- edge_pair_curve

  curve[names(edge_curves)] <- edge_curves

  # Set layout -----------------------------------------------------------------
  if (!is.function(layout)) {
    if (!is.matrix(layout)) {
      if (layout == "grid") {
        if (is.null(nrow) && is.null(ncol)) {
          nrow <- ceiling(sqrt(graph_size))
          ncol <- nrow
        } else if (is.null(nrow)) {
          nrow <- ceiling(graph_size / ncol)
        } else if (is.null(ncol)) {
          ncol <- ceiling(graph_size / nrow)
        }

        # [] removes extras when grid is not filled all the way
        layout <- cbind(
          rep(seq_len(ncol), nrow)[graph_seq],
          vapply(rev(seq_len(nrow)), rep, integer(ncol), ncol)[graph_seq]
        )
      }
    }
  }

  graphics::par(mar = margins)
  graphics::par(bg = background_color)

  # Draw! ----------------------------------------------------------------------
  igraph::plot.igraph(
    graph_igraph,
    ...,
    layout = layout,
    vertex.color = v_color,
    vertex.label = v_labels,
    vertex.label.color = "black",
    vertex.size = 20,
    edge.label = edge_labels,
    edge.label.color = "black",
    edge.curved = curve,
    edge.arrow.size = 1,
    edge.arrow.width = 1,
    asp = 0
  )

  invisible(x)
}
