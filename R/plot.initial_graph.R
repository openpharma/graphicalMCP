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
#' @param layout An igraph layout specification (See `?igraph.plotting`),
#'   `"grid"`, which lays out hypotheses left-to-right and top-to-bottom, or
#'   `"ellipse"`, which lays out hypotheses clockwise around an ellipse.
#'   `nrow` and `ncol` control the grid shape. `xradius`, `yradius`,
#'   `radian_start`, and `clockwise` control the ellipse shape.
#' @param nrow An integer scalar specifying the number of rows in the vertex
#'   grid. If row and column counts are not specified, vertices will be laid out
#'   as close to a square as possible.
#' @param ncol An integer scalar specifying the number of columns in the vertex
#'   grid. If row and column counts are not specified, vertices will be laid out
#'   as close to a square as possible.
#' @param xradius A positive numeric scalar specifying the horizontal radius of
#'   the vertex layout ellipse when `layout = "ellipse"`.
#' @param yradius A positive numeric scalar specifying the vertical radius of
#'   the vertex layout ellipse when `layout = "ellipse"`.
#' @param radian_start A numeric scalar specifying the starting angle, in
#'   radians, for the first hypothesis when `layout = "ellipse"`. If `NULL`,
#'   the default follows `gMCPLite::hGraph()`.
#' @param clockwise A logical scalar indicating whether hypotheses should be
#'   placed clockwise around the ellipse when `layout = "ellipse"`.
#' @param edge_curves A named numeric vector specifying the curvature of
#'   specific edges. Edge pairs (where two vertices share an edge in each
#'   possible direction) are detected automatically and get 0.25 curvature.
#'   Adjust edges by adding an entry with name `"vertex1|vertex2"`, and adjust
#'   default edge pairs curvature by adding an entry with name `"pairs"`.
#'   Adjust the base curvature for non-paired edges by adding an entry with name
#'   `"default"`, or by passing an unnamed numeric scalar. Paired edges keep the
#'   0.25 default unless `"pairs"` is also supplied -
#'   `edge_curves = c("default" = 0.1, "pairs" = 0.5, "H1|H3" = 0.25)`.
#' @param precision An integer scalar indicating the number of decimal places to
#'   display.
#' @param eps A numeric scalar. The transition weight of `eps` will be displayed
#'   as \eqn{\epsilon}, which indicates edges with infinitesimally small
#'   weights. See Bretz et al. (2009) for more details.
#' @param background_color A character scalar specifying a background color for
#'   the whole plotting area. Passed directly to [graphics::par()] (`bg`).
#' @param margins A length 4 numeric vector specifying the margins for the plot.
#'   Defaults to all 1, since igraph plots tend to have large margins. It is
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
#'   When `layout = "ellipse"`, the vertices are drawn with an internal ellipse
#'   shape by default. Use `vertex.size` and `vertex.size2` to control the
#'   ellipse width and height. Ellipse layouts use `rescale = FALSE` and
#'   `asp = 1` by default so that `xradius` and `yradius` are respected.
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
#'
#' plot(
#'   g,
#'   layout = "ellipse",
#'   xradius = 2,
#'   yradius = 1,
#'   vertex.size = 30,
#'   vertex.size2 = 20,
#'   edge_curves = c(default = .1, pairs = .3),
#'   eps = epsilon
#' )
plot.initial_graph <- function(x,
                               ...,
                               v_palette = c("#6baed6", "#cccccc"),
                               layout = "grid",
                               nrow = NULL,
                               ncol = NULL,
                               xradius = 2,
                               yradius = xradius,
                               radian_start = NULL,
                               clockwise = TRUE,
                               edge_curves = NULL,
                               precision = 4,
                               eps = NULL,
                               background_color = "white",
                               margins = c(1, 1, 1, 1)) {
  oldpar <- graphics::par("bg", "mar")
  on.exit(suppressWarnings(graphics::par(oldpar)))

  if (length(v_palette) != 2) {
    stop("Choose 2 palette colors or use `vertex.color` for more customization")
  }

  graph_size <- length(x$hypotheses)
  graph_seq <- seq_along(x$hypotheses)
  plot_args <- list(...)

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
  curve <- plot_edge_curves(graph_igraph, x, edge_curves)

  # Set layout -----------------------------------------------------------------
  ellipse_layout <- is.character(layout) &&
    length(layout) == 1 &&
    identical(layout, "ellipse")

  if (!is.function(layout) && !is.matrix(layout)) {
    if (is.character(layout) && length(layout) == 1 && layout == "grid") {
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
    } else if (ellipse_layout) {
      layout <- graph_ellipse_layout(
        graph_size,
        xradius = xradius,
        yradius = yradius,
        radian_start = radian_start,
        clockwise = clockwise
      )
      register_graphicalMCP_ellipse_shape()
    }
  }

  graphics::par(mar = margins)
  graphics::par(bg = background_color)

  plot_args <- set_plot_arg_default(plot_args, "layout", layout)
  plot_args <- set_plot_arg_default(plot_args, "vertex.color", v_color)
  plot_args <- set_plot_arg_default(plot_args, "vertex.label", v_labels)
  plot_args <- set_plot_arg_default(plot_args, "vertex.label.color", "black")
  plot_args <- set_plot_arg_default(plot_args, "vertex.size", 20)
  plot_args <- set_plot_arg_default(plot_args, "edge.label", edge_labels)
  plot_args <- set_plot_arg_default(plot_args, "edge.label.color", "black")
  plot_args <- set_plot_arg_default(plot_args, "edge.curved", curve)
  plot_args <- set_plot_arg_default(plot_args, "edge.arrow.size", 1)
  plot_args <- set_plot_arg_default(plot_args, "edge.arrow.width", 1)
  plot_args <- set_plot_arg_default(plot_args, "asp", if (ellipse_layout) 1 else 0)

  if (ellipse_layout) {
    vertex_sizes <- unlist(plot_args[c("vertex.size", "vertex.size2")])
    ellipse_margin <- if (is.numeric(vertex_sizes)) {
      max(vertex_sizes, na.rm = TRUE) / 200
    } else {
      20 / 200
    }

    plot_args <- set_plot_arg_default(
      plot_args,
      "vertex.shape",
      "graphicalMCP_ellipse"
    )
    plot_args <- set_plot_arg_default(
      plot_args,
      "vertex.size2",
      plot_args[["vertex.size"]]
    )
    plot_args <- set_plot_arg_default(plot_args, "rescale", FALSE)
    plot_args <- set_plot_arg_default(plot_args, "margin", ellipse_margin)
  }

  # Draw! ----------------------------------------------------------------------
  do.call(igraph::plot.igraph, c(list(x = graph_igraph), plot_args))

  invisible(x)
}

set_plot_arg_default <- function(plot_args, name, value) {
  if (is.null(plot_args[[name]])) {
    plot_args[[name]] <- value
  }

  plot_args
}

plot_edge_curves <- function(graph_igraph, graph, edge_curves = NULL) {
  if (!is.null(edge_curves) && !is.numeric(edge_curves)) {
    stop("`edge_curves` must be a numeric vector")
  }

  curve <- rep(0, length(igraph::E(graph_igraph)))
  names(curve) <- attr(igraph::E(graph_igraph), "vnames")

  if (length(curve) == 0) {
    return(curve)
  }

  if (is.null(edge_curves)) {
    edge_curves <- numeric(0)
    edge_curve_names <- character(0)
  } else {
    edge_curve_names <- names(edge_curves)
  }

  if (is.null(edge_curve_names)) {
    if (length(edge_curves) == 1) {
      curve[] <- edge_curves
    }

    edge_curve_names <- rep("", length(edge_curves))
  }

  default_curve <- edge_curves[edge_curve_names == "default"]

  if (length(default_curve) > 0 && !is.na(default_curve[[1]])) {
    curve[] <- default_curve[[1]]
  }

  # Vertex pairs connected in both directions should get a small default so
  # their edges don't overlap each other.
  edge_pair_curve <- .25
  edge_pair_curve_input <- edge_curves[edge_curve_names == "pairs"]

  if (length(edge_pair_curve_input) > 0 && !is.na(edge_pair_curve_input[[1]])) {
    edge_pair_curve <- edge_pair_curve_input[[1]]
  }

  edge_pair_locs <-
    attr(igraph::E(graph_igraph), "vnames") %in% edge_pairs(graph)

  curve[edge_pair_locs] <- edge_pair_curve

  edge_specific_locs <-
    !(edge_curve_names %in% c("", "default", "pairs")) &
      edge_curve_names %in% names(curve)

  curve[edge_curve_names[edge_specific_locs]] <-
    edge_curves[edge_specific_locs]

  curve
}

graph_ellipse_layout <- function(graph_size,
                                 xradius = 2,
                                 yradius = xradius,
                                 radian_start = NULL,
                                 clockwise = TRUE) {
  if (!is.numeric(xradius) || length(xradius) != 1 || xradius <= 0) {
    stop("`xradius` must be a positive numeric scalar")
  }

  if (!is.numeric(yradius) || length(yradius) != 1 || yradius <= 0) {
    stop("`yradius` must be a positive numeric scalar")
  }

  if (!is.null(radian_start)) {
    if (!is.numeric(radian_start) || length(radian_start) != 1) {
      stop("`radian_start` must be `NULL` or a numeric scalar")
    }
  } else {
    radian_start <- if (graph_size %% 2 != 0) {
      pi * (1 / 2 + 1 / graph_size)
    } else {
      pi * (1 + 2 / graph_size) / 2
    }
  }

  if (!is.logical(clockwise) || length(clockwise) != 1 || is.na(clockwise)) {
    stop("`clockwise` must be a logical scalar")
  }

  direction <- if (clockwise) -1 else 1
  radians <- radian_start +
    direction * (seq_len(graph_size) - 1) / graph_size * 2 * pi

  cbind(
    xradius * cos(radians),
    yradius * sin(radians)
  )
}

register_graphicalMCP_ellipse_shape <- function() {
  if (!"graphicalMCP_ellipse" %in% igraph::shapes()) {
    igraph::add_shape(
      "graphicalMCP_ellipse",
      clip = graphicalMCP_ellipse_clip,
      plot = graphicalMCP_ellipse_plot,
      parameters = list(vertex.size2 = 15)
    )
  }

  invisible(TRUE)
}

graphicalMCP_ellipse_plot <- function(coords, v = NULL, params) {
  if (length(coords) == 0) {
    return(invisible(NULL))
  }

  vertex_color <- plot_vertex_param(params, "color", coords, v)
  vertex_frame_color <- plot_vertex_param(params, "frame.color", coords, v)
  vertex_frame_width <- plot_vertex_param(params, "frame.width", coords, v)
  vertex_size <- plot_vertex_param(params, "size", coords, v)
  vertex_size2 <- plot_vertex_param(params, "size2", coords, v)

  vertex_frame_color[vertex_frame_width <= 0] <- NA
  vertex_frame_width[vertex_frame_width <= 0] <- 1

  theta <- seq(0, 2 * pi, length.out = 73)

  invisible(mapply(
    function(x, y, color, frame_color, frame_width, size, size2) {
      graphics::polygon(
        x + size * cos(theta),
        y + size2 * sin(theta),
        col = color,
        border = frame_color,
        lwd = frame_width
      )
    },
    coords[, 1],
    coords[, 2],
    vertex_color,
    vertex_frame_color,
    vertex_frame_width,
    vertex_size,
    vertex_size2
  ))
}

plot_vertex_param <- function(params, name, coords, v = NULL) {
  value <- params("vertex", name)

  if (length(value) != 1 && !is.null(v)) {
    value <- value[v]
  }

  rep(value, length.out = nrow(coords))
}

graphicalMCP_ellipse_clip <- function(coords,
                                      el,
                                      params,
                                      end = c("both", "from", "to")) {
  end <- match.arg(end)

  if (length(coords) == 0) {
    return(coords)
  }

  if (end %in% c("from", "both")) {
    res_from <- ellipse_clip_points(
      coords[, 1],
      coords[, 2],
      coords[, 3],
      coords[, 4],
      vertex_radii(params, "size", el[, 1]),
      vertex_radii(params, "size2", el[, 1])
    )
  }

  if (end %in% c("to", "both")) {
    res_to <- ellipse_clip_points(
      coords[, 3],
      coords[, 4],
      coords[, 1],
      coords[, 2],
      vertex_radii(params, "size", el[, 2]),
      vertex_radii(params, "size2", el[, 2])
    )
  }

  if (end == "from") {
    res_from
  } else if (end == "to") {
    res_to
  } else {
    cbind(res_from, res_to)
  }
}

vertex_radii <- function(params, name, vertices) {
  radii <- params("vertex", name)

  if (length(radii) == 1) {
    radii <- rep(radii, length(vertices))
  } else {
    radii <- rep(radii, length.out = max(vertices))[vertices]
  }

  pmax(radii, .Machine$double.eps)
}

ellipse_clip_points <- function(center_x,
                                center_y,
                                other_x,
                                other_y,
                                xradius,
                                yradius) {
  dx <- other_x - center_x
  dy <- other_y - center_y
  denom <- sqrt((dx / xradius)^2 + (dy / yradius)^2)

  cbind(
    center_x + ifelse(denom == 0, 0, dx / denom),
    center_y + ifelse(denom == 0, 0, dy / denom)
  )
}
