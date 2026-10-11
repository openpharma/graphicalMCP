#' S3 print method for the class `initial_graph`
#'
#' @description
#' A printed `initial_graph` displays a header stating "Initial graph",
#' hypothesis weights, and transition weights.
#'
#' @param x An object of class `initial_graph` to print.
#' @param ... Other values passed on to other methods (currently unused).
#' @param precision An integer scalar indicating the number of decimal places
#'   to to display.
#' @param indent An integer scalar indicating how many spaces to indent results.
#'
#' @return An object x of class `initial_graph`, after printing the initial
#'   graph.
#'
#' @seealso
#'   [print.updated_graph()] for the print method for the updated graph after
#'   hypotheses being deleted from the initial graph.
#'
#' @rdname print.initial_graph
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
#' hyp_names <- c("H11", "H12", "H21", "H22")
#' g <- graph_create(hypotheses, transitions, hyp_names)
#' g
print.initial_graph <- function(x,
                                ...,
                                precision = 4,
                                indent = 0) {
  test_graph_names(x, warn_only = TRUE)

  x$hypotheses[attr(x, "deleted")] <-
    x$transitions[attr(x, "deleted"), ] <-
    x$transitions[, attr(x, "deleted")] <-
    NA

  if (is.null(attr(x, "title"))) attr(x, "title") <- "Initial graph"

  pad <- paste(rep(" ", indent), collapse = "")
  pad_less_1 <- paste(rep(" ", max(indent - 1, 0)), collapse = "")

  cat(paste0(pad, attr(x, "title"), "\n\n"))

  cat(paste0(pad, "--- Hypothesis weights ---\n"))

  hypotheses_text <- paste(
    pad,
    formatC(
      names(x$hypotheses),
      width = max(nchar(names(x$hypotheses)))
    ),
    ": ",
    format(x$hypotheses, digits = precision),
    sep = "",
    collapse = "\n"
  )

  cat(hypotheses_text, "", sep = "\n")

  cat(paste0(pad, "--- Transition weights ---\n"))

  transitions <- format(
    x$transitions,
    digits = precision,
    scientific = FALSE
  )

  colname_pad <- format("", width = max(nchar(rownames(transitions))))
  label <- paste0(pad_less_1, colname_pad)
  df_trn <- data.frame(
    paste0(pad_less_1, rownames(transitions)),
    transitions,
    check.names = FALSE
  )
  names(df_trn)[[1]] <- label

  transitions_text <- data.frame(df_trn, check.names = FALSE)

  print(transitions_text, row.names = FALSE)

  invisible(x)
}
