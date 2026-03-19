#' S3 print method for the class `gsd_graph_report`
#'
#' @description
#' A printed `gsd_graph_report` displays the initial graph, group sequential
#' design parameters, repeated and sequential p-values, adjusted p-values,
#' rejection decisions with the analysis at which each rejection occurred,
#' and optional per-analysis boundary details.
#'
#' @param x An object of class `gsd_graph_report` to print.
#' @param ... Other values passed on to other methods (currently unused).
#' @param precision An integer scalar indicating the number of decimal places
#'   to display.
#' @param indent An integer scalar indicating how many spaces to indent
#'   results.
#'
#' @return An object x of class `gsd_graph_report`, invisibly.
#'
#' @rdname print.gsd_graph_report
#'
#' @export
#'
#' @references
#'   Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
#'   trials using graphical approaches. \emph{Statistics in Biopharmaceutical
#'   Research}, 5(4), 311-320.
#'
#' @examples
#' hypotheses <- c(0.5, 0.5)
#' transitions <- rbind(c(0, 1), c(1, 0))
#' g <- graph_create(hypotheses, transitions)
#'
#' p <- rbind(
#'   H1 = c(0.024, 0.01),
#'   H2 = c(0.015, 0.005)
#' )
#'
#' graph_test_shortcut_gsd(
#'   graph = g,
#'   p = p,
#'   alpha = 0.025,
#'   info_frac = c(0.5, 1),
#'   spending_fn = spending_of
#' )
print.gsd_graph_report <- function(x, ..., precision = 6, indent = 2) {
  pad <- paste(rep(" ", indent), collapse = "")
  hyp_names <- names(x$inputs$graph$hypotheses)
  num_hyps <- length(hyp_names)
  num_analyses <- ncol(x$inputs$p)

  # Input parameters -----------------------------------------------------------
  cat("\n")
  section_break("Test parameters ($inputs)")

  print(x$inputs$graph, precision = precision, indent = indent)
  cat("\n")
  cat(pad, "Alpha = ", x$inputs$alpha, "\n", sep = "")

  # Analysis names from column names of p
  analysis_names <- colnames(x$inputs$p)

  # Information fractions table
  cat("\n", pad, "Information fractions\n", sep = "")
  info_df <- as.data.frame(x$inputs$info_frac, row.names = hyp_names)
  colnames(info_df) <- analysis_names
  print(info_df)

  # P-values table
  cat("\n", pad, "P-values\n", sep = "")
  p_df <- as.data.frame(x$inputs$p, row.names = hyp_names)
  colnames(p_df) <- analysis_names
  p_df[] <- lapply(p_df, function(col) format(col, digits = precision))
  print(p_df)

  # Spending functions
  cat("\n", pad, "Spending functions\n", sep = "")
  for (j in seq_len(num_hyps)) {
    sf_body <- deparse(body(x$inputs$spending_fn[[j]]))
    sf_name <- tryCatch(
      {
        env <- environment(x$inputs$spending_fn[[j]])
        if (identical(x$inputs$spending_fn[[j]], spending_of)) {
          "O'Brien-Fleming"
        } else if (identical(x$inputs$spending_fn[[j]], spending_pocock)) {
          "Pocock"
        } else if (identical(x$inputs$spending_fn[[j]], spending_linear)) {
          "Linear"
        } else {
          paste(sf_body, collapse = " ")
        }
      },
      error = function(e) paste(sf_body, collapse = " ")
    )
    cat(pad, pad, hyp_names[j], ": ", sf_name, "\n", sep = "")
  }

  # Look back mode
  look_back <- isTRUE(x$inputs$look_back)
  cat("\n", pad, "Look back = ", look_back, "\n", sep = "")

  # Test summary ---------------------------------------------------------------
  cat("\n")
  section_break("Test summary ($outputs)")

  hyp_width <- max(nchar(c("Hypothesis", hyp_names))) + indent - 1

  adj_p <- x$outputs$adjusted_p
  exceed_1 <- adj_p > 1
  adj_p_format <- character(length(adj_p))
  adj_p_format[exceed_1] <- gsub(".00000001", "+", adj_p[exceed_1])
  adj_p_format[!exceed_1] <- format(adj_p[!exceed_1], digits = precision)

  rej_at_display <- ifelse(
    is.na(x$outputs$rejected_at),
    "--",
    as.character(x$outputs$rejected_at)
  )

  # Choose which p-values to display based on look_back mode.
  # For rejected hypotheses, show the p-value at the analysis where rejection
  # occurred. For non-rejected hypotheses, show the last non-NA value.
  p_at_decision <- function(mat, rejected_at) {
    vapply(seq_len(nrow(mat)), function(j) {
      if (!is.na(rejected_at[j])) {
        mat[j, rejected_at[j]]
      } else {
        non_na <- which(!is.na(mat[j, ]))
        if (length(non_na) == 0) NA_real_ else mat[j, max(non_na)]
      }
    }, numeric(1))
  }
  if (look_back) {
    p_col_name <- "Seq. P"
    p_col_values <- format(
      p_at_decision(x$outputs$sequential_p, x$outputs$rejected_at),
      digits = precision
    )
    adj_col_name <- "Adj. Seq. P"
  } else {
    p_col_name <- "Rep. P"
    p_col_values <- format(
      p_at_decision(x$outputs$repeated_p, x$outputs$rejected_at),
      digits = precision
    )
    adj_col_name <- "Adj. Rep. P"
  }

  df_summary <- data.frame(
    Hypothesis = formatC(hyp_names, width = hyp_width),
    P = p_col_values,
    `Adj. P` = adj_p_format,
    Reject = x$outputs$rejected,
    `Rejected at` = rej_at_display,
    check.names = FALSE
  )
  names(df_summary)[[1]] <- formatC("Hypothesis", width = hyp_width)
  names(df_summary)[[2]] <- p_col_name
  names(df_summary)[[3]] <- adj_col_name

  print(df_summary, row.names = FALSE)
  cat("\n")

  attr(x$outputs$graph, "title") <-
    "Final updated graph after removing rejected hypotheses"
  print(x$outputs$graph, precision = precision, indent = indent)
  cat("\n")

  # Per-analysis test values ---------------------------------------------------
  if (!is.null(x$test_values)) {
    section_break("Per-analysis details ($test_values)")

    for (k in seq_along(x$test_values)) {
      detail <- x$test_values[[k]]
      if (is.null(detail)) next

      cat(pad, "Analysis ", k, "\n", sep = "")

      # Format numeric columns
      detail$Weight <- format(detail$Weight, digits = precision)
      detail$p <- format(detail$p, digits = precision)
      detail$Boundary <- format(detail$Boundary, digits = precision)

      detail_out <- utils::capture.output(
        print(detail, row.names = FALSE)
      )
      cat(paste0(pad, detail_out), sep = "\n")
      cat("\n")
    }
  }

  invisible(x)
}
