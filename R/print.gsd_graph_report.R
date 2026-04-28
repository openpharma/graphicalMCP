#' S3 print method for the class `gsd_graph_report`
#'
#' @description
#' A printed `gsd_graph_report` displays:
#' * **Test parameters**: the initial graph, alpha, information fractions,
#'   p-values, spending functions, and per-hypothesis look_back settings.
#' * **Test summary**: adjusted p-values, rejection decisions, the analysis
#'   at which each decision was made (`Decision.at`), the earliest analysis
#'   at which the boundary was crossed (`First.Rej.at`), look_back status,
#'   and the rejection sequence.
#' * **Per-analysis details** (if `test_values = TRUE`): nominal p-values,
#'   boundaries, and rejection decisions at each analysis. For hypotheses
#'   rejected via look_back, additional rows show the boundary crossing at
#'   earlier analyses, marked with `*` and a footnote.
#' * **Boundary table** (if `verbose = TRUE`): nominal p-value boundaries
#'   for all possible hypothesis weights from the graph's closure, enabling
#'   manual verification of rejection decisions.
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
  p_df[] <- lapply(p_df, function(col) formatC(col, format = "f", digits = precision))
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
  look_back <- x$inputs$look_back
  if (all(look_back == look_back[1])) {
    cat("\n", pad, "Look back = ", look_back[1], "\n", sep = "")
  } else {
    cat("\n", pad, "Look back\n", sep = "")
    for (j in seq_len(num_hyps)) {
      cat(pad, pad, hyp_names[j], ": ", look_back[j], "\n", sep = "")
    }
  }

  # Test summary ---------------------------------------------------------------
  cat("\n")
  section_break("Test summary ($outputs)")

  hyp_width <- max(nchar(c("Hypothesis", hyp_names))) + indent - 1

  adj_p <- x$outputs$adjusted_p
  exceed_1 <- adj_p > 1
  adj_p_format <- character(length(adj_p))
  adj_p_format[exceed_1] <- gsub(".00000001", "+", adj_p[exceed_1])
  adj_p_format[!exceed_1] <- formatC(adj_p[!exceed_1], format = "f",
                                     digits = precision)

  decision_at <- x$outputs$decision_at

  first_rej_display <- ifelse(
    is.na(x$outputs$first_rejected_at),
    "--",
    as.character(x$outputs$first_rejected_at)
  )

  df_summary <- data.frame(
    Hypothesis = formatC(hyp_names, width = hyp_width),
    Adj.P = adj_p_format,
    Reject = x$outputs$rejected,
    Decision.at = as.character(decision_at),
    First.Rej.at = first_rej_display,
    Look.back = look_back,
    check.names = FALSE
  )
  names(df_summary)[[1]] <- formatC("Hypothesis", width = hyp_width)
  names(df_summary)[[2]] <- "Adj.P-value"

  print(df_summary, row.names = FALSE)

  # Rejection sequence
  rej_seq <- x$outputs$rejection_sequence
  if (length(rej_seq) > 0) {
    cat("\n", pad, "Rejection sequence: ",
        paste(rej_seq, collapse = " -> "), "\n", sep = "")
  }
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

      # Check for look_back rows
      has_look_back <- "Look_back" %in% names(detail) && any(detail$Look_back)
      lb_hypotheses <- if (has_look_back) {
        unique(detail$Hypothesis[detail$Look_back])
      } else {
        character(0)
      }

      # Add footnote marker (*) to hypotheses with look_back attribution
      if (has_look_back) {
        detail$Hypothesis[detail$Look_back] <-
          paste0(detail$Hypothesis[detail$Look_back], "*")
      }

      # Remove the Look_back column from display
      detail$Look_back <- NULL

      # Format numeric columns with consistent fixed notation
      detail$Weight <- formatC(detail$Weight, format = "f", digits = precision)
      detail$p <- formatC(detail$p, format = "f", digits = precision)
      detail$Boundary <- formatC(detail$Boundary, format = "f", digits = precision)

      detail_out <- utils::capture.output(
        print(detail, row.names = FALSE)
      )
      cat(paste0(pad, detail_out), sep = "\n")

      # Print footnote for look_back hypotheses
      if (has_look_back) {
        cat(pad, "(*) Rejected via look_back: nominal p-value did not cross",
            " the boundary at the\n", pad, "    current analysis, but",
            " crossed the boundary at an earlier analysis.\n", sep = "")
      }
      cat("\n")
    }
  }

  # Repeated and sequential p-values (verbose) --------------------------------
  if (!is.null(x$boundary_table)) {
    section_break("Repeated p-values ($outputs$repeated_p)")
    rep_p_display <- x$outputs$repeated_p
    rep_p_display[] <- formatC(rep_p_display, format = "f", digits = precision)
    print(as.data.frame(rep_p_display))

    cat("\n")
    section_break("Sequential p-values ($outputs$sequential_p)")
    seq_p_display <- x$outputs$sequential_p
    seq_p_display[] <- formatC(seq_p_display, format = "f", digits = precision)
    print(as.data.frame(seq_p_display))
    cat("\n")
  }

  # Boundary table (verbose) ---------------------------------------------------
  if (!is.null(x$boundary_table)) {
    section_break("Boundary table ($boundary_table)")

    cat(pad, "Nominal p-value boundaries for all possible hypothesis weights\n",
        pad, "from the graph's closure. Use to verify rejection decisions:\n",
        pad, "a hypothesis is rejected when its p-value <= boundary.\n\n",
        sep = "")

    for (hyp in names(x$boundary_table)) {
      cat(pad, hyp, "\n", sep = "")
      bt <- x$boundary_table[[hyp]]
      bt_display <- bt
      # Format numeric columns with consistent fixed notation
      for (col in names(bt_display)) {
        bt_display[[col]] <- formatC(bt_display[[col]],
                                     format = "f", digits = precision)
      }
      bt_out <- utils::capture.output(print(bt_display, row.names = FALSE))
      cat(paste0(pad, bt_out), sep = "\n")
      cat("\n")
    }
  }

  invisible(x)
}
