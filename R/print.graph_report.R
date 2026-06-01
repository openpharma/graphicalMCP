#' S3 print method for the class `graph_report`
#'
#' @description
#' A printed `graph_report` displays the initial graph, p-values and
#' significance levels, rejection decisions, and optional detailed test results.
#'
#' @param x An object of class `graph_report` to print.
#' @param ... Other values passed on to other methods (currently unused)
#' @param precision An integer scalar indicating the number of decimal places
#'   to to display.
#' @param indent An integer scalar indicating how many spaces to indent results.
#' @param rows An integer scalar indicating how many rows of detailed test
#' results to print.
#'
#' @return An object x of class `graph_report`, after printing the report of
#'   conducting a graphical multiple comparison procedure.
#'
#' @rdname print.graph_report
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
#' g <- graph_create(hypotheses, transitions)
#'
#' p <- c(0.018, 0.01, 0.105, 0.006)
#' alpha <- 0.025
#' graph_test_shortcut(g, p, alpha)
print.graph_report <- function(x, ..., precision = 4, indent = 2, rows = 10) {
  pad <- paste(rep(" ", indent), collapse = "")
  pad_less_1 <- paste(rep(" ", max(indent - 1, 0)), collapse = "")
  hyp_names <- names(x$inputs$graph$hypotheses)

  # Input calcs ----------------------------------------------------------------
  cat("\n")
  section_break("Test parameters ($inputs)")

  hyp_groups <- lapply(x$inputs$test_groups, function(group) hyp_names[group])
  pad_tests <- formatC(
    x$inputs$test_types,
    width = max(nchar(x$inputs$test_types)) + indent
  )

  test_spec <- paste0(
    pad_tests,
    ": (",
    lapply(hyp_groups, paste, collapse = ", "),
    ")",
    collapse = "\n"
  )

  p_mat <- matrix(
    x$inputs$p,
    nrow = 1,
    dimnames = list(
      paste0(pad, "Unadjusted p-values:"),
      hyp_names
    ),
  )

  if (any(x$inputs$test_types == "parametric")) {
    para_hyps <-
      unlist(x$inputs$test_groups[x$inputs$test_types == "parametric"])

    dimnames(x$inputs$test_corr) <- dimnames(x$inputs$graph$transitions)
    colname_pad <- format(
      "Correlation matrix:   ",
      width = max(nchar(rownames(x$inputs$test_corr)))
    )
    label <- paste0(pad_less_1, colname_pad)
    df_corr <- data.frame(
      paste0(pad_less_1, rownames(x$inputs$test_corr[para_hyps, ])),
      format(x$inputs$test_corr[para_hyps, para_hyps], digits = precision),
      check.names = FALSE
    )
    names(df_corr)[[1]] <- label
  }

  # Input print ----------------------------------------------------------------
  print(x$inputs$graph, precision = precision, indent = indent)
  cat("\n")
  cat(pad, "Alpha = ", x$inputs$alpha, sep = "")
  cat("\n\n")
  print(as.data.frame(format(p_mat, digits = precision)))
  cat("\n")
  if (any(x$inputs$test_types == "parametric")) {
    print(df_corr, row.names = FALSE)
    cat("\n")
  }
  cat(pad, "Test types", "\n", test_spec, sep = "")
  cat("\n")

  # Output ---------------------------------------------------------------------
  cat("\n")
  section_break("Test summary ($outputs)")

  hyp_width <- max(nchar(c("Hypothesis", hyp_names))) + indent - 1

  adjusted_p <- x$outputs$adjusted_p
  exceed_1 <- adjusted_p > 1
  adjusted_p_plus <- gsub(".00000001", "+", adjusted_p[exceed_1])
  adjusted_p_format <- format(adjusted_p[!exceed_1], digits = precision)

  adjusted_p[exceed_1] <- adjusted_p_plus
  adjusted_p[!exceed_1] <- adjusted_p_format

  df_summary <- data.frame(
    Hypothesis = formatC(hyp_names, width = hyp_width),
    Adj.p = adjusted_p,
    Reject = x$outputs$rejected,
    check.names = FALSE
  )
  names(df_summary)[[1]] <- formatC("Hypothesis", width = hyp_width)

  print(df_summary, row.names = FALSE)

  cat("\n")

  attr(x$outputs$graph, "title") <-
    "Final updated graph after removing rejected hypotheses"

  print(
    x$outputs$graph,
    precision = precision,
    indent = indent
  )

  cat("\n")

  # Adjusted p/rejection sequence details --------------------------------------
  if (!is.null(x$details)) {
    if (is.data.frame(x$details$results)) {
      df_details <- x$details$results

      for (col_num in seq_along(df_details)) {
        if (is.numeric(df_details[[col_num]])) {
          df_details[[col_num]] <-
            format(df_details[[col_num]], digits = precision)
        }
      }

      max_print_old <- getOption("max.print")
      options(max.print = 99999)

      section_break("Adjusted p details ($details)")
      detail_results_out <- utils::capture.output(
        print(utils::head(df_details, rows), row.names = FALSE)
      )
      cat(paste0(pad_less_1, detail_results_out), sep = "\n")

      options(max.print = max_print_old)

      if (rows < nrow(df_details)) {
        cat(pad, "... (Use `print(x, rows = <nn>)` for more)\n\n", sep = "")
      } else {
        cat("\n")
      }
    } else {
      graph_seq <- x$details$results
      del_seq <- x$details$del_seq

      section_break("Rejection sequence details ($details)")
      for (i in seq_along(graph_seq) - 1) {
        if (i == 0) {
          print(graph_seq[[i + 1]], precision = precision, indent = indent)
        } else {
          attr(graph_seq[[i + 1]], "title") <- paste0(
            "Step ", i, ": Updated graph after removing ",
            if (i == 1) "hypothesis " else "hypotheses ",
            paste0(del_seq[seq_len(i)], collapse = ", ")
          )

          print(
            graph_seq[[i + 1]],
            precision = precision,
            indent = indent * (i + 1)
          )
        }
        cat("\n")
      }

      attr(graph_seq[[length(graph_seq)]], "title") <-
        "Final updated graph after removing rejected hypotheses"

      print(
        graph_seq[[length(graph_seq)]],
        precision = precision,
        indent = indent
      )
      cat("\n")
    }
  }

  # Test values details --------------------------------------------------------
  if (!is.null(x$test_values)) {
    section_break("Detailed test values ($test_values)")

    if (any(x$inputs$test_types == "parametric")) {
      num_cols <- c("p", "c_value", "Weight", "Alpha")
    } else {
      num_cols <- c("p", "Weight", "Alpha")
    }

    crit_res <- x$test_values$results
    crit_res[num_cols] <- apply(
      crit_res[num_cols],
      2,
      function(num_col) {
        format(as.numeric(num_col), digits = precision)
      }
    )

    p_col_index <- which(names(crit_res) == "p")
    if (any(x$inputs$test_types == "parametric")) {
      crit_res$"c_value" <- ifelse(
        trimws(crit_res$"c_value") == "NA",
        "",
        crit_res$"c_value"
      )

      crit_res <- cbind(
        crit_res[1:p_col_index],
        data.frame("<=" = "<=", check.names = FALSE),
        crit_res[p_col_index + 1],
        data.frame("*" = "*", check.names = FALSE),
        crit_res[p_col_index + 2],
        data.frame("*" = "*", check.names = FALSE),
        crit_res[(p_col_index + 3):(p_col_index + 4)]
      )
    } else {
      crit_res <- cbind(
        crit_res[1:p_col_index],
        data.frame("<=" = "<=", check.names = FALSE),
        crit_res[p_col_index + 1],
        data.frame("*" = "*", check.names = FALSE),
        crit_res[(p_col_index + 2):(p_col_index + 3)]
      )
    }

    max_print_old <- getOption("max.print")
    options(max.print = 99999)

    test_values_results_out <- utils::capture.output(
      print(utils::head(crit_res, rows), row.names = FALSE)
    )
    cat(paste0(pad_less_1, test_values_results_out), sep = "\n")

    options(max.print = max_print_old)

    if (rows < nrow(crit_res)) {
      cat(pad, "... (Use `print(x, rows = <nn>)` for more)\n\n", sep = "")
    } else {
      cat("\n")
    }
  }

  # Optional alternate orderings
  if (!is.null(x$valid_orderings)) {
    section_break("Alternate rejection orderings ($valid_rejection_orderings)")

    lapply(
      x$valid_orderings,
      function(ordering) {
        print(ordering)
        cat("\n")
      }
    )
  }

  invisible(x)
}

section_break <- function(text) {
  cat(text, " ", rep("-", 79 - nchar(text)), "\n", sep = "")
}
