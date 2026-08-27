#' S3 print method for the class `power_report`
#'
#' @description
#' A printed `power_report` displays the initial graph, testing and simulation
#' options, power outputs, and optional detailed simulations and test results.
#'
#' @param x An object of the class `power_report` to print
#' @inheritParams print.graph_report
#'
#' @return An object x of the class `power_report`, after printing the report of
#'   conducting power simulations based on a graphical multiple comparison
#'   procedure.
#'
#' @rdname print.power_report
#'
#' @export
#'
#' @references
#'   Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
#'   Rohmeyer, K. (2011a). Graphical approaches for multiple comparison
#'   procedures using weighted Bonferroni, Simes, or parametric tests.
#'   \emph{Biometrical Journal}, 53(6), 894-913.
#'
#'   Bretz, F., Maurer, W., and Hommel, G. (2011b). Test and power
#'   considerations for multiple endpoint analyses using sequentially rejective
#'   graphical procedures. \emph{Statistics in Medicine}, 30(13), 1489-1501.
#'
#' @examples
#' # A graphical multiple comparison procedure with two primary hypotheses (H1
#' # and H2) and two secondary hypotheses (H3 and H4)
#' # See Figure 4 in Bretz et al. (2011).
#' alpha <- 0.025
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' delta <- 0.5
#' transitions <- rbind(
#'   c(0, delta, 1 - delta, 0),
#'   c(delta, 0, 0, 1 - delta),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- graph_create(hypotheses, transitions)
#'
#' marginal_power <- c(0.8, 0.8, 0.7, 0.9)
#' corr1 <- matrix(0.5, nrow = 2, ncol = 2)
#' diag(corr1) <- 1
#' corr <- rbind(
#'   cbind(corr1, 0.5 * corr1),
#'   cbind(0.5 * corr1, corr1)
#' )
#' success_fns <- list(
#'   # Probability to reject both H1 and H2
#'   `H1andH2` = function(x) x[1] & x[2],
#'   # Probability to reject both (H1 and H3) or (H2 and H4)
#'   `(H1andH3)or(H2andH4)` = function(x) (x[1] & x[3]) | (x[2] & x[4])
#' )
#' set.seed(1234)
#' # Bonferroni tests
#' # Reduce the number of simulations to save time for package compilation
#' power_output <- graph_calculate_power(
#'   g,
#'   alpha,
#'   sim_corr = corr,
#'   sim_n = 1e2,
#'   power_marginal = marginal_power,
#'   sim_success = success_fns
#' )
print.power_report <- function(x, ..., precision = 4, indent = 2, rows = 10) {
  pad <- paste(rep(" ", indent), collapse = "")
  pad_less_1 <- paste(rep(" ", max(indent - 1, 0)), collapse = "")
  hyp_names <- names(x$inputs$graph$hypotheses)

  # Test input calcs -----------------------------------------------------------
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

  if (!is.null(x$inputs$test_corr)) {
    para_hyps <-
      unlist(x$inputs$test_groups[x$inputs$test_types == "parametric"])
    dimnames(x$inputs$test_corr) <- dimnames(x$inputs$graph$transitions)
    colname_pad <- format(
      "Parametric testing correlation:   ",
      width = max(nchar(rownames(x$inputs$test_corr[para_hyps, para_hyps])))
    )
    label <- paste0(pad_less_1, colname_pad)
    df_corr <- data.frame(
      paste0(pad_less_1, rownames(x$inputs$test_corr)[para_hyps]),
      format(x$inputs$test_corr[para_hyps, para_hyps], digits = precision),
      check.names = FALSE
    )
    names(df_corr)[[1]] <- label
  }

  # Test input print -----------------------------------------------------------
  print(x$inputs$graph, precision = precision, indent = indent)
  cat("\n")
  cat(pad, "Alpha = ", x$inputs$alpha, sep = "")
  cat("\n\n")
  if (!is.null(x$inputs$test_corr)) {
    print(df_corr, row.names = FALSE)
    cat("\n")
  }
  cat(pad, "Test types", "\n", test_spec, sep = "")
  cat("\n")

  # Sim input calcs ------------------------------------------------------------
  cat("\n")
  section_break("Simulation parameters ($inputs)")

  theta_mat <- matrix(
    x$inputs$power_marginal,
    nrow = 1,
    dimnames = list(
      paste0(pad, "Marginal power:"),
      hyp_names
    ),
  )

  dimnames(x$inputs$sim_corr) <- dimnames(x$inputs$graph$transitions)
  colname_pad <- format(
    "Correlation:   ",
    width = max(nchar(rownames(x$inputs$sim_corr)))
  )
  label <- paste0(pad_less_1, colname_pad)
  df_corr <- data.frame(
    paste0(pad_less_1, rownames(x$inputs$sim_corr)),
    format(x$inputs$sim_corr, digits = precision),
    check.names = FALSE
  )
  names(df_corr)[[1]] <- label

  # Sim input print ------------------------------------------------------------
  cat(paste0(
    paste0(pad, "Testing "),
    format(x$inputs$sim_n, scientific = FALSE, big.mark = ","),
    " simulations with multivariate normal params:"
  ))
  cat("\n\n")

  print(as.data.frame(format(theta_mat, digits = precision)))
  cat("\n")
  print(df_corr, row.names = FALSE)

  # Power ----------------------------------------------------------------------
  cat("\n")
  section_break("Power calculation ($power)")

  local_mat <- matrix(
    x$power$power_local,
    nrow = 1,
    dimnames = list(
      paste0(pad, "               Local power:"),
      hyp_names
    ),
  )

  print(as.data.frame(format(local_mat, digits = precision)))
  cat("\n")

  cat(
    pad,
    "Expected no. of rejections: ",
    format(x$power$rejection_expected, digits = precision),
    "\n",
    sep = ""
  )
  cat(
    pad,
    " Power to reject 1 or more: ",
    format(x$power$power_at_least_1, digits = precision),
    "\n",
    sep = ""
  )
  cat(
    pad,
    "       Power to reject all: ",
    format(x$power$power_all, digits = precision),
    "\n",
    sep = ""
  )

  if (!length(x$power$power_success) == 0) {
    cat("\n")

    success_df <- data.frame(
      ` Success measure` = paste0(pad_less_1, names(x$power$power_success)),
      `Power` = format(x$power$power_success, digits = precision),
      check.names = FALSE
    )

    print(success_df, row.names = FALSE)
  }
  cat("\n")

  # Details --------------------------------------------------------------------
  if (!is.null(x$details)) {
    section_break("Simulation details ($details)")

    p_dets <- format(x$details$p_sim, digits = precision)
    colnames(p_dets) <- paste0("p_sim_", hyp_names)
    colnames(p_dets)[[1]] <- paste0(pad_less_1, colnames(p_dets)[[1]])

    test_dets <- x$details$test_results
    colnames(test_dets) <- paste0("rej_", hyp_names)

    max_print_old <- getOption("max.print")
    options(max.print = 99999)

    sim_det_out <- utils::capture.output(
      print(
        utils::head(
          cbind(as.data.frame(p_dets), as.data.frame(test_dets)),
          rows
        ),
        row.names = FALSE
      )
    )
    cat(paste0(pad, sim_det_out), sep = "\n")

    options(max.print = max_print_old)

    if (rows < nrow(p_dets)) {
      cat(pad, "... (Use `print(x, rows = <nn>)` for more)\n\n", sep = "")
    } else {
      cat("\n")
    }
  }

  invisible(x)
}
