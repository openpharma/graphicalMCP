#' Perform shortcut graphical multiple comparison procedures with group
#' sequential designs
#'
#' @description
#' Extends [graph_test_shortcut()] to group sequential designs where hypotheses
#' can be tested at multiple analyses (interim and final). At each analysis,
#' the significance level available for each hypothesis is determined by a
#' spending function evaluated at the information fraction. The group
#' sequential boundaries (critical values) are computed from the spending using
#' the joint distribution of test statistics across analyses.
#'
#' The procedure supports two modes
#' controlled by the `look_back` parameter:
#' * **`look_back = FALSE`** (default): At each analysis, rejection decisions
#'   are based on **repeated p-values** at the current analysis only. A
#'   repeated p-value at analysis \eqn{k} is the minimum significance level at
#'   which the group sequential boundary at analysis \eqn{k} would be crossed.
#'   The graphical shortcut procedure ([graph_test_shortcut()]) is applied at
#'   each analysis using repeated p-values, and the graph is updated before
#'   proceeding to the next analysis.
#' * **`look_back = TRUE`**: Rejection decisions are based on **sequential
#'   p-values**, which consider all analyses up to the current one. A
#'   sequential p-value is the minimum of repeated p-values across all
#'   analyses conducted so far. Like the default mode, the procedure
#'   processes analyses sequentially, applying [graph_test_shortcut()] at
#'   each analysis using sequential p-values and updating the graph before
#'   proceeding. When a hypothesis becomes testable at a later analysis
#'   (via graph update), its `first_rejected_at` is set to the earliest
#'   analysis where its boundary was crossed, while `decision_at` records
#'   the analysis where the rejection was operationally processed.
#'
#' @inheritParams graph_test_shortcut
#' @param p A numeric matrix of p-values with \eqn{m} rows (hypotheses) and
#'   \eqn{K} columns (analyses), where \eqn{K} is the maximum number of
#'   analyses across all hypotheses. For hypotheses not tested at every
#'   analysis, use `NA` for the columns without data. Each hypothesis
#'   must have at least one non-`NA` value.
#' @param info_frac Information fractions at each analysis. Can be:
#'   * A numeric vector of length \eqn{K} — same fractions for all hypotheses.
#'     Only allowed when `p` contains no `NA` values (i.e., all hypotheses
#'     have the same number of analyses).
#'   * A numeric matrix with \eqn{m} rows (hypotheses) and \eqn{K} columns
#'     (analyses) — different fractions per hypothesis. When `p` contains
#'     `NA` padding, `info_frac` must be a matrix with `NA` in the same
#'     positions as `p`.
#'
#'   Non-`NA` values must be in (0, 1] and monotonically non-decreasing per
#'   hypothesis. The last non-`NA` value does not need to be 1, allowing
#'   the procedure to be applied up to an interim analysis.
#' @param spending_fn Spending function(s) for computing group sequential
#'   boundaries. Can be:
#'   * A single function — applied to all hypotheses.
#'   * A list of \eqn{m} functions — one per hypothesis.
#'
#'   Each function must accept two arguments: `alpha` (significance level)
#'   and `info_frac` (information fraction), and return the cumulative alpha
#'   spent. Built-in options include [spending_of()], [spending_pocock()],
#'   [spending_hsd()], and [spending_linear()].
#' @param look_back A logical scalar or vector controlling the testing strategy.
#'   Can be:
#'   * A single logical — applied to all hypotheses.
#'   * A logical vector of length \eqn{m} — one per hypothesis, allowing
#'     different strategies for different hypotheses.
#'
#'   For hypotheses with `look_back = FALSE` (the default), rejection decisions
#'   at each analysis are based on repeated p-values at that analysis only.
#'   For hypotheses with `look_back = TRUE`, rejection decisions are based on
#'   sequential p-values which consider all analyses up to the current one.
#'   The `look_back = TRUE` option can lead to additional rejections because
#'   a hypothesis may have crossed its boundary at an earlier analysis but
#'   only becomes testable (via graph update) at a later analysis.
#' @param test_values A logical scalar specifying whether to include the
#'   per-analysis rejection details in results. When `test_values = TRUE`, the
#'   rejection sequence, analysis at which each rejection occurred, and the
#'   nominal p-value boundaries are reported. The default is
#'   `test_values = FALSE`.
#' @param verbose A logical scalar specifying whether to include the boundary
#'   table in results. When `verbose = TRUE`, a table of nominal p-value
#'   boundaries is computed for each hypothesis at all possible weights from
#'   the graph's closure (via [graph_generate_weights()]). This enables manual
#'   verification of rejection decisions. The default is `FALSE`.
#'
#' @return An S3 object of class `gsd_graph_report` with a list of elements:
#'   * `inputs` - Input parameters, including the initial graph, p-values,
#'     alpha, information fractions, and spending functions.
#'   * `outputs` - Output parameters:
#'     * `repeated_p` - An m x K matrix of repeated p-values at each analysis,
#'     * `sequential_p` - An m x K matrix of sequential p-values (cumulative
#'       minimum of repeated p-values),
#'     * `adjusted_p` - Adjusted p-values from the shortcut procedure
#'       (adjusted repeated p-values when `look_back = FALSE`, adjusted
#'       sequential p-values when `look_back = TRUE`),
#'     * `rejected` - Logical vector of rejection decisions,
#'     * `decision_at` - Integer vector indicating the analysis at which each
#'       hypothesis's decision was made. For rejected hypotheses, this is
#'       the analysis where the rejection was operationally processed. For
#'       non-rejected hypotheses, this is the last analysis where the
#'       hypothesis was tested,
#'     * `first_rejected_at` - Integer vector indicating the earliest analysis
#'       at which each hypothesis's boundary was crossed. For non-rejected
#'       hypotheses, this is `NA`. When `look_back = TRUE`, this may be
#'       earlier than `decision_at` if a hypothesis crossed its boundary at
#'       a prior analysis but only became testable at a later analysis,
#'     * `rejection_sequence` - Character vector giving the order in which
#'       hypotheses were rejected across all analyses,
#'     * `graph` - Updated graph after removing all rejected hypotheses.
#'   * `test_values` - Per-analysis details (if `test_values = TRUE`). A list
#'     of length \eqn{K} (one entry per analysis). Each entry is a data frame
#'     containing the hypothesis name, current weight, observed p-value,
#'     nominal boundary, and rejection decision at that analysis. Entries are
#'     `NULL` for analyses where no hypotheses are active. When
#'     `look_back = TRUE` and a hypothesis is rejected at an earlier analysis,
#'     additional rows show the nominal p-value and boundary at each prior
#'     analysis, with a `Look_back` column indicating these rows.
#'   * `boundary_table` - Boundary lookup table (if `verbose = TRUE`). A named
#'     list with one data frame per hypothesis. Each data frame contains the
#'     columns `Weight`, `Alpha.Allocated`, and `Boundary.k` for each
#'     analysis \eqn{k}, showing the nominal p-value boundary at each analysis
#'     for every possible weight from the graph's closure. This table is
#'     independent of observed p-values and can be used to manually verify
#'     rejection decisions.
#'
#' @seealso
#'   [graph_test_shortcut()] for the fixed-sample (non-sequential) shortcut
#'   procedure, [sequential_p()] for computing sequential p-values,
#'   [repeated_p()] for computing repeated p-values,
#'   [spending_of()], [spending_pocock()], [spending_hsd()],
#'   [spending_linear()] for spending functions.
#'
#' @rdname graph_test_shortcut_gsd
#'
#' @export
#'
#' @references
#'   Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
#'   trials using graphical approaches. \emph{Statistics in Biopharmaceutical
#'   Research}, 5(4), 311-320.
#'
#'   Zhao, Y., Liu, Q., Sun, L. Z., and Anderson, K. M. (2025). Adjusted
#'   inference for multiple testing procedure in group-sequential designs.
#'   \emph{Biometrical Journal}, 67(1), e70020.
#'   \doi{10.1002/bimj.70020}
#'
#' @examples
#' # A graphical procedure with two hypotheses tested at two analyses
#' hypotheses <- c(0.5, 0.5)
#' transitions <- rbind(c(0, 1), c(1, 0))
#' g <- graph_create(hypotheses, transitions)
#'
#' # P-values at interim (50% info) and final (100% info) analyses
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
#'
#' # With look_back = TRUE (sequential p-values)
#' graph_test_shortcut_gsd(
#'   graph = g,
#'   p = p,
#'   alpha = 0.025,
#'   info_frac = c(0.5, 1),
#'   spending_fn = spending_of,
#'   look_back = TRUE
#' )
#'
#' # Different spending functions per hypothesis
#' graph_test_shortcut_gsd(
#'   graph = g,
#'   p = p,
#'   alpha = 0.025,
#'   info_frac = c(0.5, 1),
#'   spending_fn = list(spending_of, spending_pocock)
#' )
#'
#' # User-defined spending functions can also be used, e.g., wrapping
#' # gsDesign::sfHSD(). See vignette("group-sequential-testing") for details.
#'
#' # Different information fractions per hypothesis
#' graph_test_shortcut_gsd(
#'   graph = g,
#'   p = p,
#'   alpha = 0.025,
#'   info_frac = rbind(c(0.5, 1), c(0.6, 1)),
#'   spending_fn = spending_of
#' )
#'
#' # Different numbers of analyses per hypothesis (NA padding)
#' # H1 at analyses 1-2, H2 at 1-3, H3 at 2-3, H4 at 1 and 3
#' g4 <- graph_create(
#'   rep(0.25, 4),
#'   rbind(
#'     c(0, 1/3, 1/3, 1/3),
#'     c(1/3, 0, 1/3, 1/3),
#'     c(1/3, 1/3, 0, 1/3),
#'     c(1/3, 1/3, 1/3, 0)
#'   )
#' )
#' p4 <- rbind(
#'   H1 = c(0.024, 0.01, NA),
#'   H2 = c(0.015, 0.005, 0.001),
#'   H3 = c(NA, 0.012, 0.004),
#'   H4 = c(0.05, NA, 0.015)
#' )
#' # info_frac must be a matrix with NA matching p
#' info_frac4 <- rbind(
#'   H1 = c(0.5, 1, NA),
#'   H2 = c(1/3, 2/3, 1),
#'   H3 = c(NA, 0.5, 1),
#'   H4 = c(0.4, NA, 1)
#' )
#' graph_test_shortcut_gsd(
#'   graph = g4,
#'   p = p4,
#'   alpha = 0.025,
#'   info_frac = info_frac4,
#'   spending_fn = spending_of
#' )
graph_test_shortcut_gsd <- function(graph,
                                    p,
                                    alpha = 0.025,
                                    info_frac,
                                    spending_fn,
                                    look_back = FALSE,
                                    verbose = FALSE,
                                    test_values = FALSE) {
  # Input normalization --------------------------------------------------------
  # p is an m × K matrix (hypotheses × analyses). Ensure it is a matrix and
  # label rows with hypothesis names from the graph if not already named.
  num_hyps <- length(graph$hypotheses)
  hyp_names <- names(graph$hypotheses)

  if (!is.matrix(p)) p <- as.matrix(p)
  if (is.null(rownames(p))) rownames(p) <- hyp_names
  num_analyses <- ncol(p)

  # info_frac can be a vector (same fractions for all hypotheses) or an m × K
  # matrix (different fractions per hypothesis). Normalize to m × K so that
  # info_frac[j, ] gives the information fractions for hypothesis j. When a
  # vector is provided, each hypothesis gets the same fractions.
  if (is.vector(info_frac)) {
    if (anyNA(p)) {
      stop(
        "When p contains NA (different numbers of analyses per hypothesis), ",
        "info_frac must be a matrix with NA in the same positions as p."
      )
    }
    info_frac <- matrix(
      rep(info_frac, each = num_hyps),
      nrow = num_hyps,
      ncol = num_analyses
    )
  }
  rownames(info_frac) <- hyp_names

  # spending_fn can be a single function (same for all hypotheses) or a list
  # of m functions (one per hypothesis). Normalize to a named list so that
  # spending_fn[[j]] gives the spending function for hypothesis j.
  if (is.function(spending_fn)) {
    spending_fn <- rep(list(spending_fn), num_hyps)
  }
  names(spending_fn) <- hyp_names

  # look_back can be a scalar (same for all hypotheses) or a logical vector
  # of length m (per hypothesis). Normalize to a named logical vector.
  if (length(look_back) == 1) {
    look_back <- structure(rep(look_back, num_hyps), names = hyp_names)
  }
  names(look_back) <- hyp_names

  # Input validation -----------------------------------------------------------
  gsd_input_val(graph, p, alpha, info_frac, spending_fn, look_back,
                verbose, test_values)

  # Determine analysis names from column names of p and info_frac.
  # If both have column names, they must match. If only one has them, use those.
  # If neither has them, default to Analysis_1, Analysis_2, ...
  p_names <- colnames(p)
  if_names <- colnames(info_frac)
  if (!is.null(p_names) && !is.null(if_names)) {
    stopifnot(
      "Column names of p and info_frac must match" =
        identical(p_names, if_names)
    )
    analysis_names <- p_names
  } else if (!is.null(p_names)) {
    analysis_names <- p_names
  } else if (!is.null(if_names)) {
    analysis_names <- if_names
  } else {
    analysis_names <- paste0("Analysis_", seq_len(num_analyses))
  }
  colnames(p) <- analysis_names
  colnames(info_frac) <- analysis_names

  # Run the procedure ----------------------------------------------------------
  result <- gsd_test(
    graph, p, alpha, info_frac, spending_fn, look_back,
    num_analyses, num_hyps, hyp_names, analysis_names,
    test_values, verbose
  )

  # Build the report -----------------------------------------------------------
  # Combine inputs, outputs, and optional test_values into an S3 object.
  # Both repeated_p and sequential_p are always included for reporting,
  # regardless of which was used for rejection decisions.
  structure(
    list(
      inputs = list(
        graph = graph,
        p = p,
        alpha = alpha,
        info_frac = info_frac,
        spending_fn = spending_fn,
        look_back = look_back,
        test_groups = list(seq_len(num_hyps)),
        test_types = "bonferroni"
      ),
      outputs = list(
        repeated_p = result$rep_p_matrix,
        sequential_p = result$seq_p_matrix,
        adjusted_p = result$adjusted_p,
        rejected = result$rejected,
        decision_at = result$decision_at,
        first_rejected_at = result$first_rejected_at,
        rejection_sequence = result$rejection_sequence,
        graph = if (any(result$rejected)) {
          graph_update(graph, result$rejected)$updated_graph
        } else {
          graph
        }
      ),
      test_values = if (test_values) result$test_values,
      boundary_table = if (verbose) {
        gsd_boundary_table(graph, alpha, info_frac, spending_fn,
                           num_hyps, hyp_names)
      }
    ),
    class = "gsd_graph_report"
  )
}


#' Compute boundary table for all possible hypothesis weights
#'
#' For each hypothesis, enumerates all unique weights from the graph's closure
#' (via [graph_generate_weights()]) and computes the group sequential boundaries
#' at each analysis for each weight. This provides a lookup table for manual
#' verification: given a hypothesis's weight (from graph propagation), the
#' nominal boundary at each analysis can be read directly.
#'
#' @param graph An `initial_graph` object.
#' @param alpha Overall significance level.
#' @param info_frac Information fraction matrix (m x K).
#' @param spending_fn List of spending functions.
#' @param num_hyps Number of hypotheses.
#' @param hyp_names Character vector of hypothesis names.
#'
#' @return A named list of data frames, one per hypothesis. Each data frame
#'   has columns: `Weight`, `Alpha.Allocated`, and one `Boundary.k` column
#'   per analysis, showing the nominal p-value boundary at each analysis
#'   for each possible weight.
#'
#' @keywords internal
gsd_boundary_table <- function(graph, alpha, info_frac, spending_fn,
                               num_hyps, hyp_names) {
  # Get all possible weights from the closure
  weights_matrix <- graph_generate_weights(graph)
  # The weight columns are the last num_hyps columns
  weight_cols <- weights_matrix[, (num_hyps + 1):(2 * num_hyps), drop = FALSE]

  result <- list()
  for (j in seq_len(num_hyps)) {
    hyp <- hyp_names[j]

    # Get unique non-zero weights for this hypothesis, sorted
    unique_weights <- sort(unique(weight_cols[, j]))

    # Non-NA analysis indices for this hypothesis
    non_na <- which(!is.na(info_frac[j, ]))
    if_j <- info_frac[j, non_na]
    num_analyses_j <- length(if_j)

    rows <- list()
    for (w in unique_weights) {
      allocated <- w * alpha
      if (allocated <= 0) {
        boundaries <- rep(0, num_analyses_j)
      } else {
        bounds_result <- gs_boundaries(
          alpha = allocated,
          info_frac = if_j,
          spending_fn = spending_fn[[j]]
        )
        boundaries <- bounds_result$bounds_nominal
      }

      row <- data.frame(
        Weight = w,
        Alpha.Allocated = allocated,
        stringsAsFactors = FALSE
      )
      for (kk in seq_len(num_analyses_j)) {
        row[[paste0("Boundary.", non_na[kk])]] <- boundaries[kk]
      }
      rows[[length(rows) + 1]] <- row
    }

    result[[hyp]] <- do.call(rbind, rows)
  }

  result
}


#' Unified GSD procedure supporting per-hypothesis look_back
#'
#' Computes repeated and sequential p-values, then processes analyses
#' sequentially. At each analysis k, applies `graph_test_shortcut()` using
#' the appropriate p-values for each hypothesis: sequential p-values for
#' hypotheses with `look_back = TRUE`, repeated p-values for those with
#' `look_back = FALSE`. The graph is updated after each analysis before
#' proceeding to the next.
#'
#' @keywords internal
gsd_test <- function(graph, p, alpha, info_frac, spending_fn, look_back,
                     num_analyses, num_hyps, hyp_names, analysis_names,
                     test_values, verbose) {
  # Indices of non-NA analyses per hypothesis
  non_na_indices <- lapply(seq_len(num_hyps), function(j) which(!is.na(p[j, ])))

  # Compute repeated p-values at each analysis (m × K matrix)
  rep_p_matrix <- matrix(
    NA_real_, num_hyps, num_analyses,
    dimnames = list(hyp_names, analysis_names)
  )
  for (j in seq_len(num_hyps)) {
    idx_j <- non_na_indices[[j]]
    for (kk in seq_along(idx_j)) {
      cols <- idx_j[1:kk]
      rep_p_matrix[j, idx_j[kk]] <- suppressMessages(repeated_p(
        p = p[j, cols],
        info_frac = info_frac[j, cols],
        spending_fn = spending_fn[[j]]
      ))
    }
  }

  # Derive sequential p-values as cumulative minimum of repeated p-values
  seq_p_matrix <- rep_p_matrix
  for (j in seq_len(num_hyps)) {
    non_na <- !is.na(rep_p_matrix[j, ])
    if (any(non_na)) {
      seq_p_matrix[j, non_na] <- cummin(rep_p_matrix[j, non_na])
    }
  }

  # Process analyses sequentially
  rejected <- structure(rep(FALSE, num_hyps), names = hyp_names)
  decision_at <- structure(rep(NA_integer_, num_hyps), names = hyp_names)
  first_rejected_at <- structure(rep(NA_integer_, num_hyps), names = hyp_names)
  adjusted_p <- structure(rep(NA_real_, num_hyps), names = hyp_names)
  rejection_sequence <- character(0)

  step_graph <- graph
  tv_details <- if (test_values) vector("list", num_analyses)

  for (k in seq_len(num_analyses)) {
    # Get active hypotheses: not rejected AND has data at this analysis.
    # For look_back hypotheses, "has data" means data at any analysis up to k
    # (since the sequential p-value carries forward from prior analyses).
    has_data_k <- !is.na(p[, k])
    has_prior_data <- vapply(seq_len(num_hyps), function(j) {
      any(!is.na(p[j, seq_len(k)]))
    }, logical(1))
    active_at_k <- ifelse(look_back, has_prior_data, has_data_k)
    active <- !rejected & active_at_k

    if (!any(active)) next

    # Construct the p-value vector for the shortcut: use sequential p-values
    # for hypotheses with look_back = TRUE, repeated p-values otherwise.
    # For look_back hypotheses without data at analysis k, use their most
    # recent sequential p-value (carried forward from the last available
    # analysis).
    last_seq_p <- vapply(seq_len(num_hyps), function(j) {
      available <- which(!is.na(seq_p_matrix[j, seq_len(k)]))
      if (length(available) == 0) NA_real_ else seq_p_matrix[j, max(available)]
    }, numeric(1))

    p_for_shortcut <- ifelse(
      look_back,
      last_seq_p,
      rep_p_matrix[, k]
    )

    # For hypotheses that are not active, set p to 1 so they are never
    # selected by graph_test_shortcut().
    p_for_shortcut[!active] <- 1

    # Apply shortcut to the current graph
    shortcut_k <- graph_test_shortcut(
      graph = step_graph,
      p = p_for_shortcut,
      alpha = alpha,
      verbose = TRUE,
      test_values = FALSE
    )

    # Identify newly rejected hypotheses at this analysis
    newly_rejected <- shortcut_k$outputs$rejected & active & !rejected
    newly_rejected_names <- hyp_names[newly_rejected]

    # Record the rejection sequence at this analysis (in shortcut order)
    del_seq_k <- shortcut_k$details$del_seq
    newly_in_order <- del_seq_k[del_seq_k %in% newly_rejected_names]
    rejection_sequence <- c(rejection_sequence, newly_in_order)

    # Record adjusted p-values and decision analysis for newly rejected.
    # decision_at = the operational analysis k.
    # first_rejected_at = earliest analysis where boundary was crossed.
    # For look_back hypotheses, search backwards; otherwise, set to k.
    for (hyp_name in newly_rejected_names) {
      adjusted_p[hyp_name] <- shortcut_k$outputs$adjusted_p[hyp_name]
      decision_at[hyp_name] <- k

      if (look_back[hyp_name]) {
        # Find the weight at the point of rejection from the shortcut's
        # graph sequence
        rej_idx <- which(del_seq_k == hyp_name)
        w_at_rejection <-
          shortcut_k$details$results[[rej_idx]]$hypotheses[hyp_name]
        allocated_alpha <- w_at_rejection * alpha

        # Look back: earliest analysis where sequential p <= allocated alpha
        j <- which(hyp_names == hyp_name)
        earliest <- which(seq_p_matrix[j, 1:k] <= allocated_alpha)[1]
        first_rejected_at[hyp_name] <-
          if (!is.na(earliest)) earliest else k
      } else {
        first_rejected_at[hyp_name] <- k
      }
    }

    # For non-rejected active hypotheses, update adjusted p-values and
    # track the last analysis where they were tested.
    not_rejected_active <- active & !shortcut_k$outputs$rejected
    adjusted_p[not_rejected_active] <-
      shortcut_k$outputs$adjusted_p[not_rejected_active]
    decision_at[not_rejected_active] <- k

    rejected[newly_rejected] <- TRUE

    # Per-analysis test_values details
    if (test_values) {
      tv_details[[k]] <- gsd_test_values_details(
        step_graph, p, k, alpha, info_frac, spending_fn,
        newly_in_order, hyp_names, rejected, has_data_k
      )

      # Add Look_back column (FALSE for all standard rows)
      tv_details[[k]]$Look_back <- FALSE

      # For look_back hypotheses: insert prior-analysis rows where
      # first_rejected_at < k (boundary crossed at an earlier analysis).
      for (hyp_name in newly_rejected_names) {
        if (look_back[hyp_name] &&
            !is.na(first_rejected_at[hyp_name]) &&
            first_rejected_at[hyp_name] < k) {
          rej_idx <- which(del_seq_k == hyp_name)
          w_at_rej <-
            shortcut_k$details$results[[rej_idx]]$hypotheses[hyp_name]
          lb_rows <- gsd_test_values_look_back(
            hyp_name, k, first_rejected_at[hyp_name], alpha, p,
            info_frac, spending_fn, hyp_names, w_at_rej
          )

          # Check if this hypothesis has a standard row at analysis k
          hyp_row <- which(tv_details[[k]]$Hypothesis == hyp_name &
                           tv_details[[k]]$Analysis == k)

          if (length(hyp_row) > 0) {
            # Has data at analysis k: set Reject to FALSE and insert
            # look_back rows after it
            tv_details[[k]]$Reject[hyp_row] <- FALSE
            before <- tv_details[[k]][seq_len(hyp_row), , drop = FALSE]
            after <- if (hyp_row < nrow(tv_details[[k]])) {
              tv_details[[k]][(hyp_row + 1):nrow(tv_details[[k]]), ,
                              drop = FALSE]
            }
            tv_details[[k]] <- rbind(before, lb_rows, after)
          } else {
            # No data at analysis k (look_back-only): append look_back rows
            # at the position where this hypothesis was rejected in the
            # shortcut sequence
            tv_details[[k]] <- rbind(tv_details[[k]], lb_rows)
          }
        }
      }
    }

    # Update graph: remove all rejected hypotheses
    if (any(newly_rejected)) {
      step_graph <- graph_update(step_graph, rejected)$updated_graph
    }
  }

  list(
    rep_p_matrix = rep_p_matrix,
    seq_p_matrix = seq_p_matrix,
    adjusted_p = adjusted_p,
    rejected = rejected,
    decision_at = decision_at,
    first_rejected_at = first_rejected_at,
    rejection_sequence = rejection_sequence,
    test_values = tv_details
  )
}


#' Compute test_values for look_back = FALSE (analysis-by-analysis)
#'
#' For each analysis, computes the nominal boundaries and records the
#' rejection sequence with boundaries at which rejections occurred.
#' Walks through the rejection sequence within each analysis, updating
#' the graph and recomputing boundaries after each rejection.
#'
#' @keywords internal
gsd_test_values_details <- function(step_graph, p, k, alpha, info_frac,
                                  spending_fn, rejection_seq_k,
                                  hyp_names, rejected_after,
                                  has_data_k = rep(TRUE, length(hyp_names))) {
  num_hyps <- length(hyp_names)
  # Active hypotheses at the start of this analysis: have data at analysis k,
  # and either not yet rejected or rejected at this analysis
  active_before <- has_data_k &
    (!rejected_after | (hyp_names %in% rejection_seq_k))

  # Early return if no hypotheses are active (e.g., all rejected at earlier
  # analyses) or if rejection_seq_k is empty and no active hypotheses remain
  if (!any(active_before)) return(NULL)

  # Helper: compute the nominal boundary at analysis k for hypothesis j
  # using the current graph's allocated alpha (weight * alpha)
  compute_boundary <- function(j, current_graph) {
    total_alpha_j <- current_graph$hypotheses[j] * alpha
    if (total_alpha_j <= 0) return(0)
    # Use non-NA entries up to and including analysis k
    non_na_up_to_k <- which(!is.na(info_frac[j, ]) & seq_len(ncol(info_frac)) <= k)
    if_j <- info_frac[j, non_na_up_to_k]
    k_eff <- length(if_j)
    bounds_result <- gs_boundaries(
      alpha = total_alpha_j,
      info_frac = if_j,
      spending_fn = spending_fn[[j]]
    )
    bounds_result$bounds_nominal[k_eff]
  }

  detail_rows <- list()
  current_graph <- step_graph

  # Walk through rejections: compute boundary on-the-fly with current graph
  # weights, then update graph after each rejection
  for (hyp_name in rejection_seq_k) {
    j <- which(hyp_names == hyp_name)
    detail_rows[[length(detail_rows) + 1]] <- data.frame(
      Hypothesis = hyp_name,
      Weight = current_graph$hypotheses[hyp_name],
      p = p[j, k],
      Boundary = compute_boundary(j, current_graph),
      Reject = TRUE,
      stringsAsFactors = FALSE
    )

    # Update graph after this rejection
    delete_vec <- structure(rep(FALSE, num_hyps), names = hyp_names)
    delete_vec[hyp_name] <- TRUE
    current_graph <- graph_update(current_graph, delete_vec)$updated_graph
  }

  # Add non-rejected active hypotheses with boundaries from the final
  # updated graph (after all rejections at this analysis)
  rejected_names <- if (length(detail_rows) > 0) {
    vapply(detail_rows, function(r) r$Hypothesis, character(1))
  } else {
    character(0)
  }
  not_rejected_active <- active_before & !(hyp_names %in% rejected_names)
  for (j in which(not_rejected_active)) {
    detail_rows[[length(detail_rows) + 1]] <- data.frame(
      Hypothesis = hyp_names[j],
      Weight = current_graph$hypotheses[j],
      p = p[j, k],
      Boundary = compute_boundary(j, current_graph),
      Reject = FALSE,
      stringsAsFactors = FALSE
    )
  }

  if (length(detail_rows) == 0) return(NULL)

  data.frame(
    Analysis = k,
    do.call(rbind, detail_rows),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' Compute look_back rows for test_values
#'
#' When a hypothesis is rejected via look_back at an earlier analysis than the
#' operational analysis, this function generates rows showing the nominal
#' p-value and boundary at each prior analysis (in decreasing order from the
#' operational analysis down to the attributed analysis). The weight and
#' boundaries are computed using the hypothesis's weight at the point of
#' rejection. The `Reject` column indicates whether the nominal p-value
#' crosses the boundary at each analysis.
#'
#' @param hyp_name Name of the hypothesis.
#' @param k The operational analysis where the rejection occurred.
#' @param attributed_to The analysis to which the rejection is attributed.
#' @param alpha Overall significance level.
#' @param p P-value matrix.
#' @param info_frac Information fraction matrix.
#' @param spending_fn List of spending functions.
#' @param hyp_names Character vector of hypothesis names.
#' @param w_at_rejection The hypothesis weight at the point of rejection
#'   (from the shortcut's internal graph sequence).
#'
#' @return A data frame with look_back rows for analyses k-1, k-2, ...,
#'   attributed_to.
#'
#' @keywords internal
gsd_test_values_look_back <- function(hyp_name, k, attributed_to, alpha, p,
                                      info_frac, spending_fn, hyp_names,
                                      w_at_rejection) {
  j <- which(hyp_names == hyp_name)
  total_alpha_j <- w_at_rejection * alpha

  # Non-NA analysis indices for this hypothesis
  non_na_all <- which(!is.na(info_frac[j, ]))

  # Compute boundaries at all analyses up to k using the allocated alpha
  non_na_up_to_k <- non_na_all[non_na_all <= k]
  if_j <- info_frac[j, non_na_up_to_k]
  bounds_result <- gs_boundaries(
    alpha = total_alpha_j,
    info_frac = if_j,
    spending_fn = spending_fn[[j]]
  )

  # Generate rows for analyses k-1 down to attributed_to (decreasing order)
  prior_analyses <- seq(k - 1, attributed_to)
  detail_rows <- list()

  for (a in prior_analyses) {
    # Find the position of analysis a within the non-NA analyses up to k
    a_pos <- which(non_na_up_to_k == a)
    if (length(a_pos) == 0) next  # hypothesis has no data at this analysis

    nominal_p <- p[j, a]
    boundary <- bounds_result$bounds_nominal[a_pos]
    crossed <- !is.na(nominal_p) && nominal_p <= boundary

    detail_rows[[length(detail_rows) + 1]] <- data.frame(
      Analysis = a,
      Hypothesis = hyp_name,
      Weight = w_at_rejection,
      p = nominal_p,
      Boundary = boundary,
      Reject = crossed,
      Look_back = TRUE,
      stringsAsFactors = FALSE
    )
  }

  if (length(detail_rows) == 0) return(NULL)
  do.call(rbind, detail_rows)
}


#' Validate inputs for group sequential graphical MCP
#'
#' @inheritParams graph_test_shortcut_gsd
#'
#' @return Invisibly returns `graph`.
#'
#' @keywords internal
gsd_input_val <- function(graph, p, alpha, info_frac, spending_fn, look_back,
                          verbose, test_values) {
  num_hyps <- length(graph$hypotheses)
  num_analyses <- ncol(p)

  p_non_na <- p[!is.na(p)]
  if_non_na <- info_frac[!is.na(info_frac)]

  stopifnot(
    "Please test an `initial_graph` object" =
      inherits(graph, "initial_graph"),
    "P-values must be a matrix with rows matching the number of hypotheses" =
      is.matrix(p) && nrow(p) == num_hyps,
    "P-values must be numeric" = is.numeric(p),
    "Non-NA p-values must be between 0 and 1" =
      length(p_non_na) == 0 || all(p_non_na >= 0 & p_non_na <= 1),
    "NA positions in p and info_frac must match" =
      identical(is.na(p), is.na(info_frac)),
    "Alpha must be numeric" = is.numeric(alpha),
    "Please choose a single alpha level" = length(alpha) == 1,
    "Alpha must be between 0 and 1" = alpha >= 0 && alpha <= 1,
    "Information fractions must be a matrix with rows matching hypotheses" =
      is.matrix(info_frac) && nrow(info_frac) == num_hyps,
    "Information fractions must have the same number of columns as p" =
      ncol(info_frac) == num_analyses,
    "Information fractions must be numeric" = is.numeric(info_frac),
    "Non-NA information fractions must be in (0, 1]" =
      length(if_non_na) == 0 || all(if_non_na > 0 & if_non_na <= 1),
    "Spending functions must be a list of functions" =
      is.list(spending_fn) &&
      all(vapply(spending_fn, is.function, logical(1))),
    "Number of spending functions must match the number of hypotheses" =
      length(spending_fn) == num_hyps,
    "look_back must be a logical vector of length matching hypotheses" =
      is.logical(look_back) && length(look_back) == num_hyps,
    "Verbose flag must be a length one logical" =
      is.logical(verbose) && length(verbose) == 1,
    "Test values flag must be a length one logical" =
      is.logical(test_values) && length(test_values) == 1
  )

  # Each hypothesis must have at least one non-NA analysis
  for (j in seq_len(num_hyps)) {
    stopifnot(
      "Each hypothesis must have at least one non-NA analysis" =
        any(!is.na(p[j, ]))
    )
  }

  # Check info_frac is non-decreasing per hypothesis (non-NA values only)
  for (j in seq_len(num_hyps)) {
    t_j <- info_frac[j, !is.na(info_frac[j, ])]
    if (length(t_j) > 1) {
      stopifnot(
        "Information fractions must be non-decreasing for each hypothesis" =
          all(diff(t_j) >= 0)
      )
    }
  }

  invisible(graph)
}
