#' Perform shortcut graphical multiple comparison procedures with group
#' sequential designs using gsDesign
#'
#' @description
#' Extends [graph_test_shortcut()] to group sequential designs in the same way
#' as [graph_test_shortcut_gsd()], but delegates all group sequential
#' calculations (boundaries and repeated p-values) to the gsDesign package.
#' Spending functions are specified in gsDesign's own terms: an upper spending
#' function `sfu`, its parameter `sfupar`, and an optional spending time
#' `usTime`. The graphical procedure, the two testing modes selected by
#' `look_back`, and the structure of the results are identical to
#' [graph_test_shortcut_gsd()]; see that function for a description of the
#' procedure and of `look_back`.
#'
#' gsDesign is an optional dependency: the function stops with an informative
#' error if it is not installed.
#'
#' @inheritParams graph_test_shortcut_gsd
#' @inheritParams graph_test_shortcut
#' @param sfu Upper spending function(s) in gsDesign's convention. Can be:
#'   * A single spending function such as [gsDesign::sfLDOF()] (the default),
#'     [gsDesign::sfLDPocock()], [gsDesign::sfHSD()], or
#'     [gsDesign::sfPower()], or one of the strings `"OF"`, `"Pocock"`, `"WT"`
#'     for the classical O'Brien-Fleming, Pocock, and Wang-Tsiatis boundaries;
#'     applied to all hypotheses.
#'   * A list of \eqn{m} such values, one per hypothesis.
#'
#'   A spending function must be evaluable at any leading subset of a
#'   hypothesis's spending times, as [gsDesign::sequentialPValue()] requires.
#'   This excludes [gsDesign::sfPoints()]; the same spending can be specified
#'   with [gsDesign::sfLinear()]. User-defined spending functions follow
#'   gsDesign's template [gsDesign::spendingFunction()]: they take
#'   `(alpha, t, param)` and return a `spendfn` object whose `spend` element
#'   is the cumulative alpha spent at `t`.
#' @param sfupar Parameter(s) of the spending function(s), passed to gsDesign
#'   as `sfupar`. A non-list value (including `NULL`, gsDesign's default) is
#'   applied to all hypotheses; a list of \eqn{m} values gives one per
#'   hypothesis, with `NULL` entries using gsDesign's default. For example,
#'   `sfupar = -4` with `sfu = gsDesign::sfHSD`, `sfupar = 1` with
#'   `sfu = gsDesign::sfPower` for linear spending, or `sfupar = 0.25` with
#'   `sfu = "WT"`. A parameter that is itself a list, as for
#'   [gsDesign::sfTruncated()], must be wrapped in a list of \eqn{m} copies so
#'   that it is not read as one parameter per hypothesis.
#' @param usTime Optional spending time, passed to gsDesign as `usTime`. Can
#'   be `NULL` (the default, in which case the spending time is
#'   `pmin(info_frac, 1)`), a numeric vector of length \eqn{K} applied to all
#'   hypotheses (only allowed when `p` contains no `NA`), or an
#'   \eqn{m \times K} matrix with `NA` in the same positions as `info_frac`.
#'   The spending time controls how `alpha` is spent across analyses,
#'   separately from `info_frac`, which controls the correlation between
#'   analyses; it plays the role of [spending_with_time()] in
#'   [graph_test_shortcut_gsd()]. It does not apply to the classical boundary
#'   families and is ignored, with a warning, for hypotheses that use one.
#'
#' @return An S3 object of class `gsd_graph_report` with the same elements as
#'   [graph_test_shortcut_gsd()]. `inputs` records `sfu`, `sfupar`, and
#'   `usTime` in place of `spending_fn`, and `inputs$engine` is `"gsDesign"`.
#'
#' @details
#' This function performs the same graphical group sequential procedure as
#' [graph_test_shortcut_gsd()] (Maurer and Bretz, 2013), but delegates all
#' group sequential calculations to the gsDesign package, used only through
#' its public functions [gsDesign::gsDesign()],
#' [gsDesign::sequentialPValue()], and the `sf*` spending functions.
#'
#' The two layers meet through a single interface: for each hypothesis at each
#' analysis, gsDesign supplies a *repeated p-value* and a *nominal p-value
#' boundary*. The graphical layer (the Bonferroni shortcut, graph updates,
#' rejection sequence, and look-back handling) is identical to
#' [graph_test_shortcut_gsd()] and never interacts with gsDesign directly.
#'
#' * **Spending specification.** The upper spending function `sfu`, its
#'   parameter `sfupar`, and the spending time `usTime` follow gsDesign's
#'   conventions and may be given once or per hypothesis. The built-in
#'   spending functions correspond to `sfu = gsDesign::sfLDOF`
#'   ([spending_of()]), `gsDesign::sfLDPocock` ([spending_pocock()]),
#'   `gsDesign::sfHSD` with `sfupar = gamma` ([spending_hsd()]),
#'   `gsDesign::sfPower` with `sfupar = 1` ([spending_linear()]), and `"WT"`
#'   with `sfupar = delta` ([spending_wt()]); `usTime` plays the role of
#'   [spending_with_time()].
#' * **Analysis structure.** For hypothesis \eqn{j} with information
#'   fractions \eqn{t_j} at its \eqn{K_j} analyses, the schedule is encoded by
#'   `gsDesign::gsDesign(k = K_j, test.type = 1, sfu, sfupar, n.I = t_j,
#'   maxn.IPlan = 1, usTime)`: a one-sided efficacy boundary with information
#'   given directly as fractions of the planned maximum, so the correlation
#'   between analyses is the one implied by `info_frac`. The design is
#'   re-evaluated at whatever level the graph currently allocates to the
#'   hypothesis (its weight times `alpha`).
#' * **Repeated p-values.** The repeated p-value at analysis \eqn{k} is the
#'   smallest level at which the boundary at analysis \eqn{k} is crossed. It is
#'   obtained from [gsDesign::sequentialPValue()] after setting the
#'   Z-statistics of analyses \eqn{1, \ldots, k-1} to \eqn{-20} (gsDesign's
#'   own value for "no lower bound"), so that only analysis \eqn{k} can trigger
#'   a rejection; this is exact, not an approximation. The spending time is
#'   always passed explicitly (`usTime` if supplied, otherwise
#'   `pmin(info_frac, 1)`): it controls how `alpha` is spent, separately from
#'   `n.I`, which controls the correlation structure. gsDesign's default would
#'   rescale the spending time by the last observed information and silently
#'   change the interim spending when a trial over-runs its plan. For the
#'   classical boundary families (`"OF"`, `"Pocock"`, `"WT"`), which gsDesign
#'   represents as boundary shapes rather than spending functions, the
#'   repeated p-value is instead found by solving \eqn{b_k(\alpha) = Z_k}
#'   directly on the design's upper boundary. Sequential p-values
#'   (`look_back = TRUE`) are the running minimum of the repeated p-values.
#' * **Nominal boundaries.** For `test_values` and `boundary_table`, the
#'   boundary at analysis \eqn{k} for an allocated level \eqn{w_j \alpha} is
#'   \eqn{1 - \Phi(b_k)}, with \eqn{b_k} the upper bound of the design rebuilt
#'   at `alpha = w_j * alpha`. A p-value lies at or below this boundary exactly
#'   when its repeated p-value lies at or below \eqn{w_j \alpha}, which is why
#'   the boundary table can be used to verify decisions by hand.
#' * **Single-analysis hypotheses.** [gsDesign::gsDesign()] requires at least
#'   two analyses, so a hypothesis with one analysis uses the univariate
#'   results directly: the boundary is the cumulative spend at \eqn{t_1}, and
#'   the repeated p-value is the level at which that spend equals the observed
#'   p-value (the p-value itself when \eqn{t_1 = 1}).
#'
#' gsDesign evaluates the required probabilities by deterministic numerical
#' integration, whereas [graph_test_shortcut_gsd()] uses randomized
#' quasi-Monte Carlo integration via mvtnorm. Results are therefore
#' reproducible without setting a seed, and the two functions agree to about
#' \eqn{10^{-5}} in p-values and boundaries; rejection decisions coincide
#' except for p-values within that tolerance of a boundary. For Wang-Tsiatis
#' boundaries gsDesign is exact, whereas [spending_wt()] is itself a Monte
#' Carlo approximation, so on the Z scale the two can differ by up to about
#' \eqn{10^{-3}} (see the `gsd-validation` vignette) while still agreeing to
#' \eqn{10^{-6}} on the p-value scale.
#'
#' @seealso
#'   [graph_test_shortcut_gsd()] for the same procedure with the built-in
#'   spending functions, [graph_test_shortcut()] for the fixed-sample
#'   procedure, [gsDesign::gsDesign()] and [gsDesign::sequentialPValue()] for
#'   the gsDesign functions used.
#'
#' @rdname graph_test_shortcut_gsDesign
#'
#' @export
#'
#' @references
#'   Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
#'   trials using graphical approaches. \emph{Statistics in Biopharmaceutical
#'   Research}, 5(4), 311-320.
#'
#'   Anderson, K. M. gsDesign: Group Sequential Design. R package.
#'   \url{https://CRAN.R-project.org/package=gsDesign}
#'
#' @examples
#' if (requireNamespace("gsDesign", quietly = TRUE)) {
#'   # A graphical procedure with two hypotheses tested at two analyses
#'   hypotheses <- c(0.5, 0.5)
#'   transitions <- rbind(c(0, 1), c(1, 0))
#'   g <- graph_create(hypotheses, transitions)
#'
#'   # P-values at interim (50% info) and final (100% info) analyses
#'   p <- rbind(
#'     H1 = c(0.024, 0.01),
#'     H2 = c(0.015, 0.005)
#'   )
#'
#'   # Lan-DeMets O'Brien-Fleming spending (the default)
#'   graph_test_shortcut_gsDesign(g, p, alpha = 0.025, info_frac = c(0.5, 1))
#'
#'   # Hwang-Shih-DeCani spending with gamma = -2, using sequential p-values
#'   graph_test_shortcut_gsDesign(
#'     g, p,
#'     alpha = 0.025, info_frac = c(0.5, 1),
#'     sfu = gsDesign::sfHSD, sfupar = -2, look_back = TRUE
#'   )
#'
#'   # Different spending functions per hypothesis
#'   graph_test_shortcut_gsDesign(
#'     g, p,
#'     alpha = 0.025, info_frac = c(0.5, 1),
#'     sfu = list(gsDesign::sfLDOF, gsDesign::sfLDPocock)
#'   )
#'
#'   # A spending time that differs from the information fraction
#'   graph_test_shortcut_gsDesign(
#'     g, p,
#'     alpha = 0.025, info_frac = c(0.5, 1), usTime = c(0.4, 1)
#'   )
#'
#'   # Exact Wang-Tsiatis boundaries with Delta = 0.25
#'   graph_test_shortcut_gsDesign(
#'     g, p,
#'     alpha = 0.025, info_frac = c(0.5, 1), sfu = "WT", sfupar = 0.25
#'   )
#' }
graph_test_shortcut_gsDesign <- function(graph,
                                         p,
                                         alpha = 0.025,
                                         info_frac,
                                         sfu = gsDesign::sfLDOF,
                                         sfupar = NULL,
                                         usTime = NULL,
                                         look_back = FALSE,
                                         verbose = FALSE,
                                         test_values = FALSE) {
  gsd_check_gsDesign()

  # Input normalization --------------------------------------------------------
  # Identical to graph_test_shortcut_gsd(): p is an m x K matrix, info_frac
  # is normalized to m x K, look_back to a named logical vector.
  num_hyps <- length(graph$hypotheses)
  hyp_names <- names(graph$hypotheses)

  if (!is.matrix(p)) p <- as.matrix(p)
  if (is.null(rownames(p))) rownames(p) <- hyp_names
  num_analyses <- ncol(p)

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

  if (length(look_back) == 1) {
    look_back <- structure(rep(look_back, num_hyps), names = hyp_names)
  }
  names(look_back) <- hyp_names

  # gsDesign spending specification: normalize sfu, sfupar, and usTime to one
  # entry per hypothesis. A single spending function or family name applies
  # to all hypotheses; only a list is interpreted per hypothesis (gsDesign's
  # sfupar may itself be a vector, e.g. for sfPoints()).
  if (is.function(sfu) || (is.character(sfu) && length(sfu) == 1)) {
    sfu <- rep(list(sfu), num_hyps)
  }
  if (!is.list(sfupar)) {
    sfupar <- rep(list(sfupar), num_hyps)
  }
  if (!is.null(usTime) && !is.matrix(usTime)) {
    if (anyNA(p)) {
      stop(
        "When p contains NA (different numbers of analyses per hypothesis), ",
        "usTime must be a matrix with NA in the same positions as p."
      )
    }
    if (length(usTime) != num_analyses) {
      stop(
        "usTime given as a vector must have one value per analysis (",
        num_analyses, ")."
      )
    }
    usTime <- matrix(
      rep(usTime, each = num_hyps),
      nrow = num_hyps,
      ncol = num_analyses
    )
  }
  # Lists of the wrong length are reported by the validation below
  if (is.list(sfu) && length(sfu) == num_hyps) names(sfu) <- hyp_names
  if (is.list(sfupar) && length(sfupar) == num_hyps) names(sfupar) <- hyp_names
  if (!is.null(usTime)) rownames(usTime) <- hyp_names

  # Input validation -----------------------------------------------------------
  gsd_input_val(
    graph, p, alpha, info_frac, NULL, look_back,
    verbose, test_values
  )
  gsd_input_val_gsDesign(sfu, sfupar, usTime, info_frac, num_hyps)

  # Determine analysis names from column names of p and info_frac
  analysis_names <- gsd_analysis_names(p, info_frac)
  colnames(p) <- analysis_names
  colnames(info_frac) <- analysis_names
  if (!is.null(usTime)) colnames(usTime) <- analysis_names

  # Run the procedure ----------------------------------------------------------
  # The gsDesign engine supplies repeated p-values and nominal boundaries; the
  # graphical logic in gsd_test() is shared with graph_test_shortcut_gsd().
  engine <- gsd_engine_gsDesign(alpha, info_frac, sfu, sfupar, usTime)
  result <- gsd_test(
    graph, p, alpha, info_frac, engine, look_back,
    num_analyses, num_hyps, hyp_names, analysis_names,
    test_values, verbose
  )

  # Build the report -----------------------------------------------------------
  structure(
    list(
      inputs = list(
        graph = graph,
        p = p,
        alpha = alpha,
        info_frac = info_frac,
        sfu = sfu,
        sfupar = sfupar,
        usTime = usTime,
        look_back = look_back,
        test_groups = list(seq_len(num_hyps)),
        test_types = "bonferroni",
        engine = "gsDesign"
      ),
      outputs = list(
        repeated_p = result$rep_p_matrix,
        sequential_p = result$seq_p_matrix,
        adjusted_p = result$adjusted_p,
        rejected = result$rejected,
        decision_at = result$decision_at,
        first_rejected_at = result$first_rejected_at,
        last_rejected_at = result$last_rejected_at,
        rejection_sequence = result$rejection_sequence,
        graph = if (any(result$rejected)) {
          graph_update(graph, result$rejected)$updated_graph
        } else {
          graph
        }
      ),
      test_values = if (test_values) result$test_values,
      boundary_table = if (verbose) {
        gsd_boundary_table(
          graph, alpha, info_frac, engine,
          num_hyps, hyp_names
        )
      }
    ),
    class = "gsd_graph_report"
  )
}


#' Stop with an informative error if gsDesign is not installed
#'
#' @noRd
gsd_check_gsDesign <- function() {
  if (!requireNamespace("gsDesign", quietly = TRUE)) {
    stop(
      "Package 'gsDesign' is required by graph_test_shortcut_gsDesign(). ",
      "Install it with install.packages(\"gsDesign\").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Validate the gsDesign spending specification
#'
#' Checks `sfu`, `sfupar`, and `usTime` after they have been normalized to one
#' entry per hypothesis. The remaining inputs are validated by
#' `gsd_input_val()`.
#'
#' @noRd
gsd_input_val_gsDesign <- function(sfu, sfupar, usTime, info_frac, num_hyps) {
  classical <- c("OF", "Pocock", "WT")
  is_valid_sfu <- function(f) {
    is.function(f) ||
      (is.character(f) && length(f) == 1 && f %in% classical)
  }

  stopifnot(
    "sfu must be a spending function, one of \"OF\", \"Pocock\", \"WT\", or a list of these" =
      is.list(sfu) && all(vapply(sfu, is_valid_sfu, logical(1))),
    "Number of spending functions (sfu) must match the number of hypotheses" =
      length(sfu) == num_hyps,
    "Number of spending function parameters (sfupar) must match the number of hypotheses" =
      length(sfupar) == num_hyps
  )

  # sfPoints() insists on one parameter per analysis time at every call, but
  # gsDesign::sequentialPValue() evaluates the spending function on the
  # analyses observed so far
  uses_points <- vapply(
    sfu, function(f) is.function(f) && identical(f, gsDesign::sfPoints),
    logical(1)
  )
  if (any(uses_points)) {
    stop(
      "gsDesign::sfPoints() cannot be evaluated at a partial analysis ",
      "schedule and is not supported by graph_test_shortcut_gsDesign(); ",
      "specify the same spending with gsDesign::sfLinear() instead.",
      call. = FALSE
    )
  }

  if (!is.null(usTime)) {
    ust_non_na <- usTime[!is.na(usTime)]
    stopifnot(
      "usTime must be a numeric matrix with rows matching hypotheses" =
        is.matrix(usTime) && is.numeric(usTime) && nrow(usTime) == num_hyps,
      "usTime must have the same number of columns as info_frac" =
        ncol(usTime) == ncol(info_frac),
      "NA positions in usTime and info_frac must match" =
        identical(unname(is.na(usTime)), unname(is.na(info_frac))),
      "Non-NA usTime values must be non-negative" =
        length(ust_non_na) == 0 || all(ust_non_na >= 0)
    )
    for (j in seq_len(num_hyps)) {
      u_j <- usTime[j, !is.na(usTime[j, ])]
      stopifnot(
        "usTime must be non-decreasing for each hypothesis" =
          length(u_j) <= 1 || all(diff(u_j) >= 0),
        "At most one usTime value per hypothesis can be >= 1" =
          sum(u_j >= 1) <= 1
      )
    }
    if (any(vapply(sfu, is.character, logical(1)))) {
      warning(
        "usTime does not apply to the classical boundary families ",
        "(\"OF\", \"Pocock\", \"WT\") and is ignored for hypotheses using them.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}
