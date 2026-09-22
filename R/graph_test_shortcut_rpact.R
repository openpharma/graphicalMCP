#' Perform shortcut graphical multiple comparison procedures with group
#' sequential designs using rpact
#'
#' @description
#' Extends [graph_test_shortcut()] to group sequential designs in the same way
#' as [graph_test_shortcut_gsd()] and [graph_test_shortcut_gsDesign()], but
#' delegates all group sequential calculations (boundaries and repeated
#' p-values) to the rpact package. Designs are specified in rpact's own terms:
#' a design type `typeOfDesign` with its parameters `gammaA`, `deltaWT`, or
#' `userAlphaSpending`, and an optional spending time. The graphical
#' procedure, the two testing modes selected by `look_back`, and the structure
#' of the results are identical to [graph_test_shortcut_gsd()]; see that
#' function for a description of the procedure and of `look_back`.
#'
#' rpact is an optional dependency: the function stops with an informative
#' error if it is not installed.
#'
#' @inheritParams graph_test_shortcut_gsd
#' @inheritParams graph_test_shortcut
#' @param typeOfDesign Type of design in rpact's convention, as accepted by
#'   [rpact::getDesignGroupSequential()]: an alpha spending design `"asOF"`
#'   (Lan-DeMets O'Brien-Fleming, the default), `"asP"` (Lan-DeMets Pocock),
#'   `"asHSD"` (Hwang-Shih-DeCani), `"asKD"` (Kim-DeMets power family), or
#'   `"asUser"` (user-defined cumulative spending), or one of the classical
#'   boundary families `"OF"`, `"P"`, `"WT"`, `"PT"`, `"HP"`, `"WToptimum"`,
#'   `"noEarlyEfficacy"`. A single string applies to all hypotheses; a list
#'   of \eqn{m} strings gives one per hypothesis.
#' @param gammaA Parameter of the `"asHSD"` and `"asKD"` designs, passed to
#'   rpact as `gammaA`. A single value applies to all hypotheses; a list of
#'   \eqn{m} values gives one per hypothesis. Required for these design types.
#' @param deltaWT Parameter \eqn{\Delta} of the `"WT"` design, passed to
#'   rpact as `deltaWT`. A single value applies to all hypotheses; a list of
#'   \eqn{m} values gives one per hypothesis. Required for `"WT"`.
#' @param userAlphaSpending Cumulative alpha spent at each analysis for the
#'   `"asUser"` design, passed to rpact as `userAlphaSpending`: a
#'   non-decreasing vector with one value per analysis of the hypothesis,
#'   between 0 and `alpha`. A single vector applies to all hypotheses; a list
#'   of \eqn{m} vectors gives one per hypothesis. When the graph allocates a
#'   level other than `alpha` to a hypothesis, the values are scaled
#'   proportionally.
#' @param spending_time Optional spending time. Can be `NULL` (the default, in
#'   which case the spending time is `pmin(info_frac, 1)`), a numeric vector
#'   of length \eqn{K} applied to all hypotheses (only allowed when `p`
#'   contains no `NA`), or an \eqn{m \times K} matrix with `NA` in the same
#'   positions as `info_frac`. It controls how `alpha` is spent across
#'   analyses, separately from `info_frac`, which controls the correlation
#'   between analyses, and plays the role of `usTime` in
#'   [graph_test_shortcut_gsDesign()] and of [spending_with_time()] in
#'   [graph_test_shortcut_gsd()]. It does not apply to the classical boundary
#'   families or to `"asUser"` and is ignored, with a warning, for hypotheses
#'   that use them.
#'
#' @return An S3 object of class `gsd_graph_report` with the same elements as
#'   [graph_test_shortcut_gsd()]. `inputs` records `typeOfDesign`, `gammaA`,
#'   `deltaWT`, `userAlphaSpending`, and `spending_time` in place of
#'   `spending_fn`, and `inputs$engine` is `"rpact"`.
#'
#' @details
#' This function performs the same graphical group sequential procedure as
#' [graph_test_shortcut_gsd()] (Maurer and Bretz, 2013), but delegates all
#' group sequential calculations to rpact, used only through its public
#' function [rpact::getDesignGroupSequential()].
#'
#' * **Analysis structure.** For hypothesis \eqn{j} with information
#'   fractions \eqn{t_j} at its \eqn{K_j} analyses, a one-sided design
#'   `getDesignGroupSequential(kMax = K_j, informationRates = t_j, alpha,
#'   sided = 1, typeOfDesign, ...)` encodes the schedule, and is re-evaluated
#'   at whatever level the graph currently allocates to the hypothesis (its
#'   weight times `alpha`). A schedule may end before full information, as
#'   when only the analyses conducted so far are supplied.
#' * **Nominal boundaries.** For `test_values` and `boundary_table`, the
#'   boundary at analysis \eqn{k} for an allocated level is the design's
#'   `stageLevels[k]` (rpact's nominal one-sided significance level).
#' * **Repeated p-values.** The repeated p-value at analysis \eqn{k} is the
#'   smallest level at which the boundary at analysis \eqn{k} is crossed. It
#'   is found by solving \eqn{c_k(\alpha) = Z_k} on the design's
#'   `criticalValues` over the level. This is the algorithm that rpact's
#'   [rpact::getRepeatedPValues()] uses internally, applied without its
#'   ceiling of 0.5 and to every design type. rpact cannot compute designs at
#'   levels above roughly 0.65; a boundary that is not crossed at the highest
#'   level rpact can compute is reported as a repeated p-value of 1.
#'   Sequential p-values (`look_back = TRUE`) are the running minimum of the
#'   repeated p-values.
#' * **Spending time and over-running information.** rpact evaluates the
#'   spending function at the design's information rates and does not accept
#'   rates above 1. When `spending_time` differs from `info_frac`, or when
#'   `info_frac` exceeds 1 at the final analysis, the design is built in two
#'   steps with rpact's `"asUser"` type: the cumulative spending is taken from
#'   a design at the spending time and passed as `userAlphaSpending` to a
#'   design at the information fractions, divided by their maximum when they
#'   exceed 1 (the correlation between analyses depends only on their
#'   ratios). The classical boundary families are defined by their boundary
#'   shape over the whole planned schedule, and rpact requires their final
#'   information fraction to be exactly 1: unlike the alpha spending designs,
#'   they cannot be given only the analyses conducted so far.
#' * **Single-analysis hypotheses.** A hypothesis with one analysis below
#'   full information uses the first stage of a two-stage design ending at 1,
#'   so that its boundary is the cumulative spending at that information
#'   fraction; at full information it is a fixed-sample test.
#'
#' rpact warns when a level lies outside its validated range of
#' \eqn{[10^{-6}, 0.5)}; the graphical procedure evaluates such levels
#' routinely (for small hypothesis weights, and when searching for repeated
#' p-values), so these warnings are suppressed. rpact's integration is
#' deterministic, so results are reproducible without setting a seed. The
#' built-in spending functions correspond to `typeOfDesign = "asOF"`
#' ([spending_of()]), `"asP"` ([spending_pocock()]), `"asHSD"` with
#' `gammaA = gamma` ([spending_hsd()]), `"asKD"` with `gammaA = 1`
#' ([spending_linear()]), and `"WT"` with `deltaWT = delta`
#' ([spending_wt()]).
#'
#' @seealso
#'   [graph_test_shortcut_gsd()] for the same procedure with the built-in
#'   spending functions, [graph_test_shortcut_gsDesign()] for the same
#'   procedure with gsDesign, [graph_test_shortcut()] for the fixed-sample
#'   procedure, [rpact::getDesignGroupSequential()] for the rpact function
#'   used.
#'
#' @rdname graph_test_shortcut_rpact
#'
#' @export
#'
#' @references
#'   Maurer, W., and Bretz, F. (2013). Multiple testing in group sequential
#'   trials using graphical approaches. \emph{Statistics in Biopharmaceutical
#'   Research}, 5(4), 311-320.
#'
#'   Wassmer, G., and Pahlke, F. rpact: Confirmatory Adaptive Clinical Trial
#'   Design and Analysis. R package.
#'   \url{https://CRAN.R-project.org/package=rpact}
#'
#' @examples
#' if (requireNamespace("rpact", quietly = TRUE)) {
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
#'   # Lan-DeMets O'Brien-Fleming alpha spending (the default)
#'   graph_test_shortcut_rpact(g, p, alpha = 0.025, info_frac = c(0.5, 1))
#'
#'   # Hwang-Shih-DeCani spending with gamma = -2, using sequential p-values
#'   graph_test_shortcut_rpact(
#'     g, p,
#'     alpha = 0.025, info_frac = c(0.5, 1),
#'     typeOfDesign = "asHSD", gammaA = -2, look_back = TRUE
#'   )
#'
#'   # Different designs per hypothesis
#'   graph_test_shortcut_rpact(
#'     g, p,
#'     alpha = 0.025, info_frac = c(0.5, 1),
#'     typeOfDesign = list("asOF", "asP")
#'   )
#'
#'   # A spending time that differs from the information fraction
#'   graph_test_shortcut_rpact(
#'     g, p,
#'     alpha = 0.025, info_frac = c(0.5, 1), spending_time = c(0.4, 1)
#'   )
#'
#'   # Wang-Tsiatis boundaries with Delta = 0.25
#'   graph_test_shortcut_rpact(
#'     g, p,
#'     alpha = 0.025, info_frac = c(0.5, 1), typeOfDesign = "WT", deltaWT = 0.25
#'   )
#' }
graph_test_shortcut_rpact <- function(graph,
                                      p,
                                      alpha = 0.025,
                                      info_frac,
                                      typeOfDesign = "asOF",
                                      gammaA = NULL,
                                      deltaWT = NULL,
                                      userAlphaSpending = NULL,
                                      spending_time = NULL,
                                      look_back = FALSE,
                                      verbose = FALSE,
                                      test_values = FALSE) {
  gsd_check_rpact()

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

  # rpact design specification: normalize typeOfDesign, its parameters, and
  # spending_time to one entry per hypothesis. A single value applies to all
  # hypotheses; only a list is interpreted per hypothesis.
  if (!is.list(typeOfDesign)) typeOfDesign <- rep(list(typeOfDesign), num_hyps)
  if (!is.list(gammaA)) gammaA <- rep(list(gammaA), num_hyps)
  if (!is.list(deltaWT)) deltaWT <- rep(list(deltaWT), num_hyps)
  if (!is.list(userAlphaSpending)) {
    userAlphaSpending <- rep(list(userAlphaSpending), num_hyps)
  }
  if (!is.null(spending_time) && !is.matrix(spending_time)) {
    if (anyNA(p)) {
      stop(
        "When p contains NA (different numbers of analyses per hypothesis), ",
        "spending_time must be a matrix with NA in the same positions as p."
      )
    }
    if (length(spending_time) != num_analyses) {
      stop(
        "spending_time given as a vector must have one value per analysis (",
        num_analyses, ")."
      )
    }
    spending_time <- matrix(
      rep(spending_time, each = num_hyps),
      nrow = num_hyps,
      ncol = num_analyses
    )
  }
  # Lists of the wrong length are reported by the validation below
  for (nm in c("typeOfDesign", "gammaA", "deltaWT", "userAlphaSpending")) {
    if (length(get(nm)) == num_hyps) {
      assign(nm, stats::setNames(get(nm), hyp_names))
    }
  }
  if (!is.null(spending_time)) rownames(spending_time) <- hyp_names

  # Input validation -----------------------------------------------------------
  gsd_input_val(
    graph, p, alpha, info_frac, NULL, look_back,
    verbose, test_values
  )
  gsd_input_val_rpact(
    typeOfDesign, gammaA, deltaWT, userAlphaSpending, spending_time,
    info_frac, alpha, num_hyps
  )

  # Determine analysis names from column names of p and info_frac
  analysis_names <- gsd_analysis_names(p, info_frac)
  colnames(p) <- analysis_names
  colnames(info_frac) <- analysis_names
  if (!is.null(spending_time)) colnames(spending_time) <- analysis_names

  # Run the procedure ----------------------------------------------------------
  # The rpact engine supplies repeated p-values and nominal boundaries; the
  # graphical logic in gsd_test() is shared with graph_test_shortcut_gsd().
  engine <- gsd_engine_rpact(
    alpha, info_frac, typeOfDesign, gammaA, deltaWT, userAlphaSpending,
    spending_time
  )
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
        typeOfDesign = typeOfDesign,
        gammaA = gammaA,
        deltaWT = deltaWT,
        userAlphaSpending = userAlphaSpending,
        spending_time = spending_time,
        look_back = look_back,
        test_groups = list(seq_len(num_hyps)),
        test_types = "bonferroni",
        engine = "rpact"
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


#' Stop with an informative error if rpact is not installed
#'
#' @noRd
gsd_check_rpact <- function() {
  if (!requireNamespace("rpact", quietly = TRUE)) {
    stop(
      "Package 'rpact' is required by graph_test_shortcut_rpact(). ",
      "Install it with install.packages(\"rpact\").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Validate the rpact design specification
#'
#' Checks `typeOfDesign`, `gammaA`, `deltaWT`, `userAlphaSpending`, and
#' `spending_time` after they have been normalized to one entry per
#' hypothesis. The remaining inputs are validated by `gsd_input_val()`.
#'
#' @noRd
gsd_input_val_rpact <- function(typeOfDesign, gammaA, deltaWT,
                                userAlphaSpending, spending_time, info_frac,
                                alpha, num_hyps) {
  types <- c(gsd_rpact_spending, gsd_rpact_classical)
  is_valid_type <- function(x) {
    is.character(x) && length(x) == 1 && x %in% types
  }
  is_scalar <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)

  stopifnot(
    "typeOfDesign must be one of rpact's design types or a list of these" =
      is.list(typeOfDesign) && all(vapply(typeOfDesign, is_valid_type, logical(1))),
    "Number of design types (typeOfDesign) must match the number of hypotheses" =
      length(typeOfDesign) == num_hyps,
    "Number of gammaA values must match the number of hypotheses" =
      length(gammaA) == num_hyps,
    "Number of deltaWT values must match the number of hypotheses" =
      length(deltaWT) == num_hyps,
    "Number of userAlphaSpending vectors must match the number of hypotheses" =
      length(userAlphaSpending) == num_hyps
  )

  for (j in seq_len(num_hyps)) {
    type <- typeOfDesign[[j]]
    t_j <- info_frac[j, !is.na(info_frac[j, ])]
    if (type %in% c("asHSD", "asKD") && !is_scalar(gammaA[[j]])) {
      stop("gammaA must be a single number for typeOfDesign \"", type, "\".")
    }
    if (identical(type, "WT") && !is_scalar(deltaWT[[j]])) {
      stop("deltaWT must be a single number for typeOfDesign \"WT\".")
    }
    if (identical(type, "asUser")) {
      u <- userAlphaSpending[[j]]
      if (!is.numeric(u) || length(u) != length(t_j) || anyNA(u)) {
        stop(
          "userAlphaSpending must give the cumulative alpha spent at each of ",
          "the ", length(t_j), " analyses of hypothesis ", j, " for ",
          "typeOfDesign \"asUser\"."
        )
      }
      if (any(u < 0) || any(u > alpha) || any(diff(u) < 0)) {
        stop(
          "userAlphaSpending must be non-decreasing and between 0 and alpha."
        )
      }
    }
    if (type %in% gsd_rpact_classical && length(t_j) > 1 &&
      !isTRUE(all.equal(t_j[length(t_j)], 1))) {
      stop(
        "rpact requires the final information fraction to be exactly 1 for ",
        "the classical boundary families (\"", type, "\" for hypothesis ", j,
        "): supply the full planned schedule."
      )
    }
  }

  if (!is.null(spending_time)) {
    st_non_na <- spending_time[!is.na(spending_time)]
    stopifnot(
      "spending_time must be a numeric matrix with rows matching hypotheses" =
        is.matrix(spending_time) && is.numeric(spending_time) &&
          nrow(spending_time) == num_hyps,
      "spending_time must have the same number of columns as info_frac" =
        ncol(spending_time) == ncol(info_frac),
      "NA positions in spending_time and info_frac must match" =
        identical(unname(is.na(spending_time)), unname(is.na(info_frac))),
      "Non-NA spending_time values must be non-negative" =
        length(st_non_na) == 0 || all(st_non_na >= 0)
    )
    for (j in seq_len(num_hyps)) {
      s_j <- spending_time[j, !is.na(spending_time[j, ])]
      stopifnot(
        "spending_time must be non-decreasing for each hypothesis" =
          length(s_j) <= 1 || all(diff(s_j) >= 0),
        "At most one spending_time value per hypothesis can be >= 1" =
          sum(s_j >= 1) <= 1
      )
    }
    ignored <- vapply(
      typeOfDesign,
      function(x) x %in% gsd_rpact_classical || identical(x, "asUser"),
      logical(1)
    )
    if (any(ignored)) {
      warning(
        "spending_time does not apply to the classical boundary families or ",
        "to \"asUser\" and is ignored for hypotheses using them.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}
