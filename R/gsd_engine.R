#' Group sequential engines for the graphical procedure
#'
#' @description
#' The graphical procedure in [graph_test_shortcut_gsd()] relies on exactly two
#' group sequential computations: repeated p-values and nominal p-value
#' boundaries. An *engine* bundles these two computations so that the
#' graphical logic in `gsd_test()` and its helpers is independent of how they
#' are carried out.
#'
#' An engine is a list of two functions. Each takes the hypothesis index `j`
#' and closes over that hypothesis's spending specification:
#' * `repeated_p(p, info_frac, j)` - the repeated p-value for hypothesis `j`
#'   at the last of the supplied analyses, given the p-values and information
#'   fractions at all analyses up to and including it.
#' * `nominal_bounds(alpha, info_frac, j)` - the nominal p-value boundaries
#'   for hypothesis `j` at each supplied analysis when the significance level
#'   `alpha` is allocated to it.
#'
#' `gsd_engine_mvtnorm()` builds the engine backed by [repeated_p()] and
#' [gs_boundaries()], which evaluate the joint distribution of the test
#' statistics with mvtnorm.
#'
#' @param spending_fn A list of spending functions, one per hypothesis. Each
#'   must accept two arguments, `alpha` and `info_frac`, and return the
#'   cumulative alpha spent.
#'
#' @return A list with the functions `repeated_p` and `nominal_bounds`.
#'
#' @keywords internal
gsd_engine_mvtnorm <- function(spending_fn) {
  list(
    repeated_p = function(p, info_frac, j) {
      repeated_p(p = p, info_frac = info_frac, spending_fn = spending_fn[[j]])
    },
    nominal_bounds = function(alpha, info_frac, j) {
      gs_boundaries(
        alpha = alpha,
        info_frac = info_frac,
        spending_fn = spending_fn[[j]]
      )$bounds_nominal
    }
  )
}


#' Group sequential engine backed by gsDesign
#'
#' @description
#' `gsd_engine_gsDesign()` builds the engine used by
#' [graph_test_shortcut_gsDesign()]. It has the same two-function interface as
#' [gsd_engine_mvtnorm()] but obtains every group sequential quantity from the
#' gsDesign package:
#' * One design per hypothesis encodes its analysis schedule,
#'   `gsDesign::gsDesign(k, test.type = 1, sfu, sfupar, n.I = info_frac,
#'   maxn.IPlan = 1, usTime)`, with information given as fractions of the
#'   planned maximum so that the correlation between analyses is the one
#'   implied by `info_frac`, and `usTime` controlling how alpha is spent.
#' * Repeated p-values come from `gsDesign::sequentialPValue()` after setting
#'   the Z-statistics of the earlier analyses to -20 (gsDesign's own value for
#'   "no lower bound"), so that only the current analysis can trigger a
#'   rejection. For the classical boundary families that gsDesign represents
#'   as boundary shapes rather than spending functions (`sfu` given as `"OF"`,
#'   `"Pocock"`, or `"WT"`), `sequentialPValue()` is not available, and the
#'   repeated p-value is instead found by solving \eqn{b_k(\alpha) = Z_k} on
#'   the design's upper boundary.
#' * Nominal boundaries are `pnorm(gsDesign(...)$upper$bound, lower.tail =
#'   FALSE)` with the design rebuilt at the allocated significance level.
#' * A hypothesis with a single analysis uses the univariate results directly,
#'   since `gsDesign()` requires at least two analyses.
#'
#' The spending time is always passed to gsDesign explicitly (`usTime` if
#' supplied, otherwise `pmin(info_frac, 1)`), because `sequentialPValue()`'s
#' default rescales it by the last observed information and would change the
#' interim spending when a trial over-runs its planned information. `beta` is
#' set to `(1 - alpha) / 2` in every design call: it does not affect the upper
#' boundary for `test.type = 1` and only has to satisfy `alpha + beta < 1`.
#'
#' @param alpha Overall significance level, used to build the per-hypothesis
#'   design objects read by `sequentialPValue()` (the level actually tested is
#'   supplied per call).
#' @param info_frac Information fraction matrix (m x K), `NA`-padded.
#' @param sfu A list of m upper spending functions in gsDesign's convention,
#'   each either a spending function such as `gsDesign::sfLDOF` or one of the
#'   strings `"OF"`, `"Pocock"`, `"WT"`.
#' @param sfupar A list of m spending function parameters (`NULL` to use
#'   gsDesign's default).
#' @param usTime `NULL`, or an m x K matrix of spending times with `NA` in the
#'   same positions as `info_frac`.
#'
#' @return A list with the functions `repeated_p` and `nominal_bounds`; see
#'   [gsd_engine_mvtnorm()].
#'
#' @keywords internal
gsd_engine_gsDesign <- function(alpha, info_frac, sfu, sfupar, usTime) {
  num_hyps <- nrow(info_frac)
  # Search interval for repeated p-values, matching repeated_p()'s conventions
  p_lower <- 1e-6
  p_upper <- 1 - 1e-6

  # Per-hypothesis full analysis schedule (non-NA analyses only)
  sched <- lapply(seq_len(num_hyps), function(j) {
    ok <- !is.na(info_frac[j, ])
    t_j <- unname(info_frac[j, ok])
    ust_j <- if (is.null(usTime)) pmin(t_j, 1) else unname(usTime[j, ok])
    list(
      t = t_j, ust = ust_j, K = length(t_j),
      classical = is.character(sfu[[j]])
    )
  })

  # gsDesign's own default parameter, used when none is supplied. formals()
  # returns the default as an unevaluated expression (`-4` is a call to unary
  # minus), so evaluate it to obtain the numeric value.
  default_sfupar <- eval(formals(gsDesign::gsDesign)$sfupar)
  sfupar_j <- function(j) {
    if (is.null(sfupar[[j]])) default_sfupar else sfupar[[j]]
  }

  # Full design for hypothesis j at significance level `a`
  build_design <- function(j, a) {
    s <- sched[[j]]
    args <- list(
      k = s$K, test.type = 1, alpha = a, beta = (1 - a) / 2,
      sfu = sfu[[j]], sfupar = sfupar_j(j),
      n.I = s$t, maxn.IPlan = 1
    )
    # Spending time only applies to spending-function designs
    if (!s$classical) args$usTime <- s$ust
    d <- do.call(gsDesign::gsDesign, args)
    # sequentialPValue() evaluates the spending function stored in the design
    # (`upper$sf`), which gsDesign's own sf* functions set to themselves. Store
    # the supplied function so that a user-defined spending function built on
    # gsDesign::spendingFunction() is used consistently even if it leaves that
    # element at the template's default.
    if (!s$classical) d$upper$sf <- sfu[[j]]
    d
  }

  # sequentialPValue() only reads the spending function and schedule from the
  # design (the level is searched over), so one design per hypothesis suffices
  cache <- new.env(parent = emptyenv())
  cached_design <- function(j) {
    key <- as.character(j)
    if (is.null(cache[[key]])) cache[[key]] <- build_design(j, alpha)
    cache[[key]]
  }

  # Cumulative spend of hypothesis j at level `a` and spending time `st`
  spend <- function(j, a, st) sfu[[j]](a, st, sfupar_j(j))$spend

  # Smallest level at which `exceed()` (increasing in the level) becomes
  # non-negative, with the same conventions as repeated_p(): 1 if the
  # boundary is never crossed, p_lower if it is crossed already at p_lower.
  find_level <- function(exceed) {
    up <- tryCatch(exceed(p_upper), error = function(e) NA_real_)
    if (is.na(up) || up <= 0) {
      return(1)
    }
    lo <- tryCatch(exceed(p_lower), error = function(e) -1)
    if (lo >= 0) {
      return(p_lower)
    }
    stats::uniroot(exceed, c(p_lower, p_upper), tol = 1e-8)$root
  }

  list(
    repeated_p = function(p, info_frac, j) {
      s <- sched[[j]]
      k <- length(p)
      stopifnot(isTRUE(all.equal(unname(info_frac), s$t[seq_len(k)])))
      z <- pmin(pmax(stats::qnorm(1 - p), -20), 20)

      if (s$K == 1) {
        # Single analysis: the boundary is the cumulative spend at its
        # spending time (or the full level for a classical family)
        if (s$classical) {
          return(p[1])
        }
        return(find_level(function(a) spend(j, a, s$ust[1]) - p[1]))
      }

      if (s$classical) {
        return(find_level(function(a) {
          z[k] - build_design(j, a)$upper$bound[k]
        }))
      }

      zz <- z
      if (k > 1) zz[seq_len(k - 1)] <- -20
      # At very small levels the cumulative spend at an early spending time
      # underflows to exactly 0, which sequentialPValue() rejects ("Final
      # spend must be > 0"). Raise the lower end of the search interval to the
      # smallest level with a positive spend; a p-value that crosses the
      # boundary already at that level is reported as that level, as
      # repeated_p() does at its own lower bound.
      lo <- p_lower
      while (lo < 0.5 && spend(j, lo, s$ust[k]) <= 0) lo <- lo * 2
      val <- gsDesign::sequentialPValue(
        gsD = cached_design(j),
        n.I = s$t[seq_len(k)],
        Z = zz,
        usTime = s$ust[seq_len(k)],
        interval = c(lo, p_upper)
      )
      if (val >= p_upper) 1 else val
    },
    nominal_bounds = function(alpha, info_frac, j) {
      s <- sched[[j]]
      k <- length(info_frac)
      if (alpha <= 0) {
        return(rep(0, k))
      }
      if (alpha >= 1) {
        return(rep(1, k))
      }
      if (s$K == 1) {
        return(if (s$classical) alpha else spend(j, alpha, s$ust[1]))
      }
      b <- build_design(j, alpha)$upper$bound[seq_len(k)]
      stats::pnorm(b, lower.tail = FALSE)
    }
  )
}
