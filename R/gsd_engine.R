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


#' Group sequential engine backed by rpact
#'
#' @description
#' `gsd_engine_rpact()` builds the engine used by [graph_test_shortcut_rpact()].
#' It has the same two-function interface as [gsd_engine_mvtnorm()] and obtains
#' every group sequential quantity from [rpact::getDesignGroupSequential()]:
#' * One design per hypothesis at the allocated level encodes its analysis
#'   schedule, `getDesignGroupSequential(kMax = K_j, informationRates, alpha,
#'   sided = 1, typeOfDesign, ...)`. Nominal boundaries are its `stageLevels`.
#' * Repeated p-values are found by solving \eqn{c_k(\alpha) = Z_k} on the
#'   design's `criticalValues` over the level. This is the algorithm that
#'   rpact's own `getRepeatedPValues()` uses internally (a bisection over the
#'   level with the design rebuilt at each step), applied without its ceiling
#'   of 0.5 and to every design type.
#' * rpact evaluates the spending function at the design's information rates
#'   and does not accept rates above 1. A spending time that differs from the
#'   information fractions, or information fractions above 1 (over-running
#'   information), is therefore expressed through an `"asUser"` design: the
#'   cumulative spending is taken from a design at the spending time
#'   (`alphaSpent`) and passed as `userAlphaSpending` to a design at the
#'   information fractions, which are divided by their maximum when they
#'   exceed 1 (the correlation between analyses depends only on their
#'   ratios). This does not apply to the classical boundary families, which
#'   are defined by their boundary shape.
#' * A hypothesis with a single analysis below full information uses the
#'   first stage of a two-stage design ending at 1; at full information it is
#'   a fixed-sample test.
#'
#' rpact warns when a level lies outside its validated range `[1e-6, 0.5)`,
#' which the engine muffles, and cannot compute designs at levels above
#' roughly 0.65 ("critical values cannot be calculated"). The engine searches
#' up to the highest level rpact can compute and reports a repeated p-value
#' of 1 when the boundary is not crossed there.
#'
#' @inheritParams gsd_engine_gsDesign
#' @param typeOfDesign A list of m design types in rpact's convention, e.g.
#'   `"asOF"`, `"asP"`, `"asHSD"`, `"asKD"`, `"asUser"`, `"OF"`, `"P"`,
#'   `"WT"`.
#' @param gammaA,deltaWT,userAlphaSpending Lists of m design parameters
#'   (`NULL` where not applicable): `gammaA` for `"asHSD"` and `"asKD"`,
#'   `deltaWT` for `"WT"`, and the cumulative alpha spent at each analysis
#'   for `"asUser"`, scaled proportionally to the allocated level.
#' @param spending_time `NULL`, or an m x K matrix of spending times with `NA`
#'   in the same positions as `info_frac`.
#'
#' @return A list with the functions `repeated_p` and `nominal_bounds`; see
#'   [gsd_engine_mvtnorm()].
#'
#' @keywords internal
gsd_engine_rpact <- function(alpha, info_frac, typeOfDesign, gammaA, deltaWT,
                             userAlphaSpending, spending_time) {
  num_hyps <- nrow(info_frac)
  # Search interval for repeated p-values, matching repeated_p()'s conventions
  p_lower <- 1e-6
  p_upper <- 1 - 1e-6

  # Per-hypothesis full analysis schedule (non-NA analyses only)
  sched <- lapply(seq_len(num_hyps), function(j) {
    ok <- !is.na(info_frac[j, ])
    t_j <- unname(info_frac[j, ok])
    type <- typeOfDesign[[j]]
    is_classical <- type %in% gsd_rpact_classical
    is_user <- identical(type, "asUser")
    st_j <- if (is.null(spending_time)) t_j else unname(spending_time[j, ok])
    st_j <- pmin(st_j, 1)
    # Information rates for the correlation: rpact requires them to be at
    # most 1, and only their ratios matter
    rates <- if (max(t_j) > 1) t_j / max(t_j) else t_j
    list(
      t = t_j, st = st_j, rates = rates, K = length(t_j), type = type,
      classical = is_classical, user = is_user,
      # Spending at a time other than the information rates needs the
      # "asUser" construction
      separate = !is_classical && !is_user && !isTRUE(all.equal(st_j, rates)),
      # A single analysis at full information (or of a classical family) is
      # a fixed-sample test
      fixed = length(t_j) == 1 && (is_classical || t_j[1] >= 1)
    )
  })

  # A single analysis below full information is the first stage of a
  # two-stage design ending at 1
  pad <- function(r) if (length(r) == 1 && r[1] < 1) c(r, 1) else r

  design_args <- function(j, a, rates, type) {
    args <- list(
      kMax = length(rates), informationRates = rates, alpha = a, sided = 1,
      typeOfDesign = type
    )
    if (type %in% c("asHSD", "asKD")) args$gammaA <- gammaA[[j]]
    if (identical(type, "WT")) args$deltaWT <- deltaWT[[j]]
    args
  }

  # rpact warns twice per design about levels outside [1e-6, 0.5), and about
  # user spending values that underflow at early analyses; the graphical
  # procedure evaluates such levels routinely
  get_design <- function(args) {
    withCallingHandlers(
      do.call(rpact::getDesignGroupSequential, args),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("validated bounds", msg, fixed = TRUE) ||
          grepl("imprecise critical values", msg, fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  # Cumulative spending of hypothesis j at level `a` at its spending time
  spend_at <- function(j, a) {
    s <- sched[[j]]
    if (s$K == 1) {
      if (s$st[1] >= 1) {
        return(a)
      }
      return(get_design(design_args(j, a, c(s$st[1], 1), s$type))$alphaSpent[1])
    }
    get_design(design_args(j, a, s$st, s$type))$alphaSpent
  }

  # Design for hypothesis j at level `a`, or NULL where rpact cannot compute
  # it. Near its limits rpact returns degenerate designs rather than failing
  # (critical values at its internal floor of about 7e-9, or negative), which
  # are treated as not computable as well.
  build_design <- function(j, a) {
    s <- sched[[j]]
    d <- tryCatch(
      {
        rates <- pad(s$rates)
        if (s$user || s$separate) {
          spent <- if (s$user) userAlphaSpending[[j]] * a / alpha else spend_at(j, a)
          if (length(spent) < length(rates)) spent <- c(spent, a)
          args <- design_args(j, a, rates, "asUser")
          args$userAlphaSpending <- pmin(spent, a)
          get_design(args)
        } else {
          get_design(design_args(j, a, rates, s$type))
        }
      },
      error = function(e) NULL
    )
    if (is.null(d) || anyNA(d$criticalValues) || any(d$criticalValues <= 1e-6)) {
      return(NULL)
    }
    d
  }

  # Highest level at which rpact can compute the design of hypothesis j
  ceiling_cache <- new.env(parent = emptyenv())
  level_ceiling <- function(j) {
    key <- as.character(j)
    if (is.null(ceiling_cache[[key]])) {
      candidates <- c(p_upper, seq(0.95, 0.05, by = -0.05))
      found <- NA_real_
      for (a in candidates) {
        if (!is.null(build_design(j, a))) {
          found <- a
          break
        }
      }
      if (is.na(found)) {
        stop(
          "rpact could not compute a group sequential design for hypothesis ",
          j, " at any level."
        )
      }
      ceiling_cache[[key]] <- found
    }
    ceiling_cache[[key]]
  }

  # Critical value at analysis k; an infinite value (a boundary that cannot
  # be crossed) is capped so that the root search sees a finite function
  crit_at <- function(j, a, k) {
    d <- build_design(j, a)
    if (is.null(d)) NA_real_ else min(d$criticalValues[k], 40)
  }

  # Smallest level at which `exceed()` (increasing in the level, NA where
  # rpact cannot compute the design) becomes non-negative, with the same
  # conventions as repeated_p(): 1 if the boundary is not crossed at the
  # highest computable level, p_lower if it is crossed already at p_lower.
  find_level <- function(j, exceed) {
    hi <- level_ceiling(j)
    up <- exceed(hi)
    if (is.na(up) || up <= 0) {
      return(1)
    }
    lo <- p_lower
    lo_val <- exceed(lo)
    while (is.na(lo_val) && lo < hi / 2) {
      lo <- lo * 2
      lo_val <- exceed(lo)
    }
    if (is.na(lo_val)) {
      stop("rpact could not compute the group sequential design for hypothesis ", j, ".")
    }
    if (lo_val >= 0) {
      return(lo)
    }
    stats::uniroot(exceed, c(lo, hi), tol = 1e-8)$root
  }

  list(
    repeated_p = function(p, info_frac, j) {
      s <- sched[[j]]
      k <- length(p)
      stopifnot(isTRUE(all.equal(unname(info_frac), s$t[seq_len(k)])))
      if (s$fixed) {
        return(p[1])
      }
      z <- pmin(pmax(stats::qnorm(1 - p), -20), 20)
      find_level(j, function(a) z[k] - crit_at(j, a, k))
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
      if (s$fixed) {
        return(alpha)
      }
      d <- build_design(j, alpha)
      if (is.null(d)) {
        stop(
          "rpact could not compute the group sequential design for hypothesis ",
          j, " at level ", alpha, "."
        )
      }
      d$stageLevels[seq_len(k)]
    }
  )
}

# rpact design types: the classical boundary families are defined by their
# boundary shape rather than by a spending function
gsd_rpact_classical <- c("OF", "P", "WT", "PT", "HP", "WToptimum", "noEarlyEfficacy")
gsd_rpact_spending <- c("asOF", "asP", "asHSD", "asKD", "asUser")
