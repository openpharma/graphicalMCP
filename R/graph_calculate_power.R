#' Calculate power values for a graphical multiple comparison procedure
#'
#' @description
#' Under the alternative hypotheses, the distribution of test statistics is
#' assumed to be a multivariate normal distribution. Given this distribution,
#' this function calculates power values for a graphical multiple comparison
#' procedure. By default, it calculate the local power, which is the probability
#' to reject an individual hypothesis, the probability to reject at least one
#' hypothesis, the probability to reject all hypotheses, the expected number of
#' rejections, and the probability of user-defined success criteria.
#' See `vignette("shortcut-testing")` and `vignette("closed-testing")` for more
#' illustration of power calculation.
#'
#' @inheritParams graph_test_closure
#' @param alpha A numeric value of the one-sided overall significance level,
#'   which should be between 0 & 1. The default is 0.025 for one-sided
#'   hypothesis testing. Note that only one-sided tests are supported.
#' @param sim_n An integer scalar specifying the number of simulations. The
#'   default is 1e5.
#' @param power_marginal A numeric vector of marginal power values to use when
#'   simulating p-values. See Details for more on the simulation process.
#' @param sim_corr A numeric matrix of correlations between test statistics for
#'   all hypotheses. The dimensions should match the number of hypotheses in
#'   `graph`.
#' @param sim_success A list of user-defined functions to specify the success
#'   criteria. Functions must take one simulation's logical vector of results as
#'   an input, and return a length-one logical vector. For instance, if
#'   "success" means rejecting hypotheses 1 and 2, use `sim_success = list("1
#'   and 2" = function(x) x[1] && x[2])`. If the list is not named, the function
#'   body will be used as the name. Lambda functions also work starting with R
#'   4.1, e.g. `sim_success = list(\(x) x[3] || x[4])`.
#' @param verbose A logical scalar specifying whether the details of power
#'   simulations should be included in results. The default is `verbose =
#'   FALSE`.
#'
#' @return A `power_report` object with a list of 3 elements:
#'   * `inputs` - Input parameters, which is a list of:
#'     * `graph` - Initial graph,
#'     * `alpha` - Overall significance level,
#'     * `test_groups` - Groups of hypotheses for different types of tests,
#'     * `test_types` - Different types of tests,
#'     * `test_corr` - Correlation matrices for parametric tests,
#'     * `sim_n` - Number of simulations,
#'     * `power_marginal` - Marginal power of all hypotheses
#'     * `sim_corr` - Correlation matrices for simulations,
#'     * `sim_success` - User-defined success criteria.
#'   * `power` - A list of power values
#'     * `power_local` - Local power of all hypotheses, which is the proportion
#'   of simulations in which each hypothesis is rejected,
#'     * `rejection_expected` - Expected (average) number of rejected hypotheses,
#'     * `power_at_least_1` - Power to reject at least one hypothesis,
#'     * `power_all` - Power to reject all hypotheses,
#'     * `power_success` - Power of user-defined success, which is the
#'   proportion of simulations in which the user-defined success criterion
#'     * `sim_success` is met.
#'   * `details` - An optional list of datasets showing simulated p-values and
#'   results for each simulation.
#'
#' @section Simulation details: The power calculation is based on simulations.
#'   The distribution to simulate from is determined as a multivariate normal
#'   distribution by `power_marginal` and `sim_corr`. In particular,
#'   `power_marginal` is a vector of marginal power values for all hypotheses.
#'   The marginal power is the power to reject the null hypothesis at the
#'   significance level `alpha` *without multiplicity adjustment*. This value could be readily available from
#'   standard software and other R packages. Then we can determine the mean of
#'   the multivariate normal distribution as
#'   \deqn{\Phi^{-1}\left(1-\alpha\right)-\Phi^{-1}\left(1-d_i\right)}, which is
#'   often called the non-centrality parameter or the drift parameter. Here
#'   \eqn{d_i} is the marginal power `power_marginal` of hypothesis \eqn{i}.
#'   Given the correlation matrix `sim_corr`, we can simulate from this
#'   multivariate normal distribution using the `mvtnorm` R package (Genz and
#'   Bretz, 2009).
#'
#'   Each set simulated values can be used to calculate the corresponding
#'   one-sided p-values. Then this set of p-values are plugged into the
#'   graphical multiple comparison procedure to determine which hypotheses are
#'   rejected. This process is repeated `n_sim` times to produce the power
#'   values as the proportion of simulations in which a particular success
#'   criterion is met.
#'
#' @rdname graph_calculate_power
#'
#' @export
#'
#' @references Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W.,
#'   and Rohmeyer, K. (2011a). Graphical approaches for multiple comparison
#'   procedures using weighted Bonferroni, Simes, or parametric tests.
#'   \emph{Biometrical Journal}, 53(6), 894-913.
#'
#'   Bretz, F., Maurer, W., and Hommel, G. (2011b). Test and power
#'   considerations for multiple endpoint analyses using sequentially rejective
#'   graphical procedures. \emph{Statistics in Medicine}, 30(13), 1489-1501.
#'
#'   Genz, A., and Bretz, F. (2009). \emph{Computation of Multivariate Normal
#'   and t Probabilities}, series Lecture Notes in Statistics. Springer-Verlag,
#'   Heidelberg.
#'
#'   Lu, K. (2016). Graphical approaches using a Bonferroni mixture of weighted
#'   Simes tests. \emph{Statistics in Medicine}, 35(22), 4041-4055.
#'
#'   Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
#'   for weighted parametric multiple test procedures. \emph{Biometrical
#'   Journal}, 59(5), 918-931.
#'
#' @examples
#' # A graphical multiple comparison procedure with two primary hypotheses (H1
#' # and H2) and two secondary hypotheses (H3 and H4)
#' # See Figure 4 in Bretz et al. (2011a).
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
#'
#' # Parametric tests for H1 and H2; Simes tests for H3 and H4
#' # User-defined success: to reject H1 or H2; to reject H1 and H2
#' # Reduce the number of simulations to save time for package compilation
#' graph_calculate_power(
#'   g,
#'   alpha,
#'   test_groups = list(1:2, 3:4),
#'   test_types = c("parametric", "simes"),
#'   test_corr = list(corr1, NA),
#'   sim_n = 1e2,
#'   sim_success = list(
#'     function(.) .[1] || .[2],
#'     function(.) .[1] && .[2]
#'   )
#' )
#'
graph_calculate_power <- function(graph,
                                  alpha = 0.025,
                                  power_marginal =
                                    rep(alpha, length(graph$hypotheses)),
                                  test_groups =
                                    list(seq_along(graph$hypotheses)),
                                  test_types = c("bonferroni"),
                                  test_corr = rep(list(NA), length(test_types)),
                                  sim_n = 1e5,
                                  sim_corr = diag(length(graph$hypotheses)),
                                  sim_success = NULL,
                                  verbose = FALSE) {
  # Input sanitization ---------------------------------------------------------
  # Test types should be passed as full names or first letter, case-insensitive,
  # and a single provided type should get expanded to all groups
  test_types_names <- names(test_types)
  test_opts <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    hochberg = "hochberg",
    b = "bonferroni",
    p = "parametric",
    s = "simes",
    h = "hochberg"
  )
  test_types <- test_opts[tolower(test_types)]
  names(test_types) <- test_types_names
  if (length(test_types) == 1) {
    test_types <- rep(test_types, length(test_groups))
  }

  # Groups of size 1 should always use Bonferroni testing
  test_types[lengths(test_groups) == 1] <- "bonferroni"

  # A bare success function should get put into a length-one list
  if (is.function(sim_success)) sim_success <- list(sim_success)

  hyp_names <- names(graph$hypotheses)
  num_hyps <- length(graph$hypotheses)

  # Input validation -----------------------------------------------------------
  test_input_val(
    graph,
    rep(alpha, length(graph$hypotheses)),
    alpha,
    test_groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  # The test specification arguments can be named or not. However, if
  # `test_groups` is named, all of them must be named. The other two are
  # re-ordered to match `test_groups`
  if (!is.null(names(test_groups))) {
    if (!all(names(c(test_types, test_corr)) %in% names(test_groups))) {
      stop("If `test_groups` is named, `test_types` and `test_corr` must use the
           same names")
    } else {
      test_types <- test_types[names(test_groups)]
      test_corr <- test_corr[names(test_groups)]
    }
  } else {
    names(test_groups) <-
      names(test_types) <-
      names(test_corr) <-
      paste0("grp", seq_along(test_groups))
  }

  # Correlation matrix input is easier for end users to input as a list, but
  # it's easier to work with internally as a full matrix, potentially with
  # missing values. This puts all the correlation pieces into one matrix
  new_corr <- matrix(NA, num_hyps, num_hyps)

  for (group_num in seq_along(test_groups)) {
    new_corr[test_groups[[group_num]], test_groups[[group_num]]] <-
      test_corr[[group_num]]
  }
  diag(new_corr) <- 1
  test_corr <- if (any(test_types == "parametric")) new_corr else NULL

  power_input_val(graph, sim_n, power_marginal, sim_corr, sim_success)

  # Simulated p-values are generated by sampling from the multivariate normal
  # distribution. The means are set with `power_marginal`, and the correlations
  # are set with `sim_corr`. Random samples are converted to p-values with a
  # one-sided test.
  noncentrality_parameter <-
    stats::qnorm(1 - alpha, lower.tail = TRUE) -
    stats::qnorm(1 - power_marginal, lower.tail = TRUE)

  p_sim <- stats::pnorm(
    mvtnorm::rmvnorm(
      sim_n,
      noncentrality_parameter,
      sigma = sim_corr
    ),
    lower.tail = FALSE
  )

  simulation_test_results <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(power_marginal),
    dimnames = list(seq_len(sim_n), hyp_names)
  )

  # Calculate weights for each intersection in the closure -------------------
  weighting_strategy <- graph_generate_weights(graph)
  matrix_intersections <- weighting_strategy[, seq_len(num_hyps), drop = FALSE]
  matrix_weights <-
    weighting_strategy[, seq_len(num_hyps) + num_hyps, drop = FALSE]

  if (all(test_types == "bonferroni")) {
    for (row in seq_len(sim_n)) {
      simulation_test_results[row, ] <- graph_test_shortcut_fast(
        p_sim[row, ],
        alpha,
        matrix_weights
      )
    }
  } else {
    # Calculate Bonferroni adjusted weights ------------------------------------
    groups_bonferroni <- test_groups[test_types == "bonferroni", drop = FALSE]

    # Bonferroni adjusted weights are just the weights from the closure
    adjusted_weights_bonferroni <-
      matrix_weights[, unlist(groups_bonferroni), drop = FALSE]

    # Calculate parametric adjusted weights ------------------------------------
    groups_parametric <- test_groups[test_types == "parametric", drop = FALSE]

    # Parametric adjusted weights depend only on the joint distribution and
    # alpha. This allows adjusted weights to be calculated once, rather than
    # re-calculating for each simulation
    adjusted_weights_parametric <- adjust_weights_parametric_util(
      matrix_weights,
      matrix_intersections,
      test_corr,
      alpha,
      groups_parametric
    )

    # Separate Simes weighting strategy ----------------------------------------
    groups_simes <- test_groups[test_types == "simes", drop = FALSE]

    # The fastest option found for calculating Simes adjusted weights requires
    # missing hypotheses' weights to be 0, rather than NA
    matrix_weights_simes <-
      matrix_weights[, unlist(groups_simes), drop = FALSE]

    # Unlike Bonferroni and parametric adjusted weights, Simes adjusted weights
    # depend on the order of p-values. This means they must be re-calculated for
    # each simulation. Because this causes a bottleneck in calculations, Simes
    # testing has been heavily optimized. Fast Simes testing requires Simes
    # hypothesis numbers to be mapped to their relative position within the set
    # of all Simes hypotheses. For example, if hypotheses 1/7 form a parametric
    # group, and 2/5 & 3/4/6 each form a Simes group, the fast Simes functions
    # will get hypotheses 2/5 & 3/4/6 passed, but the groups must first be
    # re-indexed to 1/4 & 2/3/5 (their relative locations within all Simes
    # groups).
    groups_simes_reduce <- lapply(
      groups_simes,
      function(group) which(unlist(groups_simes) %in% group)
    )

    # Fast Simes testing also requires a set of p-values with columns already
    # subset for Simes testing, similar to how the weighting strategy is subset
    # for each test type
    p_sim_simes <- p_sim[, unlist(groups_simes), drop = FALSE]

    # Separate Hochberg weighting strategy -------------------------------------
    # All Simes comments apply to Hochberg
    groups_hochberg <- test_groups[test_types == "hochberg", drop = FALSE]

    matrix_weights_hochberg <-
      matrix_weights[, unlist(groups_hochberg), drop = FALSE]

    matrix_intersections_hochberg <-
      matrix_intersections[, unlist(groups_hochberg), drop = FALSE]

    groups_hochberg_reduce <- lapply(
      groups_hochberg,
      function(group) which(unlist(groups_hochberg) %in% group)
    )

    p_sim_hochberg <- p_sim[, unlist(groups_hochberg), drop = FALSE]

    # Apply closure testing to each simulation ---------------------------------
    for (row in seq_len(sim_n)) {
      # If there are no Simes groups, adjusted weights are the Simes weighting
      # strategy (a matrix with 0 columns)
      if (length(groups_simes) == 0) {
        adjusted_weights_simes <- matrix_weights_simes
      } else {
        # Simes testing depends on p-values, so adjusted weights must be
        # calculated for each simulation.
        adjusted_weights_simes <- adjust_weights_simes(
          matrix_weights_simes,
          p_sim_simes[row, ],
          groups_simes_reduce
        )

        # *Note:* The Simes adjusted weights are incorrect for missing Simes
        # hypotheses. To improve performance, missing hypotheses are given a
        # zero value rather than NA before calculating adjusted weights. This
        # results in missing hypotheses getting an adjusted weight calculated
        # for them. These incorrect values are then replaced with zeroes for
        # testing
      }

      # All Simes comments apply to similar lines for Hochberg
      if (length(groups_hochberg) == 0) {
        adjusted_weights_hochberg <- matrix_weights_hochberg
      } else {
        adjusted_weights_hochberg <- adjust_weights_hochberg(
          matrix_weights_hochberg,
          matrix_intersections_hochberg,
          p_sim_hochberg[row, ],
          groups_hochberg_reduce
        )
      }

      # `graph_test_closure_fast()` requires hypotheses, p-values, and the
      # intersections matrix to all have hypotheses/columns in the same order.
      # P-values and the intersections matrix are already in the original order,
      # so order the adjusted weights back in original hypothesis order.
      adjusted_weights_all <- cbind(
        adjusted_weights_bonferroni,
        adjusted_weights_simes,
        adjusted_weights_hochberg,
        adjusted_weights_parametric
      )[, hyp_names, drop = FALSE]

      # Similar to Simes adjusted weights, the optimized testing function
      # requires missing values to be replaced by zero. This line also replaces
      # the incorrect Simes and Hochberg adjusted weights with zero.
      adjusted_weights_all[!matrix_intersections] <- 0

      # Record test results for one simulation, all groups
      simulation_test_results[row, ] <- graph_test_closure_fast(
        p_sim[row, ],
        alpha,
        adjusted_weights_all,
        matrix_intersections
      )
    }
  }

  # Summarize power results ----------------------------------------------------
  # Each user-defined function provided as a "success" measure should take a
  # logical vector (a single simulation's test results) as input, and return a
  # logical scalar. Applying such a function to each simulation, results in a
  # success indicator vector with one entry per simulation. The average of this
  # vector is the probability of "success".
  power_success <- vapply(
    sim_success,
    function(fn_success) mean(apply(simulation_test_results, 1, fn_success)),
    numeric(1)
  )

  # If the success functions are not named, set names according to each
  # function's body
  if (is.null(names(power_success))) {
    success_fun_bodies <- vapply(
      sim_success,
      function(fn_success) deparse(fn_success)[[2]],
      character(1)
    )

    names(power_success) <- success_fun_bodies
  }

  # Power summaries:
  # * Local power is the probability of rejecting each individual hypothesis:
  # Mean of results for each hypothesis individually.
  # * Expected rejections is the total number of rejections divided by the total
  # possible rejections.
  # * Power to reject at least one hypothesis is the probability that any result
  # in a row is TRUE. This one is just like if a success function was defined as
  # rejecting any hypothesis in the graph
  # * Power to reject all hypotheses is the mean of a success vector where
  # success is only triggered when the whole results vector is TRUE
  power <- list(
    power_local = colMeans(simulation_test_results),
    rejection_expected = sum(simulation_test_results) / sim_n,
    power_at_least_1 = mean(rowSums(simulation_test_results) > 0),
    power_all =
      mean(rowSums(simulation_test_results) == length(power_marginal)),
    power_success = power_success
  )

  # The core output of a power report is the 5 power summaries. It also includes
  # the main testing and simulation input parameters (similar to test results).
  # For completion, the full matrix of simulations and corresponding matrix of
  # test results are included. They are truncated in the print method so as to
  # not blow up output space. It may be preferred for these to be an optional
  # output with e.g. `verbose = TRUE/FALSE`.
  structure(
    list(
      inputs = list(
        graph = graph,
        alpha = alpha,
        test_groups = test_groups,
        test_types = test_types,
        test_corr = test_corr,
        sim_n = sim_n,
        power_marginal = power_marginal,
        sim_corr = sim_corr,
        sim_success = sim_success
      ),
      power = power,
      details = if (verbose) {
        list(
          p_sim = p_sim,
          test_results = simulation_test_results
        )
      }
    ),
    class = "power_report"
  )
}
