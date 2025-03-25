meta_test_graph <- function(...) {
  args <- list(...)

  valid_inputs <- list(
    graph = simple_successive_1(),
    p = c(.001, .02, .002, .03),
    alpha = .025,
    test_groups = list(1, 2, 3:4),
    test_types = c("bonferroni", "simes", "parametric"),
    test_corr = list(NA, NA, rbind(c(1, 0), c(0, 1))),
    verbose = TRUE,
    test_values = TRUE
  )

  if (length(args) > 0) {
    spliced_inputs <- c(
      valid_inputs[!names(valid_inputs) %in% names(args)],
      args
    )
  } else {
    spliced_inputs <- valid_inputs
  }

  do.call(graph_test_closure, spliced_inputs)
}

test_that("invalid test inputs throw errors", {
  par_gate <- simple_successive_1()
  par_gate_inval <- unclass(par_gate)

  p_val <- c(.001, .01, .002, .03)
  p_inval1 <- as.character(p_val)
  p_inval2 <- c(-.001, .01, .002, 1.03)
  p_inval3 <- p_val[1:3]

  alpha_inval1 <- ".025"
  alpha_inval2 <- c(.025, .05)
  alpha_inval3 <- 1.1

  tests_inval1 <- c("bonferoni", "simes", "parametric")
  tests_inval2 <- c("bonferroni", "simes")

  groups_inval1 <- list(1, 1, 3:4)
  groups_inval2 <- list(1, 3:4)
  groups_inval3 <- c(1, 2, 3, 4)

  verbose_inval <- c(TRUE, FALSE)

  test_values_inval <- 1

  corr_inval1 <- list(NA, NA, rbind(c(1, .01), c(0, 1)))
  corr_inval2 <- list(diag(2))
  corr_inval3 <- list(
    NA,
    rbind(
      c(1, .997, .929),
      c(.997, 1, .769),
      c(.929, .769, 1)
    )
  )

  expect_s3_class(meta_test_graph(), "graph_report")

  expect_error(meta_test_graph(graph = par_gate_inval))
  expect_error(meta_test_graph(p = p_inval1))
  expect_error(meta_test_graph(p = p_inval2))
  expect_error(meta_test_graph(p = p_inval3))
  expect_error(meta_test_graph(alpha = alpha_inval1))
  expect_error(meta_test_graph(alpha = alpha_inval2))
  expect_error(meta_test_graph(alpha = alpha_inval3))
  expect_error(meta_test_graph(test_types = tests_inval1))
  expect_error(meta_test_graph(test_groups = groups_inval1))
  expect_error(
    meta_test_graph(
      test_types = tests_inval2,
      test_groups = groups_inval2
    )
  )
  expect_error(meta_test_graph(test_groups = groups_inval3))
  expect_error(meta_test_graph(verbose = verbose_inval))
  expect_error(meta_test_graph(test_values = test_values_inval))
  expect_error(meta_test_graph(test_corr = corr_inval1))
  expect_error(meta_test_graph(test_corr = corr_inval2))
  expect_error(
    meta_test_graph(
      test_groups = list(1, 2:4),
      test_types = c("b", "p"),
      test_corr = corr_inval3
    )
  )
  expect_error(
    meta_test_graph(
      test_groups = list(f1 = 1, f2 = 2:4),
      test_types = c(f1 = "b", f2 = "p"),
      test_corr = list(f1 = NA, f3 = matrix(0, 3, 3))
    )
  )
  expect_s3_class(
    meta_test_graph(
      test_groups = list(f1 = 1:2, f2 = 3:4),
      test_types = c(f2 = "b", f1 = "s"),
      test_corr = list(f2 = NA, f1 = NA)
    ),
    "graph_report"
  )
})

test_that("adjusted p-values are capped at 1", {
  expect_equal(
    graph_test_closure(random_graph(2), c(1, 1))$outputs$adjusted_p,
    c(H1 = 1, H2 = 1)
  )
})

test_that("Non-Bonferroni adjusted p-values are less than Bonferroni", {
  rando <- random_graph(4)

  expect_true(
    all(
      graph_test_closure(rando, rep(.01, 4))$outputs$adjusted_p >=
        graph_test_closure(
          rando,
          rep(.01, 4),
          test_types = "s"
        )$outputs$adjusted_p
    )
  )

  expect_true(
    all(
      graph_test_closure(bonferroni_holm(rep(1/4, 4)),
                         rep(.01, 4))$outputs$adjusted_p >=
        graph_test_closure(
          bonferroni_holm(rep(1/4, 4)),
          rep(.01, 4),
          test_types = "h"
        )$outputs$adjusted_p
    )
  )

  expect_true(
    all(
      graph_test_closure(rando, rep(.01, 4))$outputs$adjusted_p >=
        graph_test_closure(
          rando,
          rep(.01, 4),
          test_types = "p",
          test_corr = list(diag(4))
        )$outputs$adjusted_p
    )
  )
})

test_that("verbose/test values output is only present when asked for", {
  rando <- random_graph(6)

  expect_null(graph_test_closure(rando, rep(.01, 6))$details)
  expect_null(graph_test_closure(rando, rep(.01, 6))$test_values)

  expect_s3_class(
    graph_test_closure(
      rando,
      rep(.01, 6),
      verbose = TRUE
    )$details$results,
    "data.frame"
  )
  expect_s3_class(
    graph_test_closure(
      rando,
      rep(.01, 6),
      test_values = TRUE
    )$test_values$results,
    "data.frame"
  )
})

test_that("check assertions in testing vignette", {
  par_gate <- simple_successive_1(c("A1", "A2", "B1", "B2"))
  pvals <- c(.024, .01, .026, .027)

  expect_equal(
    graph_test_closure(par_gate, p = pvals, alpha = .05)$outputs,
    list(
      adjusted_p = c(.048, .02, .052, .052),
      rejected = c(TRUE, TRUE, FALSE, FALSE),
      graph = graph_update(par_gate, c(TRUE, TRUE, FALSE, FALSE))$updated_graph
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    graph_test_closure(
      par_gate,
      p = pvals,
      alpha = .05,
      test_types = "s"
    )$outputs,
    list(
      adjusted_p = c(.027, .02, .027, .027),
      rejected = rep(TRUE, 4),
      graph = graph_update(par_gate, rep(TRUE, 4))$updated_graph
    ),
    ignore_attr = TRUE
  )

  corr1 <- list(NA, matrix(1, nrow = 2, ncol = 2))
  corr1[[2]][1, 2] <- corr1[[2]][2, 1] <- .5

  expect_equal(
    graph_test_closure(
      par_gate,
      pvals,
      .05,
      list(1:2, 3:4),
      c("b", "p"),
      corr1
    )$outputs,
    list(
      adjusted_p = c(.048, .02, .048, .048),
      rejected = rep(TRUE, 4),
      graph = graph_update(par_gate, rep(TRUE, 4))$updated_graph
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    graph_test_closure(
      par_gate,
      pvals,
      .05,
      list(1, 2, 3, 4),
      rep("p", 4),
      rep(list(matrix(1)), 4)
    )$outputs,
    graph_test_closure(par_gate, pvals, .05)$outputs
  )
})

test_that("compare adjusted p-values to gMCP - Bonferroni & parametric", {
  g <- random_graph(6)
  p <- pnorm(rnorm(6, 2.5), lower.tail = FALSE)

  if (requireNamespace("gMCP", quietly = TRUE)) {
    gmcp_g <- as_graphMCP(g)

    expect_equal(
      graph_test_shortcut(g, p)$outputs$adjusted_p,
      gMCP::gMCP(gmcp_g, p, "Bonferroni")@adjPValues
    )

    expect_equal(
      graph_test_closure(g, p)$outputs$adjusted_p,
      gMCP::gMCP(gmcp_g, p, "Bonferroni")@adjPValues
    )

    expect_equal(
      graph_test_closure(
        g,
        p,
        test_types = "p",
        test_corr = list(diag(6))
      )$outputs$adjusted_p,
      gMCP::gMCP(gmcp_g, p, "parametric", correlation = diag(6))@adjPValues
    )
  }
})

test_that("compare adjusted p-values to lrstat - Bonferroni & Simes", {
  g <- random_graph(6)

  # lrstat has no allowance for floating point differences - hypothesis weights
  # must sum to exactly 1. So this corrects any floating point differences (I
  # think).
  g$hypotheses[[6]] <- 1 - sum(g$hypotheses[1:5])

  p <- pnorm(rnorm(6, 2.5), lower.tail = FALSE)

  if (requireNamespace("lrstat", quietly = TRUE)) {
    fwgtmat_will_run <- tryCatch(
      {
        lrstat::fwgtmat(g$hypotheses, g$transitions)
        TRUE
      },
      error = function(cond) FALSE
    )

    if (fwgtmat_will_run) {
      gw <- lrstat::fwgtmat(g$hypotheses, g$transitions)

      fam1 <- matrix(1, ncol = 6)
      fam2 <- rbind(c(1, 1, 1, 0, 0, 0), c(0, 0, 0, 1, 1, 1))

      expect_equal(
        graph_test_shortcut(g, p)$outputs$adjusted_p,
        lrstat::fadjpbon(g$hypotheses, g$transitions, matrix(p, ncol = 6)),
        ignore_attr = TRUE
      )

      expect_equal(
        graph_test_closure(g, p)$outputs$adjusted_p,
        lrstat::fadjpbon(g$hypotheses, g$transitions, matrix(p, ncol = 6)),
        ignore_attr = TRUE
      )

      expect_equal(
        graph_test_closure(g, p, test_types = "s")$outputs$adjusted_p,
        lrstat::fadjpsim(gw, p, fam1),
        ignore_attr = TRUE
      )

      expect_equal(
        graph_test_closure(
          g,
          p,
          test_groups = list(1:3, 4:6),
          test_types = "s"
        )$outputs$adjusted_p,
        lrstat::fadjpsim(gw, p, fam2),
        ignore_attr = TRUE
      )
    }
  }
})

test_that("closure testing rejects none when adjusted p-values exceed 1", {
  expect_equal(
    graph_test_closure(random_graph(6), rep(1, 6), 1)$outputs$rejected,
    rep(FALSE, 6),
    ignore_attr = TRUE
  )
})

test_that("closure internal consistency", {
  rando <- random_graph(6)
  p <- pnorm(rnorm(6, 2), lower.tail = FALSE)

  closure_results <- graph_test_closure(
    rando,
    p,
    .025,
    list(1:2, 3:4, 5:6),
    c("b", "p", "s"),
    list(NA, diag(2), NA),
    TRUE,
    TRUE
  )

  num_hyps <- length(rando$hypotheses)

  expect_equal(
    closure_results$inputs$p,
    closure_results$test_values$results$p[seq_len(num_hyps)],
    ignore_attr = TRUE
  )

  expect_equal(
    closure_results$inputs$alpha,
    closure_results$test_values$results$Alpha[[1]],
    ignore_attr = TRUE
  )

  if (requireNamespace("dplyr", quietly = TRUE)) {
    df_test_values_inter_reject <- dplyr::mutate(
      dplyr::group_by(
        tibble::as_tibble(closure_results$test_values$results),
        Intersection
      ),
      Hypothesis = Hypothesis,
      Inequality_holds = max(Inequality_holds),
      .keep = "used"
    )

    df_test_values_hyp_reject <- dplyr::summarise(
      dplyr::group_by(
        df_test_values_inter_reject,
        Hypothesis
      ),
      Inequality_holds = min(Inequality_holds)
    )

    test_values_hypothesis_reject <- !!setNames(
      df_test_values_hyp_reject$Inequality_holds,
      df_test_values_hyp_reject$Hypothesis
    )

    expect_equal(
      closure_results$outputs$rejected,
      test_values_hypothesis_reject
    )
  }
})

test_that("parametric floating point errors", {
  # Parametric adjusted p-values and c-values are now rounded to 10 decimals to
  # avoid floating point errors. These tests fail without this rounding.
  bh <- bonferroni_holm(rep(1 / 3, 3))
  p <- rep(.025, 3)

  t_corr <- list(matrix(1, 3, 3))

  res_para <- graph_test_closure(
    bh,
    p,
    .025,
    test_types = "p",
    test_corr = t_corr,
    verbose = TRUE,
    test_values = TRUE
  )

  expect_equal(
    res_para$outputs$rejected,
    c(H1 = TRUE, H2 = TRUE, H3 = TRUE)
  )

  expect_equal(res_para$details$results$reject_intersection, rep(TRUE, 7))

  expect_equal(res_para$test_values$results$Inequality_holds, rep(TRUE, 12))
})

test_that("adjusted p that exceeds alpha by floating point diff is rejected", {
  g <- graph_create(
    c(.5, .5, 0, 0, 0, 0),
    rbind(
      c(0.0000, 0.5000, 0.2500, 0.0000, 0.2500, 0.0000),
      c(0.5000, 0.0000, 0.0000, 0.2500, 0.0000, 0.2500),
      c(0.0000, 0.0000, 0.0000, 0.0000, 1.0000, 0.0000),
      c(0.0001, 0.0000, 0.0000, 0.0000, 0.0000, 0.9999),
      c(0.0000, 0.0001, 0.9999, 0.0000, 0.0000, 0.0000),
      c(0.0000, 0.0000, 0.0000, 1.0000, 0.0000, 0.0000)
    )
  )

  p <- c(0.001, 0.01875, 0.013, .0002, 0.03, 0.04)

  expect_equal(
    graph_test_closure(g, p)$outputs$rejected,
    c(H1 = TRUE, H2 = TRUE, H3 = FALSE, H4 = TRUE, H5 = FALSE, H6 = FALSE)
  )
})
