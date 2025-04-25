test_that("vectorized testing matches standard testing (single-group)", {
  m <- 6
  rando <- random_graph(m)
  hyp_names <- names(rando$hypotheses)

  p <- pnorm(rnorm(m, 2), lower.tail = FALSE)
  gw <- graph_generate_weights(rando)
  gw_h <- gw[, seq_len(m)]
  gw_weights <- gw[, seq_len(m) + m]

  groups1 <- list(seq_len(m))

  gw_compact_simes <- adjust_weights_simes(
    gw_weights,
    p,
    groups1
  )

  gw_compact_parametric <- adjust_weights_parametric_util(
    gw_weights,
    gw_h,
    diag(m),
    0.025,
    list(seq_len(m))
  )

  expect_equal(
    graphicalMCP:::graph_test_closure_fast(p, 0.025, gw_weights, gw_h),
    graph_test_closure(rando, p)$outputs$rejected,
    ignore_attr = TRUE
  )

  expect_equal(
    graphicalMCP:::graph_test_closure_fast(
      p,
      0.025,
      gw_compact_simes[, hyp_names],
      gw_h
    ),
    graph_test_closure(rando, p, test_types = "s")$outputs$rejected,
    ignore_attr = TRUE
  )

  expect_equal(
    graphicalMCP:::graph_test_closure_fast(
      p,
      0.025,
      gw_compact_parametric,
      gw_h
    ),
    graph_test_closure(
      rando,
      p,
      test_types = "p",
      test_corr = list(diag(m))
    )$outputs$rejected,
    ignore_attr = TRUE
  )
})

test_that("vectorized testing matches standard testing (multi-group)", {
  m <- 6
  rando <- random_graph(m)
  hyp_names <- names(rando$hypotheses)

  p <- pnorm(rnorm(m, 2), lower.tail = FALSE)
  gw <- graph_generate_weights(rando)
  gw_h <- gw[, seq_len(m)]
  gw_weights <- gw[, seq_len(m) + m]

  bonf_groups <- list(2:1)
  simes_groups <- list(3:4)
  simes_groups_reduce <- list(1:2)
  para_groups <- list(5:m)

  adjusted_weights_simes <- adjust_weights_simes(
    gw_weights[, unlist(simes_groups)],
    p[unlist(simes_groups)],
    simes_groups_reduce
  )

  adjusted_weights_parametric <- adjust_weights_parametric_util(
    gw_weights,
    gw_h,
    diag(m),
    0.05,
    para_groups
  )

  gw_weights <- gw_weights[, unlist(bonf_groups)]

  expect_equal(
    graphicalMCP:::graph_test_closure_fast(
      p,
      0.025,
      cbind(
        gw_weights,
        adjusted_weights_simes,
        adjusted_weights_parametric
      )[, hyp_names],
      gw_h
    ),
    graph_test_closure(
      rando,
      p,
      test_groups = list(1:2, 4:3, 5:m),
      test_types = c("b", "s", "p"),
      test_corr = list(NA, NA, diag(m - 4))
    )$outputs$rejected,
    ignore_attr = TRUE
  )
})
