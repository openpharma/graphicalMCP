test_that("improper inputs throw errors", {
  rando <- random_graph(3)

  expect_no_error(graph_calculate_power(rando))

  expect_error(graph_calculate_power(rando, sim_n = 100.5))

  expect_error(graph_calculate_power(rando, power_marginal = c("1", 1, 1)))
  expect_error(graph_calculate_power(rando, sim_corr = matrix("1", 3, 3)))

  expect_error(graph_calculate_power(rando, power_marginal = c(1, 1)))
  expect_error(graph_calculate_power(rando, sim_corr = matrix(1, 2, 2)))

  expect_error(graph_calculate_power(rando, sim_corr = matrix(NA, 3, 3)))
  corr_inval <- matrix(c(1, 0, .5, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
  expect_error(graph_calculate_power(rando, sim_corr = corr_inval))

  expect_error(graph_calculate_power(rando, sim_success = "non-function"))

  expect_error(
    graph_calculate_power(
      rando,
      test_groups = list(f1 = 1:2, f2 = 3),
      test_types = c(f2 = "b", f1 = "s"),
      test_corr = list(f3 = NA, f1 = NA)
    )
  )

  expect_no_error(
    graph_calculate_power(
      rando,
      test_groups = list(f1 = 1:2, f2 = 3),
      test_types = c(f2 = "b", f1 = "s"),
      test_corr = list(f2 = NA, f1 = NA)
    )
  )
})

test_that("power results are identical under a given seed", {
  rando <- random_graph(2)

  set.seed(42823)
  bonf_1 <- graph_calculate_power(rando, sim_n = 1e5)

  set.seed(42823)
  bonf_2 <- graph_calculate_power(rando, sim_n = 1e5)

  expect_equal(bonf_1, bonf_2)

  set.seed(42824)
  simes_1 <- graph_calculate_power(rando, test_types = "s", sim_n = 1e5)

  set.seed(42824)
  simes_2 <- graph_calculate_power(rando, test_types = "s", sim_n = 1e5)

  expect_equal(simes_1, simes_2)

  set.seed(42825)
  para_1 <- graph_calculate_power(
    rando,
    test_types = "p",
    test_corr = list(diag(2)),
    sim_n = 1e4
  )

  set.seed(42825)
  para_2 <- graph_calculate_power(
    rando,
    test_types = "p",
    test_corr = list(diag(2)),
    sim_n = 1e4
  )

  expect_equal(para_1, para_2)
})

test_that("size one groups are turned into Bonferroni", {
  g <- fallback(rep(1 / 3, 3))

  set.seed(42823)
  expect_equal(
    graph_calculate_power(
      graph = g,
      alpha = 0.05,
      test_groups = list(1, 2, 3),
      test_types = c("s", "p", "p"),
      sim_n = 1e5
    )$inputs$test_types,
    c("bonferroni", "bonferroni", "bonferroni"),
    ignore_attr = TRUE
  )
})

test_that("multi-group/multi-test type runs without error", {
  expect_no_error(
    graph_calculate_power(
      graph = random_graph(4),
      test_groups = list(c(4, 1), 2:3),
      test_types = "s"
    )
  )

  expect_no_error(
    graph_calculate_power(
      graph = random_graph(4),
      test_groups = list(c(3, 1), c(2, 4)),
      test_types = "p",
      test_corr = list(diag(2), diag(2))
    )
  )
})

test_that("medium graph runs without error", {
  # random positive definite matrix - not sure if the diag override can break
  # this, but it's at least better than my last try
  t_corr <- matrix(abs(stats::rWishart(1, 9, diag(9))), 9, 9)
  t_corr <- t_corr / max(t_corr)
  diag(t_corr) <- 1
  t_corr_para <- t_corr[c(1, 4, 7), c(1, 4, 7)]

  expect_no_error(
    graph_calculate_power(
      graph = bonferroni(9),
      alpha = 0.025,
      test_groups = list(c(1, 4, 7), 2:3, 5:6, 8:9),
      test_types = c("p", "s", "s", "s"),
      test_corr = list(t_corr_para, NA, NA, NA),
      sim_n = 1e4,
      power_marginal = runif(9, min = 0, max = 1),
      sim_corr = diag(9),
      sim_success = function(.) .[1] || .[4] || .[7]
    )
  )
})

test_that("verbose output", {
  t_corr <- matrix(abs(stats::rWishart(1, 9, diag(9))), 9, 9)
  t_corr <- t_corr / max(t_corr)
  diag(t_corr) <- 1
  t_corr_para <- t_corr[c(1, 4, 7), c(1, 4, 7)]

  expect_equal(
    names(
      graph_calculate_power(
        graph = fallback_improved_1(rep(1 / 3, 3)),
        alpha = 0.025,
        sim_n = 1e4,
        power_marginal = runif(3, min = 0, max = 1),
        verbose = TRUE
      )$details
    ),
    c("p_sim", "test_results")
  )
})
