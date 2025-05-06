test_that("bonferroni", {
  expect_equal(
    bonferroni(3),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0, 0, 0, 0, 0, 0, 0, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("bonferroni_weighted", {
  expect_equal(
    bonferroni_weighted(rep(1 / 3, 3)),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0, 0, 0, 0, 0, 0, 0, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("bonferroni_holm", {
  expect_equal(
    bonferroni_holm(3),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("bonferroni_holm_weighted", {
  expect_equal(
    bonferroni_holm_weighted(rep(1 / 3, 3)),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("dunnett_single_step", {
  expect_equal(
    dunnett_single_step(3),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0, 0, 0, 0, 0, 0, 0, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("dunnett_single_step_weighted", {
  expect_equal(
    dunnett_single_step_weighted(rep(1 / 3, 3)),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0, 0, 0, 0, 0, 0, 0, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("dunnett_closure_weighted", {
  expect_equal(
    dunnett_closure_weighted(rep(1 / 3, 3)),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("fallback", {
  expect_equal(
    fallback(rep(1 / 3, 3)),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0, 0, 1, 0, 0, 0, 1, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("fallback_improved_1", {
  expect_equal(
    fallback_improved_1(rep(1 / 3, 3)),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0, 0.5, 1, 0, 0.5, 0, 1, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("fallback_improved_2", {
  expect_equal(
    fallback_improved_2(rep(1 / 3, 3)),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0.9999, 1, 1, 0, 0, 0, 0.0001, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("fixed_sequence", {
  expect_equal(
    fixed_sequence(3),
    structure(
      list(
        hypotheses = c(
          H1 = 1,
          H2 = 0,
          H3 = 0
        ),
        transitions = structure(
          c(0, 0, 0, 1, 0, 0, 0, 1, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("hochberg", {
  expect_equal(
    hochberg(3),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("hommel", {
  expect_equal(
    hommel(3),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})

test_that("sidak", {
  expect_equal(
    sidak(3),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0, 0, 0, 0, 0, 0, 0, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph",
      title = "Initial graph"
    )
  )
})
