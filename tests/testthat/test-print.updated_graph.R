hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
names <- c("H1", "H2", "H3", "H4")
g <- graph_create(hypotheses, transitions, names)

test_that("snapshot print method", {
  expect_snapshot(graph_update(g, integer(0)))

  expect_snapshot(graph_update(g, c(FALSE, FALSE, FALSE, TRUE)))
  expect_snapshot(graph_update(g, c(1, 2, 4)))
})

test_that("print works with ordered deletion (intermediate graphs)", {
  updated <- graph_update(g, c(2, 3))

  expect_output(print(updated), "Deletion sequence")
  expect_output(print(updated), "Step 1")
  expect_output(print(updated), "Final updated graph")
})

test_that("print works with single ordered deletion", {
  updated <- graph_update(g, 4)

  expect_output(print(updated), "Deletion sequence")
})

test_that("print works with three ordered deletions", {
  updated <- graph_update(g, c(1, 2, 4))

  expect_output(print(updated), "Step 1")
  expect_output(print(updated), "Step 2")
})
