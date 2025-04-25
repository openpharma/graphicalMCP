hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
names <- c("H1", "H2", "H3", "H4")
g <- graph_create(hypotheses, transitions, names)
gw4 <- graph_generate_weights(g)
gw4_gmcp <- gMCP::generateWeights(g$transitions, g$hypotheses)

bh10 <- bonferroni_holm(10)
gw10 <- graph_generate_weights(bh10)
gw10_gmcp <- gMCP::generateWeights(bh10$transitions, bh10$hypotheses)

# 2.1 from the gMCP vignette
m <- rbind(
  H11 = c(0, 0.5, 0, 0.5, 0, 0),
  H21 = c(1 / 3, 0, 1 / 3, 0, 1 / 3, 0),
  H31 = c(0, 0.5, 0, 0, 0, 0.5),
  H12 = c(0, 1, 0, 0, 0, 0),
  H22 = c(0.5, 0, 0.5, 0, 0, 0),
  H32 = c(0, 1, 0, 0, 0, 0)
)
w <- c(1 / 3, 1 / 3, 1 / 3, 0, 0, 0)
gmcp_graph <- gMCP::matrix2graph(m)
gmcp_graph@weights <- structure(w, names = rownames(m))
gw_11_gmcp <- gMCP::generateWeights(gmcp_graph)

graph <- graph_create(w, m)
gw_11 <- graph_generate_weights(graph)

test_that("compare to gMCP", {
  # The `[nrow():1,]` piece reverses row order to match gmcp ordering
  expect_true(all.equal(unname(gw4[seq(nrow(gw4), 1), ]), unname(gw4_gmcp)))
  expect_true(all.equal(unname(gw10[seq(nrow(gw10), 1), ]), unname(gw10_gmcp)))
  expect_true(
    all.equal(unname(gw_11[seq(nrow(gw_11), 1), ]), unname(gw_11_gmcp))
  )
})
