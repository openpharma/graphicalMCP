# test_values_bonferroni -------------------------------------------------------
test_that("test_values_bonferroni returns correct structure", {
  p <- c(H1 = 0.01, H2 = 0.03)
  w <- c(H1 = 0.5, H2 = 0.5)
  result <- graphicalMCP:::test_values_bonferroni(p, w, alpha = 0.025)

  expect_s3_class(result, "data.frame")
  expect_true(all(c(
    "Intersection", "Hypothesis", "Test", "p",
    "c_value", "Weight", "Alpha", "Inequality_holds"
  ) %in%
    names(result)))
  expect_equal(nrow(result), 2)
  expect_equal(result$Test, c("bonferroni", "bonferroni"))
})

test_that("test_values_bonferroni inequality logic is correct", {
  p <- c(H1 = 0.01, H2 = 0.03)
  w <- c(H1 = 0.5, H2 = 0.5)
  result <- graphicalMCP:::test_values_bonferroni(p, w, alpha = 0.025)

  # H1: 0.01 <= 0.5 * 0.025 = 0.0125 -> TRUE
  # H2: 0.03 <= 0.5 * 0.025 = 0.0125 -> FALSE
  expect_equal(result$Inequality_holds, c(TRUE, FALSE))
})

test_that("test_values_bonferroni returns NULL for empty input", {
  result <- graphicalMCP:::test_values_bonferroni(
    numeric(0), numeric(0),
    alpha = 0.025
  )
  expect_null(result)
})

test_that("test_values_bonferroni handles p=0 and w=0 as NA", {
  p <- c(H1 = 0, H2 = 0.01)
  w <- c(H1 = 0, H2 = 0.5)
  result <- graphicalMCP:::test_values_bonferroni(p, w, alpha = 0.025)

  expect_true(is.na(result$Inequality_holds[1]))
  expect_false(is.na(result$Inequality_holds[2]))
})

# test_values_simes -----------------------------------------------------------
test_that("test_values_simes returns correct structure", {
  p <- c(H1 = 0.01, H2 = 0.03)
  w <- c(H1 = 0.5, H2 = 0.5)
  result <- graphicalMCP:::test_values_simes(p, w, alpha = 0.025)

  expect_s3_class(result, "data.frame")
  expect_equal(result$Test, c("simes", "simes"))
  expect_equal(nrow(result), 2)
})

test_that("test_values_simes cumulative weight logic is correct", {
  p <- c(H1 = 0.01, H2 = 0.03)
  w <- c(H1 = 0.5, H2 = 0.5)
  result <- graphicalMCP:::test_values_simes(p, w, alpha = 0.025)

  # For H1 (p=0.01): w_sum = sum of weights where p <= 0.01 = 0.5
  # For H2 (p=0.03): w_sum = sum of weights where p <= 0.03 = 1.0
  expect_equal(result$Weight, c(0.5, 1.0))
})

test_that("test_values_simes returns NULL for empty input", {
  result <- graphicalMCP:::test_values_simes(
    numeric(0), numeric(0),
    alpha = 0.025
  )
  expect_null(result)
})

test_that("test_values_simes handles p=0 and w_sum=0 as NA", {
  p <- c(H1 = 0, H2 = 0.01)
  w <- c(H1 = 0, H2 = 0.5)
  result <- graphicalMCP:::test_values_simes(p, w, alpha = 0.025)

  # H1: p=0, w_sum = sum of weights where p <= 0 = 0 -> NA
  expect_true(is.na(result$Inequality_holds[1]))
})

# test_values_hochberg --------------------------------------------------------
test_that("test_values_hochberg returns correct structure", {
  p <- c(H1 = 0.01, H2 = 0.03)
  w <- c(H1 = 0.5, H2 = 0.5)
  result <- graphicalMCP:::test_values_hochberg(p, w, alpha = 0.025)

  expect_s3_class(result, "data.frame")
  expect_equal(result$Test, c("hochberg", "hochberg"))
  expect_equal(nrow(result), 2)
})

test_that("test_values_hochberg weight quotient logic is correct", {
  p <- c(H1 = 0.01, H2 = 0.03)
  w <- c(H1 = 0.5, H2 = 0.5)
  result <- graphicalMCP:::test_values_hochberg(p, w, alpha = 0.025)

  total_weight <- sum(w)
  # H1 (p=0.01): num with p <= 0.01 = 1, w_quo = 1 / (2 - 1 + 1) = 0.5
  # H2 (p=0.03): num with p <= 0.03 = 2, w_quo = 1 / (2 - 2 + 1) = 1.0
  expect_equal(result$Weight, c(total_weight / 2, total_weight / 1))
})

test_that("test_values_hochberg returns NULL for empty input", {
  result <- graphicalMCP:::test_values_hochberg(
    numeric(0), numeric(0),
    alpha = 0.025
  )
  expect_null(result)
})

# test_values_parametric ------------------------------------------------------
test_that("test_values_parametric returns correct structure", {
  p <- c(H1 = 0.01, H2 = 0.03)
  w <- c(H1 = 0.5, H2 = 0.5)
  corr <- diag(2)
  result <- graphicalMCP:::test_values_parametric(
    p, w,
    alpha = 0.025, test_corr = corr
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$Test, c("parametric", "parametric"))
  expect_true("c_value" %in% names(result))
})

test_that("test_values_parametric returns NULL for empty input", {
  result <- graphicalMCP:::test_values_parametric(
    numeric(0), numeric(0),
    alpha = 0.025, test_corr = matrix(0, 0, 0)
  )
  expect_null(result)
})
