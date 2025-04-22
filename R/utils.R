parse_parametric_corr <- function(test_groups, test_corr, num_hyps) {
  new_corr <- matrix(NA, num_hyps, num_hyps)

  for (group_num in seq_along(test_groups)) {
    test_group <- test_groups[[group_num]]

    new_corr[test_group, test_group] <- test_corr[[group_num]]
  }

  diag(new_corr) <- 1

  new_corr
}

