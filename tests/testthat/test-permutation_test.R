test_that("stat_ks is in [0, 1] and large when distributions differ", {
  x <- c(1, 2, 3, 10, 11, 12)
  Y <- c(0, 0, 0, 1, 1, 1)
  k <- stat_ks(x, Y)
  expect_gte(k, 0)
  expect_lte(k, 1)
  expect_equal(k, 1)
})

test_that("stat_cvm is non-negative", {
  x <- c(1, 2, 3, 10, 11, 12)
  Y <- c(0, 0, 0, 1, 1, 1)
  expect_gte(stat_cvm(x, Y), 0)
})

test_that("permutation_pvalue_one returns value in (0, 1]", {
  set.seed(1)
  x <- rnorm(50, mean = rep(c(0, 1), each = 25))
  Y <- rep(0:1, each = 25)
  p <- permutation_pvalue_one(x, Y, stat_ks, n_shuffles = 99)
  expect_gt(p, 0)
  expect_lte(p, 1)
})

test_that("permutation_pvalues returns length p", {
  set.seed(1)
  sim <- simulate_mixture_data(n = 40, p = 8, n_relevant = 2, alpha = 0.9, seed = 1)
  pvals <- permutation_pvalues(sim$X, sim$Y, stat_ks, n_shuffles = 49)
  expect_length(pvals, 8)
  expect_true(all(pvals > 0 & pvals <= 1))
})

test_that("select_features_permutation returns subset of 1:p", {
  set.seed(1)
  sim <- simulate_mixture_data(n = 60, p = 10, n_relevant = 3, alpha = 0.85, seed = 2)
  sel <- select_features_permutation(sim$X, sim$Y, stat_ks, n_shuffles = 99, alpha_level = 0.10)
  expect_true(all(sel %in% seq_len(10)))
})

