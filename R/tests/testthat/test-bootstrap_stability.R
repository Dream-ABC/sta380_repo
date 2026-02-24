test_that("bootstrap_selection_frequency returns length p and values in [0, 1]", {
  set.seed(1)
  sim <- simulate_mixture_data(n = 50, p = 10, n_relevant = 3, alpha = 0.8, seed = 5)
  select_wrapper <- function(X, Y) {
    select_features_permutation(X, Y, stat_mean_diff, n_shuffles = 99, alpha_level = 0.05)
  }
  freq <- bootstrap_selection_frequency(sim$X, sim$Y, select_wrapper, B = 20, seed = 10)
  expect_length(freq, 10)
  expect_true(all(freq >= 0 & freq <= 1))
})

test_that("select_features_bootstrap returns indices and has frequency attribute", {
  set.seed(1)
  sim <- simulate_mixture_data(n = 50, p = 10, n_relevant = 3, alpha = 0.8, seed = 5)
  select_wrapper <- function(X, Y) {
    select_features_permutation(X, Y, stat_mean_diff, n_shuffles = 99, alpha_level = 0.05)
  }
  sel <- select_features_bootstrap(sim$X, sim$Y, select_wrapper, B = 20, threshold = 0.3, seed = 10)
  expect_true(all(sel %in% seq_len(10)))
  expect_length(attr(sel, "frequency"), 10)
})
