test_that("simulate_mixture_data returns correct structure", {
  sim <- simulate_mixture_data(n = 50, p = 10, n_relevant = 3, alpha = 0.8, seed = 1)
  expect_equal(nrow(sim$X), 50)
  expect_equal(ncol(sim$X), 10)
  expect_length(sim$Y, 50)
  expect_true(all(sim$Y %in% c(0, 1)))
  expect_length(sim$S, 3)
  expect_true(all(sim$S %in% seq_len(10)))
  expect_equal(sim$alpha, 0.8)
})

test_that("simulate_mixture_data respects n_relevant and seed", {
  sim1 <- simulate_mixture_data(n = 30, p = 5, n_relevant = 2, seed = 42)
  sim2 <- simulate_mixture_data(n = 30, p = 5, n_relevant = 2, seed = 42)
  expect_equal(sim1$S, sim2$S)
  expect_equal(sim1$X, sim2$X)
  expect_equal(sim1$Y, sim2$Y)
})

test_that("simulate_mixture_data works with dist_G and dist_H options", {
  sim_n <- simulate_mixture_data(n = 20, p = 5, n_relevant = 2, dist_G = "normal", dist_H = "normal", seed = 1)
  sim_u <- simulate_mixture_data(n = 20, p = 5, n_relevant = 2, dist_H = "uniform", seed = 1)
  expect_equal(dim(sim_n$X), c(20, 5))
  expect_equal(dim(sim_u$X), c(20, 5))
})
