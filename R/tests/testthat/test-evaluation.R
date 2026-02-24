test_that("evaluate_precision_recall perfect selection", {
  er <- evaluate_precision_recall(S_truth = c(1, 3, 5), S_hat = c(1, 3, 5))
  expect_equal(er$precision, 1)
  expect_equal(er$recall, 1)
})

test_that("evaluate_precision_recall empty S_hat", {
  er <- evaluate_precision_recall(S_truth = c(1, 2), S_hat = integer(0))
  expect_equal(er$precision, 0)
  expect_equal(er$recall, 0)
})

test_that("evaluate_precision_recall empty S_truth", {
  er <- evaluate_precision_recall(S_truth = integer(0), S_hat = c(1, 2))
  expect_equal(er$precision, 0)
  expect_equal(er$recall, 0)
})

test_that("evaluate_precision_recall partial overlap", {
  er <- evaluate_precision_recall(S_truth = c(1, 3, 5), S_hat = c(1, 2, 3, 5))
  # correct: 1, 3, 5 -> 3; |S_hat| = 4 -> precision 3/4; |S| = 3 -> recall 1
  expect_equal(er$precision, 3/4)
  expect_equal(er$recall, 1)
})

test_that("f1_score is 0 when both 0", {
  expect_equal(f1_score(0, 0), 0)
})

test_that("f1_score harmonic mean when precision equals recall", {
  expect_equal(f1_score(0.5, 0.5), 0.5)
})

test_that("f1_score formula", {
  expect_equal(f1_score(0.8, 0.6), 2 * 0.8 * 0.6 / (0.8 + 0.6))
})
