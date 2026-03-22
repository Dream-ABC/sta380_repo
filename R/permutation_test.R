#' Test statistic: Kolmogorov–Smirnov statistic.
#'
#' @description Maximum absolute difference between the two empirical CDFs
#'   of x in group 0 and group 1.
#'
#' @param x Numeric vector of feature values.
#' @param Y Integer or numeric vector of class labels (0 and 1).
#' @return Scalar statistic (>= 0).
#'
#' @examples
#' x <- c(1, 2, 3, 10, 11, 12)
#' Y <- c(0, 0, 0, 1, 1, 1)
#' stat_ks(x, Y)
#'
#' @importFrom stats ecdf
#' @export
stat_ks <- function(x, Y) {
  x0 <- x[Y == 0]
  x1 <- x[Y == 1]
  if (length(x0) == 0 || length(x1) == 0) return(0)
  F0 <- ecdf(x0)
  F1 <- ecdf(x1)
  xs <- sort(unique(c(x0, x1)))
  if (length(xs) == 0) return(0)
  max(abs(F1(xs) - F0(xs)))
}


#' Test statistic: Cramér–von Mises statistic.
#'
#' @description Two-sample CvM: n0*n1/(n0+n1)^2 * sum over all (x0,x1) of
#'   (F0(x1) - F1(x0))^2, using empirical CDFs F0, F1. Alternative common
#'   form: average squared difference of ECDFs at pooled sample points.
#'
#' @param x Numeric vector of feature values.
#' @param Y Integer or numeric vector of class labels (0 and 1).
#' @return Scalar statistic (>= 0).
#'
#' @examples
#' x <- c(1, 2, 3, 10, 11, 12)
#' Y <- c(0, 0, 0, 1, 1, 1)
#' stat_cvm(x, Y)
#'
#' @importFrom stats ecdf
#' @export
stat_cvm <- function(x, Y) {
  x0 <- x[Y == 0]
  x1 <- x[Y == 1]
  n0 <- length(x0)
  n1 <- length(x1)
  if (n0 == 0 || n1 == 0) return(0)
  F0 <- ecdf(x0)
  F1 <- ecdf(x1)
  n <- n0 + n1
  # CvM = (n0*n1/n^2) * sum_i (F0(z_i) - F1(z_i))^2 over pooled z
  z <- sort(unique(c(x0, x1)))
  if (length(z) == 0) return(0)
  d <- F0(z) - F1(z)
  (n0 * n1 / n^2) * sum(d^2)
}


#' Permutation p-value for a single feature.
#'
#' @description Computes test statistic T on (x, Y), then repeatedly shuffles
#'   Y and recomputes T. p-value = (# shuffles with T >= T_observed) / n_shuffles
#'   (one-sided). For stability, 1 is added to numerator and denominator.
#'
#' @param x Numeric vector of feature values.
#' @param Y Integer or numeric vector of class labels (0 and 1).
#' @param stat_fun Function of (x, Y) returning a scalar statistic (e.g. stat_ks or stat_cvm).
#' @param n_shuffles Number of permutation samples.
#' @return Scalar p-value in (0, 1].
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(50, mean = rep(c(0, 1), each = 25))
#' Y <- rep(0:1, each = 25)
#' permutation_pvalue_one(x, Y, stat_ks, n_shuffles = 999)
#'
#' @importFrom stats runif
#' @export
permutation_pvalue_one <- function(x, Y, stat_fun, n_shuffles = 999) {
  T_obs <- stat_fun(x, Y)
  T_perm <- numeric(n_shuffles)
  n <- length(Y)
  for (b in seq_len(n_shuffles)) {
    Y_shuf <- sample(Y)
    T_perm[b] <- stat_fun(x, Y_shuf)
  }
  # p = (# T_perm >= T_obs + 1) / (n_shuffles + 1) to avoid p=0
  (sum(T_perm >= T_obs) + 1) / (n_shuffles + 1)
}


#' Permutation p-values for all features.
#'
#' @description For each column of X, computes permutation p-value using the
#'   chosen test statistic. Returns a vector of p-values (one per feature).
#'
#' @param X n x p numeric matrix of data.
#' @param Y Length-n vector of class labels (0 and 1).
#' @param stat_fun Function of (x, Y) returning a scalar (e.g. stat_ks, stat_ks, stat_cvm).
#' @param n_shuffles Number of permutation samples per feature.
#' @return Numeric vector of length p (p-values).
#'
#' @examples
#' set.seed(1)
#' sim <- simulate_mixture_data(n = 60, p = 10, n_relevant = 3, alpha = 0.9, seed = 1)
#' pvals <- permutation_pvalues(sim$X, sim$Y, stat_ks, n_shuffles = 199)
#' which(pvals < 0.05)
#'
#' @export
permutation_pvalues <- function(X, Y, stat_fun, n_shuffles = 999) {
  p <- ncol(X)
  pvals <- numeric(p)
  for (j in seq_len(p)) {
    pvals[j] <- permutation_pvalue_one(X[, j], Y, stat_fun, n_shuffles)
  }
  pvals
}


#' Select features by permutation test.
#'
#' @description Computes permutation p-values for all features and returns
#'   indices of features with p-value <= alpha_level.
#'
#' @param X n x p numeric matrix.
#' @param Y Length-n class labels (0 and 1).
#' @param stat_fun Test statistic function (x, Y) -> scalar.
#' @param n_shuffles Number of permutations per feature.
#' @param alpha_level Significance level (e.g. 0.05).
#' @return Integer vector of selected feature indices (1-based).
#'
#' @examples
#' set.seed(1)
#' sim <- simulate_mixture_data(n = 80, p = 20, n_relevant = 5, alpha = 0.85, seed = 2)
#' selected <- select_features_permutation(sim$X, sim$Y, stat_ks, n_shuffles = 499, alpha_level = 0.05)
#'
#' @export
select_features_permutation <- function(X, Y, stat_fun, n_shuffles = 999, alpha_level = 0.05) {
  pvals <- permutation_pvalues(X, Y, stat_fun, n_shuffles)
  which(pvals <= alpha_level)
}
