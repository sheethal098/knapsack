#' Solve the knapsack problem using vectorized brute force
#'
#' Optimized version of \code{brute_force_knapsack()}. Instead of looping
#' over every combination, it builds the total weight and value of all
#' subsets at once by doubling the vectors once per item.
#'
#' @param x A data frame containing positive numeric columns w and v.
#' @param W A positive number representing the maximum weight.
#'
#' @return A list containing the maximum value and selected row indices.
#' @export
brute_force_knapsack_opt <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% names(x))) {
    stop("x must be a data frame with columns w and v")
  }
  if (!is.numeric(x$w) || !is.numeric(x$v) ||
      any(!is.finite(x$w)) || any(!is.finite(x$v)) ||
      any(x$w <= 0) || any(x$v <= 0)) {
    stop("w and v must be positive and finite")
  }
  if (!is.numeric(W) || length(W) != 1L ||
      !is.finite(W) || W <= 0) {
    stop("W must be positive and finite")
  }
  n <- nrow(x)
  if (n > 22L) {
    stop("This version stores all 2^n subsets in memory; use at most 22 items.")
  }

  w <- x$w
  v <- x$v
  total_w <- 0
  total_v <- 0
  for (i in seq_len(n)) {
    total_w <- c(total_w, total_w + w[i])
    total_v <- c(total_v, total_v + v[i])
  }
  # position k in these vectors corresponds to mask k - 1

  total_v[total_w > W] <- -Inf
  best <- which.max(total_v)

  bits <- as.integer(intToBits(best - 1))[seq_len(n)]
  list(value = total_v[best], elements = which(bits == 1L))
}

#' Solve the knapsack problem using vectorized dynamic programming
#'
#' Optimized version of \code{knapsack_dynamic()}. The inner loop over
#' capacities is replaced by one vector operation per item.
#'
#' @param x A data frame containing positive numeric columns
#'   w (weight) and v (value). Weights must be whole numbers.
#' @param W A positive integer representing the maximum weight.
#'
#' @return A list containing the maximum value and selected row indices.
#' @export
knapsack_dynamic_opt <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% names(x))) {
    stop("x must be a data frame containing columns 'w' and 'v'.")
  }
  if (!is.numeric(x$w) || !is.numeric(x$v) ||
      any(!is.finite(x$w)) || any(!is.finite(x$v)) ||
      any(x$w <= 0) || any(x$v <= 0)) {
    stop("Weights and values must be positive and finite.")
  }
  if (any(x$w != floor(x$w))) {
    stop("Weights must be whole numbers.")
  }
  if (!is.numeric(W) || length(W) != 1L ||
      !is.finite(W) || W <= 0 || W != floor(W)) {
    stop("W must be a positive integer.")
  }

  n <- nrow(x)
  w <- x$w
  v <- x$v
  dp <- matrix(0, nrow = n + 1L, ncol = W + 1L)

  for (i in seq_len(n)) {
    prev <- dp[i, ]
    new <- prev
    if (w[i] <= W) {
      idx <- (w[i] + 1L):(W + 1L)
      new[idx] <- pmax(prev[idx], v[i] + prev[idx - w[i]])
    }
    dp[i + 1L, ] <- new
  }

  best_value <- dp[n + 1L, W + 1L]
  elements <- integer(0)
  capacity <- W

  for (i in seq.int(n, 1L)) {
    if (dp[i + 1L, capacity + 1L] != dp[i, capacity + 1L]) {
      elements <- c(i, elements)
      capacity <- capacity - w[i]
    }
  }

  list(value = best_value, elements = elements)
}

#' Solve the knapsack problem using an optimized greedy heuristic
#'
#' Optimized version of \code{knapsack_greedy()}. It avoids growing a
#' vector inside the loop and stops as soon as no remaining item can fit.
#'
#' @param x A data frame containing positive numeric columns
#'   w (weight) and v (value).
#' @param W A positive number representing the maximum weight.
#'
#' @return A list containing the total value and selected row indices.
#' @export
knapsack_greedy_opt <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% names(x))) {
    stop("x must be a data frame containing columns 'w' and 'v'.")
  }
  if (!is.numeric(x$w) || !is.numeric(x$v) ||
      any(!is.finite(x$w)) || any(!is.finite(x$v)) ||
      any(x$w <= 0) || any(x$v <= 0)) {
    stop("Weights and values must be positive and finite.")
  }
  if (!is.numeric(W) || length(W) != 1L ||
      !is.finite(W) || W <= 0) {
    stop("W must be positive and finite.")
  }

  ord <- order(x$v / x$w, decreasing = TRUE)
  w <- x$w[ord]
  n <- length(w)
  min_left <- rev(cummin(rev(w)))   # smallest weight among the remaining items

  take <- logical(n)
  used <- 0
  for (i in seq_len(n)) {
    if (W - used < min_left[i]) break   # nothing left can fit
    if (used + w[i] <= W) {
      take[i] <- TRUE
      used <- used + w[i]
    }
  }

  list(value = sum(x$v[ord][take]), elements = ord[take])
}
