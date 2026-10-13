
#' Solve the knapsack problem using dynamic programming
#'
#' Finds the maximum total value without exceeding the
#' weight capacity.
#'
#' @param x A data frame containing positive numeric columns
#'   w (weight) and v (value).
#' @param W A positive integer representing the maximum weight.
#'
#' @return A list containing the maximum value and selected row indices.
#' @export

knapsack_dynamic <- function(x, W) {
  if (!is.data.frame(x) || !all(c("w", "v") %in% names(x))) {
    stop("x must be a data frame containing columns 'w' and 'v'.")
  }
  if (!is.numeric(x$w) || !is.numeric(x$v) ||
      any(!is.finite(x$w)) || any(!is.finite(x$v)) ||
      any(x$w <= 0) || any(x$v <= 0)) {
    stop("Weights and values must be positive and finite ")
  }
  if (!is.numeric(W) || length(W) != 1L ||
      !is.finite(W) || W <= 0 || W != floor(W)) {
    stop("W must be a positive integer.")
  }
  n <- nrow(x)
  dp <- matrix(0, nrow = n + 1L, ncol = W + 1L)

  for (i in seq_len(n)) {
    for (capacity in 0:W) {
      dp[i + 1L, capacity + 1L] <- dp[i, capacity + 1L]

      if (x$w[i] <= capacity) { # if the item fits then we include it
        candidate <- x$v[i] +
          dp[i, capacity - x$w[i] + 1L]

        if (candidate > dp[i + 1L, capacity + 1L]) {
          dp[i + 1L, capacity + 1L] <- candidate
        }
      }
    }
  }
  best_value <- dp[n + 1L, W + 1L]
  elements <- integer(0)
  capacity <- W

  for (i in seq.int(n, 1L)) {
    if (dp[i + 1L, capacity + 1L] !=
        dp[i, capacity + 1L]) {
      elements <- c(i, elements)
      capacity <- capacity - x$w[i]
    }
  }
  list(
    value = best_value,
    elements = elements
  )
}
