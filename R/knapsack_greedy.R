#' Solve the knapsack problem using a greedy heuristic
#'
#' Selects items in descending order of value per unit of weight
#' until no more items fit within the capacity.
#'
#' @param x A data frame containing positive numeric columns
#'   w (weight) and v (value).
#' @param W A positive number representing the maximum weight.
#'
#' @return A list containing the total value and selected row indices.
#' @export

knapsack_greedy <- function(x, W) {
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
  ratio <- x$v / x$w  #value/weight ratio
  order_items <- order(ratio, decreasing = TRUE)
  elements <- integer(0)
  total_weight <- 0
  total_value <- 0

  for (i in order_items) {
    if (total_weight + x$w[i] <= W) {
      elements <- c(elements, i)
      total_weight <- total_weight + x$w[i]
      total_value <- total_value + x$v[i]
    }
  }

  list(
    value = total_value,
    elements = elements
  )
}
