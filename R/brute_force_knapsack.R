#' Solve the knapsack problem using brute force
#'
#' Checks all possible combinations of items and returns
#' the combination with the highest value within the weight limit.
#'
#' @param x A data frame containing positive numeric columns w and v.
#' @param W A positive number representing the maximum weight.
#'
#' @return A list containing the maximum value and selected row indices.
#' @export

brute_force_knapsack <- function(x, W) { #x is the dataframe ,w is the max weight
  if (!is.data.frame(x) || !all(c("w", "v") %in% names(x))) {
    stop("x must be a data frame must have column w and v")
  }
  if (!is.numeric(x$w) || !is.numeric(x$v) ||
      any(!is.finite(x$w)) || any(!is.finite(x$v)) ||
      any(x$w <= 0) || any(x$v <= 0)) {
    stop("W and V must be positive and finite")
  }
  if (!is.numeric(W) || length(W) != 1L ||
      !is.finite(W) || W <= 0) {
    stop("W must be positive and finite")
  }
  n <- nrow(x)
  if (n > 25L) {
    stop("Brute force is too slow for more than 25 items.")
  }

  best_value <- 0
  best_elements <- integer(0)
  for (mask in 0:(2^n - 1)) {  #caz comb starts from 0, every possible combination possible and converts the each no to bits
    bits <- as.integer(intToBits(mask))[seq_len(n)]  #keeps only first n bits
    elements <- which(bits == 1L)

    total_weight <- sum(x$w[elements])  #weight of selected items
    if (total_weight <= W) {
      total_value <- sum(x$v[elements])

      if (total_value > best_value) {  # compares the ttl_value w best_value if it fitswe save it move to nxt one
        best_value <- total_value
        best_elements <- elements
      }
    }
  }
  list(
    value = best_value,
    elements = best_elements
  )
}
