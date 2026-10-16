#' Solve the knapsack problem using parallel brute force
#'
#' Checks all possible combinations of items and returns the
#' combination with the highest value within the weight limit.
#' If parallel is TRUE, the search is divided among multiple
#' CPU cores.
#'
#' @param x A data frame containing positive numeric columns w and v.
#' @param W A positive number representing the maximum weight.
#' @param parallel Logical. If TRUE, parallelizes the search
#'   across detected CPU cores. Defaults to FALSE.
#'
#' @return A list containing the maximum value and selected row indices.
#' @export
brute_force_knapsack_parallel <- function(x, W, parallel = FALSE) {
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

  if (!is.logical(parallel) || length(parallel) != 1L ||
      is.na(parallel)) {
    stop("parallel must be TRUE or FALSE")
  }

  n <- nrow(x)

  if (n > 25L) {
    stop("Brute force is too slow for more than 25 items.")
  }

  # Search a given set of masks and return its best solution
  search_chunk <- function(masks, items, W, n) {
    best_value <- 0
    best_elements <- integer(0)

    for (mask in masks) {
      bits <- as.integer(intToBits(mask))[seq_len(n)]
      elements <- which(bits == 1L)

      total_weight <- sum(items$w[elements])

      if (total_weight <= W) {
        total_value <- sum(items$v[elements])

        if (total_value > best_value) {
          best_value <- total_value
          best_elements <- elements
        }
      }
    }

    list(value = best_value, elements = best_elements)
  }

  masks <- 0:(2^n - 1)

  if (!parallel) {
    return(search_chunk(masks, items = x, W = W, n = n))
  }

  cores <- parallel::detectCores()

  # R CMD check allows at most 2 cores
  if (identical(Sys.getenv("_R_CHECK_LIMIT_CORES_"), "TRUE")) {
    cores <- 2L
  }

  # Fall back to the sequential search if parallelism is not possible
  if (is.na(cores) || cores < 2L) {
    return(search_chunk(masks, items = x, W = W, n = n))
  }

  cores <- min(cores, length(masks))

  # Divide the masks into roughly equal-sized chunks
  chunks <- split(
    masks,
    cut(seq_along(masks), breaks = cores, labels = FALSE)
  )

  # Create workers and stop the cluster when the function exits
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl))

  results <- parallel::parLapply(
    cl, chunks, search_chunk, items = x, W = W, n = n
  )

  # Select the best result returned by any worker
  values <- vapply(results, function(res) res$value, numeric(1))
  best <- which.max(values)

  results[[best]]
}
