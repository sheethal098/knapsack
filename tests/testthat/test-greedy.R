
test_that("greedy finds the expected result for this dataset", {
  items <- data.frame(
    w = c(2, 3, 4),
    v = c(10, 15, 18)
  )

  result <- knapsack_greedy(items, W = 5)

  expect_equal(result$value, 25)
  expect_equal(result$elements, c(1L, 2L))
})

test_that("greedy rejects invalid capacity", {
  items <- data.frame(
    w = c(2, 3),
    v = c(10, 15)
  )

  expect_error(knapsack_greedy(items, W = 0))
})


test_that("greedy can return less than the optimal value", {
  items <- data.frame(
    w = c(4, 3, 2),
    v = c(40, 30, 19)
  )

  greedy_result <- knapsack_greedy(items, W = 5)
  optimal_result <- knapsack_dynamic(items, W = 5)

  expect_equal(greedy_result$value, 40)
  expect_equal(optimal_result$value, 49)
  expect_lt(greedy_result$value, optimal_result$value)
})
