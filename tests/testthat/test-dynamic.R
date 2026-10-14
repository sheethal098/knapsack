
test_that("dynamic programming finds the optimal value", {
  items <- data.frame(
    w = c(2, 3, 4),
    v = c(10, 15, 18)
  )

  result <- knapsack_dynamic(items, W = 5)

  expect_equal(result$value, 25)
  expect_equal(result$elements, c(1L, 2L))
})

test_that("dynamic programming rejects invalid capacity", {
  items <- data.frame(
    w = c(2, 3),
    v = c(10, 15)
  )

  expect_error(knapsack_dynamic(items, W = 0))
})
