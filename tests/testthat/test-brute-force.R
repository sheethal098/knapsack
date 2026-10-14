
test_that("brute force finds the best combination", {
  items <- data.frame(
    w = c(2, 3, 4),
    v = c(10, 15, 18)
  )

  result <- brute_force_knapsack(items, W = 5)

  expect_equal(result$value, 25)
  expect_equal(result$elements, c(1L, 2L))
})

test_that("brute force rejects invalid capacity", {
  items <- data.frame(
    w = c(2, 3),
    v = c(10, 15)
  )

  expect_error(brute_force_knapsack(items, W = 0))
})
