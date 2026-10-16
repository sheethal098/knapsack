# knapsack

<!-- badges: start -->
[![R-CMD-check](https://github.com/sheethal098/knapsack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sheethal098/knapsack/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
<!-- badges: end -->

An R package that solves the 0/1 knapsack problem with several algorithms and
compares their running times.

Given a set of items, each with a weight `w` and a value `v`, the goal is to
choose the items with the highest total value without exceeding a weight
limit `W`.

## Installation

Install the development version from GitHub, including the vignette:

```r
# install.packages("remotes")
remotes::install_github("sheethal098/knapsack", build_vignettes = TRUE)
```

## Functions

| Function | Method | Result |
|---|---|---|
| `brute_force_knapsack()` | Checks all 2^n combinations | Exact |
| `brute_force_knapsack_parallel()` | Brute force split across CPU cores | Exact |
| `knapsack_dynamic()` | Dynamic programming, O(nW) | Exact |
| `knapsack_greedy()` | Picks items by value/weight ratio | Approximate |
| `brute_force_knapsack_opt()` | Vectorized brute force | Exact |
| `knapsack_dynamic_opt()` | Vectorized dynamic programming | Exact |
| `knapsack_greedy_opt()` | Greedy without vector growth, with early exit | Approximate |

All functions take a data frame `x` with positive numeric columns `w` and `v`,
and a maximum weight `W`. They return a list with the total `value` and the
row indices in `elements`.

## Example

```r
library(knapsack)

items <- data.frame(
  w = c(2, 3, 4),
  v = c(3, 4, 5)
)

brute_force_knapsack(items, W = 5)
#> $value
#> [1] 7
#>
#> $elements
#> [1] 1 2

knapsack_dynamic(items, W = 5)
knapsack_greedy(items, W = 5)

# Parallel brute force
brute_force_knapsack_parallel(items, W = 5, parallel = TRUE)
```

## Notes on usage

- Brute force is limited to 25 items (22 for the vectorized version), because
  the number of combinations doubles with every extra item.
- `knapsack_dynamic()` needs whole-number weights and a whole-number `W`.
- The greedy heuristic is very fast and scales to millions of items, but it
  does not always find the optimal value.

## Vignette

The vignette answers how long each algorithm takes (brute force with 16
objects, dynamic programming with 500, greedy with 1,000,000), what
parallelizing brute force gains, and what profiling and optimization changed.

```r
browseVignettes("knapsack")
```

## License

MIT, see [LICENSE.md](LICENSE.md).
