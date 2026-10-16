devtools::load_all()
library(profvis)

# Same data as the vignette
suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

n_big <- 1000000
knapsack_big <- data.frame(
  w = sample(1:4000, size = n_big, replace = TRUE),
  v = runif(n = n_big, 0, 10000)
)

items16  <- knapsack_objects[1:16, ]
items500 <- knapsack_objects[1:500, ]

# ---- 1. Profiling the originals (find the bottlenecks) ----
p_brute  <- profvis(brute_force_knapsack(items16, W = 3500))
p_dyn    <- profvis(knapsack_dynamic(items500, W = 3500))
p_greedy <- profvis(knapsack_greedy(knapsack_big, W = 3500))

p_brute
p_dyn
p_greedy


# ---- 2. Profiling the optimized versions ----
profvis(brute_force_knapsack_opt(items16, W = 3500))
profvis(knapsack_dynamic_opt(items500, W = 3500))
profvis(knapsack_greedy_opt(knapsack_big, W = 3500))

# ---- 3. Checking the results are the same ----
all.equal(brute_force_knapsack(items16, 3500),
          brute_force_knapsack_opt(items16, 3500))
all.equal(knapsack_dynamic(items500, 3500),
          knapsack_dynamic_opt(items500, 3500))
all.equal(knapsack_greedy(knapsack_big, 3500),
          knapsack_greedy_opt(knapsack_big, 3500))

# ---- 4. Comparing timings ----
time_it <- function(expr) system.time(expr)[["elapsed"]]

comparison <- data.frame(
  algorithm = c("Brute force (n=16)", "Dynamic (n=500)", "Greedy (n=1e6)"),
  old = c(
    time_it(brute_force_knapsack(items16, 3500)),
    time_it(knapsack_dynamic(items500, 3500)),
    time_it(knapsack_greedy(knapsack_big, 3500))
  ),
  new = c(
    time_it(brute_force_knapsack_opt(items16, 3500)),
    time_it(knapsack_dynamic_opt(items500, 3500)),
    time_it(knapsack_greedy_opt(knapsack_big, 3500))
  )
)
comparison$speedup <- round(comparison$old / comparison$new, 1)
comparison

bm <- bench::mark(
  old = brute_force_knapsack(items16, 3500),
  new = brute_force_knapsack_opt(items16, 3500),
  check = FALSE,
  iterations = 20
)
bm[, c("expression", "median", "mem_alloc")]
as.numeric(bm$median[1] / bm$median[2])
