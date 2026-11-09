library(tidyverse)
library(rethinking)

# 12.1.1 Beta-binomial
pbar <- 0.6
theta <- 25
curve(dbeta2(x, pbar, theta), from = 0, to = 1, xlab = "probability", ylab = "Density")

