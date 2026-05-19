library(tidyverse)
library(rethinking)

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = T)

# 3E1
sum(samples < 0.2) / length(samples)

# 3E2
sum(samples > 0.8) / length(samples)

# 3E3
sum(samples > 0.2 & samples < 0.8) / length(samples)

# 3E4
quantile(samples, 0.2)

# 3E5
quantile(samples, 0.8)

# 3E6
HPDI(samples, prob = 0.66)

# 3E7
PI(samples, prob = 0.66)

# 3M1
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size = 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

# 3M2
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = T)
HPDI(samples, prob = 0.9)

# 3M3
w <- rbinom(1e4, 15, prob = samples)
hist(w)
sum(w == 8)/length(w)

# 3M4
w <- rbinom(1e4, 9, prob = samples)
hist(w)
sum(w == 6)/length(w)
