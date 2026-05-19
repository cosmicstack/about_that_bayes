library(tidyverse)

# 2M1 (a)
p_grid <- seq(0, 1, length.out = 50)
prior <- rep(1, 50)
likelihood <- dbinom(3, 3, p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior/sum(unstd.posterior)

plot(p_grid, posterior, type = "b")

compute_posterior <- function(num_success, num_trials, prior) {
  p_grid <- seq(0, 1, length.out = 50)
  likelihood <- dbinom(num_success, num_trials, prob = p_grid)
  unstd.posterior <- likelihood * prior
  posterior <- unstd.posterior/sum(unstd.posterior)
  return(posterior)
}

# 2M1 (b)
posterior <- compute_posterior(3, 4, prior)
plot(p_grid, posterior, type = "b")

# 2M1 (c)
posterior <- compute_posterior(5, 7, prior)
plot(p_grid, posterior, type = "b")

# 2M2 (a)
prior <- if_else(p_grid < 0.5, 0, 1)
posterior <- compute_posterior(3, 3, prior)
plot(p_grid, posterior, type = "b")

# 2M2 (b)
posterior <- compute_posterior(3, 4, prior)
plot(p_grid, posterior, type = "b")

# 2M2 (c)
posterior <- compute_posterior(5, 7, prior)
plot(p_grid, posterior, type = "b")
