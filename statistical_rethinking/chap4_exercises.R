library(rethinking)
library(tidyverse)
library(splines)

# 4M1
# Simulate observed y values from the prior

prior.sim <- matrix(data = NA, nrow = 1000, ncol = 3, dimnames = list(NULL, c("mu", "sigma", "y")))
for (i in 1:1000) {
  prior.sim[i, 1] <- rnorm(1, mean = 0, sd = 10)
  prior.sim[i, 2] <- rexp(1)
  prior.sim[i, 3] <- rnorm(1, prior.sim[i, 1], prior.sim[i, 2])
}

hist(prior.sim[, 1])
hist(prior.sim[, 2])
hist(prior.sim[, 3])

mean(prior.sim[, 1])
mean(prior.sim[, 3])

mu <- rnorm(1e4, mean = 0, sd = 10)
sigma <- rexp(1e4)
y <- rnorm(1e4, mu, sigma)
dens(y)

# 4M2

# model.1 <- quap(
#   alist(
#     y ~ dnorm(mu, sigma),
#     mu ~ dnorm(0, 10),
#     sigma ~ dexp(1)
#   ),
#   data = list(),
#   start = list()
# )