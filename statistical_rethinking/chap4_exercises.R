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

# 4M7

data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

xbar <- mean(d2$weight)
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

m4.3.revised <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

post.m4.3 <- extract.samples(m4.3)
post.m4.3.revised <- extract.samples(m4.3.revised)

data.frame(post.m4.3) %>%
  pivot_longer(cols = c("a", "b", "sigma"), names_to = "metric", values_to = "value") %>%
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(vars(metric), scale = "free_x")

data.frame(post.m4.3.revised) %>%
  pivot_longer(cols = c("a", "b", "sigma"), names_to = "metric", values_to = "value") %>%
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(vars(metric), scale = "free_x")

vcov(m4.3)
vcov(m4.3.revised)

# Compatins posterior predictions of both models
weight.seq <- seq(from = 25, to = 70, by = 1)
sim.height.m4.3 <- sim(m4.3, data = list(weight=weight.seq))
sim.height.m4.3.revised <- sim(m4.3.revised, data = list(weight=weight.seq))

apply(sim.height.m4.3, 2, PI, prob=0.89)
apply(sim.height.m4.3.revised, 2, PI, prob=0.89)

# 4M8
library(splines)

data("cherry_blossoms")
d <- cherry_blossoms
precis(d)
d2 <- d[complete.cases(d$doy), ]
num_knots <- 15
knot_list <- quantile(d2$year, probs = seq(0, 1, length.out = num_knots))

B <- bs(d2$year, knots = knot_list[-c(1, num_knots)], degree = 3, intercept = T)
model <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = list(D = d2$doy, B=B),
  start = list(w = rep(0, ncol(B)))
)

post <- extract.samples(model)
w <- apply(post$w, 2, mean)
mu <- link(model)
mu_PI <- apply(mu, 2, PI, prob=0.89)
plot(d2$year, d2$doy, col = col.alpha(rangi2, 0.3), pch=16)
shade(mu_PI, d2$year, col = col.alpha("black", 0.5))

# Experiment with perturbations
num_knots <- 20
knot_list <- quantile(d2$year, probs = seq(0, 1, length.out = num_knots))

B <- bs(d2$year, knots = knot_list[-c(1, num_knots)], degree = 3, intercept = T)
model <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(100, 10),
    w ~ dnorm(0, 15),
    sigma ~ dexp(1)
  ),
  data = list(D = d2$doy, B=B),
  start = list(w = rep(0, ncol(B)))
)

post <- extract.samples(model)
w <- apply(post$w, 2, mean)
mu <- link(model)
mu_PI <- apply(mu, 2, PI, prob=0.89)
plot(d2$year, d2$doy, col = col.alpha(rangi2, 0.3), pch=16, main = "num_knots = 20, weights sd = 15")
shade(mu_PI, d2$year, col = col.alpha("black", 0.5))

# 4H1

data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

model.4h1 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * weight,
    a ~ dnorm(178, 10),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

weights.4h1 <- c(46.95, 43.72, 64.78, 32.59, 54.63)

heights.4h1 <- link(model.4h1, data = data.frame(weight=weights.4h1))
expected_heights.4h1 <- apply(heights.4h1, 2, mean)
heights.4h1_89p.interval <- apply(heights.4h1, 2, PI)
data.frame(
  "id" = seq(1:5),
  "weight" = weights.4h1,
  "expected height" = expected_heights.4h1,
  "lower" = heights.4h1_89p.interval[1, ], 
  "upper" = heights.4h1_89p.interval[2, ]  
) %>%
  ggplot(aes(id, expected.height)) +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper,
      width = 0.1
    )
  )
