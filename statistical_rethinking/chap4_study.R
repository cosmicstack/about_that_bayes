library(rethinking)
library(tidyverse)
data("Howell1")

d <- Howell1
precis(d)

d2 <- d[d$age >= 18, ]

curve(dnorm(x, 178, 20), from = 100, to = 250)
curve(dunif(x, 0, 50), from = -10, to = 60)

sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

# Grid Approximation

mu.list <- seq(from = 150, to = 160, length.out = 100)
sigma.list <- seq(from = 7, to = 9, length.out = 100)
post <- expand.grid(mu = mu.list, sigma = sigma.list)

post$LL <- sapply(1:nrow(post), function(i) sum(dnorm(d2$height, post$mu[i], post$sigma[i], log = T)))
post$prod <- post$LL + dnorm(post$mu, 178, 20, log = T) + dunif(post$sigma, 0, 50, log = T)
post$prob <- exp(post$prod - max(post$prod))

contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)

# Sampling from the posterior

sample.rows <- sample(1:nrow(post), size = 1e4, replace = T, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))
dens(sample.mu)
dens(sample.sigma)

PI(sample.mu)
PI(sample.sigma)

# ============================================================================ #

plot(d2$height, d2$weight)

xbar <- mean(d2$weight)

m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b * (weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

precis(m4.3)
pairs(m4.3)
round(vcov(m4.3), 3)

post_20 <- extract.samples(m4.3, n = 20)

plot(d2$height, d2$weight)
# not working; fix
for (i in 1:20) curve(post_20$a[i] + post_20$b[i] * (x - xbar), col = col.alpha("black", 0.3), add = T)

# ============================================================================ #

plot(height ~ weight, d)

d$weight_s <- (d$weight - mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2

m4.5 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight_s + b2*weight_s2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d
)

precis(m4.5)

weight.seq <- seq(from =-2.2, to = 2, length.out = 30)
pred_dat <- list(weight_s = weight.seq, weight_s2 = weight.seq^2)
mu <- link(m4.5, data = pred_dat)
dim(mu)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(m4.5, data = pred_dat)
dim(sim.height)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight, d, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

# ============================================================================ #

data("cherry_blossoms")
d <- cherry_blossoms
precis(d)
plot(doy ~ year, d)
