library(tidyverse)
library(rethinking)
library(dagitty)

N <- 1000
G <- sample(1:2, size = N, replace = TRUE)
D <- rbern(N, ifelse(G == 1, 0.3, 0.8)) + 1
accept_rate <- matrix(c(0.05, 0.12, 0.2, 0.2), nrow = 2)
A <- rbern(N, accept_rate[D, G])

dat <- list(
  G = G, 
  D = D, 
  A = A
)

m.total <- ulam(
  alist(
    A ~ dbern(p),
    logit(p) <- a[G],
    a[G] ~ dnorm(0, 1)
  ),
  data = dat,
  chains = 4,
  cores = 4
)
precis(m.total, depth = 2)

m.direct <- ulam(
  alist(
    A ~ dbern(p),
    logit(p) <- a[G, D],
    matrix[G, D]:a ~ dnorm(0, 1)
  ),
  data = dat,
  chains = 4,
  cores = 4
)
precis(m.direct, depth = 4)


plot(coeftab(m.total, m.direct))

dat2 <- aggregate(A ~ G + D, dat, sum)
dat2$N <- aggregate(A ~ G + D, dat, length)$A

m.direct.binomial <- ulam(
  alist(
    A ~ dbinom(N, p),
    logit(p) <- a[G, D],
    matrix[G, D]:a ~ dnorm(0, 1)
  ),
  data = dat2,
  chains = 4,
  cores = 4
)
precis(m.direct.binomial, depth = 4)

traceplot(m.direct.binomial)
trankplot(m.direct.binomial)
