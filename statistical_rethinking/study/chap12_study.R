library(tidyverse)
library(rethinking)

# 12.1.1 Beta-binomial
pbar <- 0.6
theta <- 25
curve(dbeta2(x, pbar, theta), from = 0, to = 1, xlab = "probability", ylab = "Density")

# Zero-Inflated
prob_drink <- 0.2
rate_work <- 1
N <- 365
set.seed(N)
drink <- rbinom(N, 1, prob_drink)
y <- (1-drink)*rpois(N, rate_work)

simplehist(y, xlab="manuscripts completed", lwd=4)
zeros_drink <- sum(drink)
zeros_work <- sum(y==0 & drink==0)
zeros_total <- sum(y==0)
lines(c(0,0), c(zeros_work, zeros_total), col=rangi2, lwd=4)

# Trolley problem revisited
data("Trolley")
d <- Trolley
dat <- list(
  R = d$response,
  A = d$action,
  I = d$intention,
  C = d$contact
)

m12.5 <- ulam(
  alist(
    R ~ dordlogit(phi, cutpoints),
    phi <- bA*A + bC*C + BI*I,
    BI <- bI + bIA*A + bIC*C,
    cutpoints ~ dnorm(0, 1.5),
    c(bA, bC, bI, bIA, bIC) ~ dnorm(0, 0.5)
  ),
  data = dat,
  chains = 4,
  cores = 4
)
trankplot(m12.5)

sim.d1 <- data.frame(
  A = 0,
  C = 0,
  I = 0:1
)

link.d1 <- link(m12.5, data = sim.d1)$phi
post <- extract.samples(m12.5)

plot(NULL, type="n", xlab="intention", ylab="prob", xlim=c(0,1), ylim=c(0,1))
for (s in 1:50) {
  pk <- pordlogit(1:6, link.d1[s, ], post$cutpoints[s, ])
  for (i in 1:6) { lines(0:1, pk[, i], col=grau(0.1))}
}

