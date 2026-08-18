library(tidyverse)
library(rethinking)

data("chimpanzees")
d <- chimpanzees
head(d)

d$treatment <- 1 + d$prosoc_left + 2*d$condition

xtabs(~ treatment + prosoc_left + condition, d)

dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  treatment = as.integer(d$treatment)
)

m.test <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    # a ~ dnorm(0, 10)
    a ~ dnorm(0, 1.5)
  ),
  data = dat_list,
  cores = 4,
  chains = 4
)

traceplot(m.test)

prior <- extract.prior(m.test, n=1e4)
p <- inv_logit(prior$a)
dens(p, adj = 0.1, show.HPDI = T)

m.test2 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + b[treatment],
    a ~ dnorm(0, 1.5),
    # b[treatment] ~ dnorm(0, 10)
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = dat_list,
  cores = 4,
  chains = 4
)

traceplot(m.test2)

prior <- extract.prior(m.test2, n = 1e4)
p <- sapply(1:4, function(k) inv_logit(prior$a + prior$b[, k]))
dens(abs(p[, 3] - p[, 4]), adj = 0.1)


m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = dat_list,
  cores = 4,
  chains = 4,
  log_lik = TRUE
)

precis(m11.4, depth = 2)
traceplot(m11.4)
