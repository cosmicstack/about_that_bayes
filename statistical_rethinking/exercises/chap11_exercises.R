library(tidyverse)
library(rethinking)

# 11M7
data("chimpanzees")
d <- chimpanzees
head(d)

d$treatment <- 1 + d$prosoc_left + 2*d$condition

m11.4_quap <- quap(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = d
)

precis(m11.4_quap, depth = 2)

samples <- extract.samples(m11.4_quap)

dens(samples$a[, 2])

m11.4_quap <- quap(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 10),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = d
)

m11.4_mcmc <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 10),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = list(pulled_left = d$pulled_left, actor = d$actor, treatment = d$treatment),
  chains = 4,
  log_lik = TRUE
)
traceplot(m11.4_mcmc)

samples.quap <- extract.samples(m11.4_quap)
samples.mcmc <- extract.samples(m11.4_mcmc)

data.frame(quap = samples.quap$a[, 2], mcmc = samples.mcmc$a[, 2]) %>%
  pivot_longer(cols = c("quap", "mcmc"), names_to = "algorithm", values_to = "samples") %>%
  ggplot(aes(x = samples, color = algorithm)) +
  geom_density() +
  theme_classic()

# 11M8
data("Kline")
d <- Kline
head(d)
d$P <- log(d$population)
d$contact_id <- ifelse(d$contact == "high", 2, 1)

d2 <- d %>%
  filter(culture != "Hawaii")

m8.1 <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- exp(a[cid])*P^b[cid]/g,
    a[cid] ~ dnorm(1, 1),
    b[cid] ~ dexp(1),
    g ~ dexp(1)
  ),
  data = list(T=d$total_tools, P=d$population, cid=d$contact_id),
  chains = 4,
  log_lik = TRUE
)

m8.2 <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- exp(a[cid])*P^b[cid]/g,
    a[cid] ~ dnorm(1, 1),
    b[cid] ~ dexp(1),
    g ~ dexp(1)
  ),
  data = list(T=d2$total_tools, P=d2$population, cid=d2$contact_id),
  chains = 4,
  log_lik = TRUE
)

precis(m8.1, depth = 2)
precis(m8.2, depth = 2)

compare(m8.1, m8.2, func=PSIS)

# 11H1
m11.1 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 1.5)
  ),
  data = list(pulled_left = d$pulled_left),
  chains = 4,
  log_lik = TRUE
)

m11.3 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + b[treatment],
    a ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = list(pulled_left = d$pulled_left, actor = d$actor, treatment = d$treatment),
  chains = 4,
  log_lik = TRUE
)

m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = list(pulled_left = d$pulled_left, actor = d$actor, treatment = d$treatment),
  chains = 4,
  log_lik = TRUE
)

plot(compare(m11.1, m11.3, m11.4, func = WAIC))

# Sidetrack
data("Howell2")
tmp <- Howell2

tmp <- tmp %>%
  filter(!is.na(height) & !is.na(weight) & age > 17)

head(tmp)

m.tmp <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = tmp
)
precis(m.tmp)

mu <- link(m.tmp)
mu.sim <- sim(m.tmp)

# 11H2
library(MASS)
data("eagles")
d <- eagles

