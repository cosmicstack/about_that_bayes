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
head(d)

d <- d %>%
  mutate(
    P = if_else(P == "L", 1, 0),
    A = if_else(A == "A", 1, 0),
    V = if_else(V == "L", 1, 0)
  )

m.11h2.quap <- quap(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bP*P + bA*A + bV*V,
    a ~ dnorm(0, 1.5),
    c(bP, bA, bV) ~ dnorm(0, 0.5)
  ),
  data = d
)
plot(precis(m.11h2.quap))

m.11h2.mcmc <- ulam(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bP*P + bA*A + bV*V,
    a ~ dnorm(0, 1.5),
    c(bP, bA, bV) ~ dnorm(0, 0.5)
  ),
  data = list(y=d$y, n=d$n, P=d$P, A=d$A, V=d$V),
  chains = 4,
  cores = 16,
  log_lik = TRUE
)
traceplot(m.11h2.mcmc)
plot(precis(m.11h2.mcmc))

p <- link(m.11h2.mcmc)
p.mean <- apply(p, 2, mean)
p.PI <- apply(p, 2, PI)
data.frame(row=seq(1, 8), mean=p.mean, t(p.PI)) %>%
  dplyr::select(row, prob.low=X5., mean, prob.high=X94.) %>%
  ggplot(aes(row, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = prob.low, ymax = prob.high), width = 0.2) +
  scale_x_continuous(breaks = seq(1, 8)) +
  coord_flip() +
  theme_bw()

y.sim <- sim(m.11h2.mcmc)
y.sim.mean <- apply(y.sim, 2, mean)
y.sim.PI <- apply(y.sim, 2, PI)
data.frame(row=seq(1, 8), mean=y.sim.mean, t(y.sim.PI)) %>%
  dplyr::select(row, count.low=X5., mean, count.high=X94.) %>%
  ggplot(aes(row, mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = count.low, ymax = count.high), width = 0.2) +
  scale_x_continuous(breaks = seq(1, 8)) +
  coord_flip() +
  theme_bw()

m.11h2.mcmc.2 <- ulam(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bPA*P*A + bV*V,
    a ~ dnorm(0, 1.5),
    c(bPA, bV) ~ dnorm(0, 0.5)
  ),
  data = list(y=d$y, n=d$n, P=d$P, A=d$A, V=d$V),
  chains = 4,
  cores = 16,
  log_lik = TRUE
)

compare(m.11h2.mcmc, m.11h2.mcmc.2)

plot(compare(m.11h2.mcmc, m.11h2.mcmc.2))

# 11H3
data("salamanders")
d <- salamanders
head(d)

m.11h3.quap <- quap(
  alist(
    SALAMAN ~ dpois(lambda),
    log(lambda) <- a + b*PCTCOVER,
    a ~ dnorm(3, 0.5),
    b ~ dnorm(0, 0.2)
  ),
  data = d
)

prior <- extract.prior(m.11h3.quap)
data.frame(a = prior$a[1:20], b = prior$b[1:20], x1 = 0.2, x2 = 0.5, x3 = 0.8, grp = factor(seq(1, 20))) %>%
  pivot_longer(cols = c(x1, x2), values_to = "x") %>%
  mutate(
    lambda = exp(a + b*x),
    S = rpois(40, lambda)
  ) %>%
  ggplot(aes(x, S, color=grp)) +
  geom_line()

m.11h3.mcmc <- ulam(
  alist(
    SALAMAN ~ dpois(lambda),
    log(lambda) <- a + b*PCTCOVER,
    a ~ dnorm(3, 0.5),
    b ~ dnorm(0, 0.2)
  ),
  data = list(SALAMAN = d$SALAMAN, PCTCOVER = d$PCTCOVER),
  chains = 4,
  log_lik = TRUE
)

precis(m.11h3.quap)
precis(m.11h3.mcmc)

samples.quap <- extract.samples(m.11h3.quap, n=1e3)
samples.mcmc <- extract.samples(m.11h3.mcmc, n=1e3)

data.frame(quap = samples.quap$a, mcmc = samples.mcmc$a[1:1000]) %>%
  pivot_longer(cols = c(quap, mcmc), names_to = "type") %>%
  ggplot() +
  geom_density(aes(x = value, fill = type), alpha = 0.4) +
  theme_bw()

data.frame(quap = samples.quap$b, mcmc = samples.mcmc$b[1:1000]) %>%
  pivot_longer(cols = c(quap, mcmc), names_to = "type") %>%
  ggplot() +
  geom_density(aes(x = value, fill = type), alpha = 0.4) +
  theme_bw()

S <- sim(m.11h3.mcmc)
S.mean <- apply(S, 2, mean)
S.PI <- apply(S, 2, PI)
data.frame(pct_cover=d$PCTCOVER, mean=S.mean, t(S.PI)) %>%
  dplyr::select(pct_cover, count.low=X5., mean, count.high=X94.) %>%
  ggplot(aes(pct_cover, mean)) +
  geom_smooth() +
  geom_point(data = d, aes(PCTCOVER, SALAMAN), inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = count.low, ymax = count.high), alpha = 0.2) +
  theme_bw()


# 11H4
data("NWOGrants")
d <- NWOGrants
head(d)
