library(rethinking)
library(tidyverse)

data("rugged")
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$log_gdp), ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse(dd$cont_africa == 1, 1, 2)

dat_slim <- list(
  log_gdp_std = dd$log_gdp_std,
  rugged_std = dd$rugged_std,
  cid = as.integer(dd$cid)
)

## EXAMPLE ##
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = dat_slim,
  chains = 4,
  cores = 4,
  warmup = 100,
  iter = 1000
)

precis(m9.1, depth = 2)

prior <- extract.prior(m9.1)
mu <- link(m9.1, post = prior)
## ======= ##

# 9M1
m.9m1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dunif(0, 1)
  ),
  data = dat_slim,
  chains = 4,
  cores = 4
)

precis(m.9m1, depth = 2)

# 9M2
m.9m2 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dexp(0.3),
    sigma ~ dexp(1)
  ),
  data = dat_slim,
  chains = 4,
  cores = 4
)

precis(m.9m2, depth = 2)

plot(coeftab(m.9m1, m.9m2), pars=c("b[1]", "b[2]"))

# 9M3
warmup <- seq(100, 1000, 100)
n.eff.mat <- matrix(NA, nrow = 5, ncol = 10)

for (i in 1:10) {
  m.9m3 <- ulam(
    alist(
      log_gdp_std ~ dnorm(mu, sigma),
      mu <- a[cid] + b[cid] * (rugged_std - 0.215),
      a[cid] ~ dnorm(1, 0.1),
      b[cid] ~ dnorm(0, 0.3),
      sigma ~ dexp(1)
    ),
    data = dat_slim,
    chains = 4,
    cores = 4,
    warmup = warmup[i],
    iter = 2000
  )
  
  n.eff.mat[, i] <- precis(m.9m3, depth = 2)$ess_bulk
}

data.frame(parameters = c("a[1]", "a[2]", "b[1]", "b[2]", "sigma"), n.eff.mat) %>%
  pivot_longer(cols = starts_with("X"), names_to = "warmup.iters", names_prefix = "X", values_to = "n.eff") %>%
  mutate(
    warmup.iters = as.integer(warmup.iters)*100
  ) %>%
  ggplot(aes(warmup.iters, n.eff, color = parameters)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(100, 1000, 100))

# 9H1

mp <- ulam(
  alist(
    a ~ dnorm(0, 1),
    b ~ dcauchy(0, 1)
  ),
  data = list(y = 1),
  chains = 1
)

precis(mp)
traceplot(mp)
