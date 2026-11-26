library(tidyverse)
library(rethinking)

# 12M1
ratings <- c(12, 36, 7, 41)
ratings_prop <- ratings/sum(ratings)
ratings_prop_cumsum <- cumsum(ratings_prop)
ratings_prop_cumsum_odds <- log(ratings_prop_cumsum/(1-ratings_prop_cumsum))

# 12M2
data.frame(x=1:4, cumul_prop=ratings_prop_cumsum, prop=ratings_prop) %>%
  mutate(prev_cumul = lag(cumul_prop, default = 0)) %>%
  ggplot() +
  geom_col(aes(x, cumul_prop), width = 0.05, fill="grey") +
  geom_point(aes(x, cumul_prop), shape = 1) +
  geom_line(aes(x, cumul_prop)) +
  geom_rect(
    aes(
      xmin = x + 0.05 / 2,
      xmax = x + 0.05 * 1.5,
      ymin = prev_cumul,
      ymax = cumul_prop
    ),
    fill = "skyblue"
  ) +
  theme_bw()

# 12H1
data("Hurricanes")
d <- Hurricanes
head(d)

dat <- list(
  D = d$deaths,
  F = (d$femininity - min(d$femininity))/max(d$femininity),
  S = standardize(d$damage_norm),
  P = standardize(d$min_pressure)
)

m.12h1.1 <- ulam(
  alist(
    D ~ dpois(lambda),
    log(lambda) <- a + bF*F,
    a ~ dnorm(1, 1),
    bF ~ dnorm(0, 1)
  ),
  data = dat,
  chains = 4,
  cores = 4
)

m.12h1.2 <- ulam(
  alist(
    D ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(1, 0.5)
  ),
  data = dat,
  chains = 4,
  cores = 4
)

trankplot(m.12h1.1)
trankplot(m.12h1.2)

# Strength of association
plot(coeftab(m.12h1.1, m.12h1.2))
precis(m.12h1.1)

post <- extract.samples(m.12h1.1)
dens(exp(post$bF))
summary(exp(post$bF))

# Post check
postcheck(m.12h1.1, window = 100)
sims <- sim(m.12h1.1)
sims.mean <- apply(sims, 2, mean)
sims.PI <- apply(sims, 2, PI)

# 12H2
m.12h2 <- ulam(
  alist(
    D ~ dgampois(lambda, phi),
    log(lambda) <- a + bF*F,
    a ~ dnorm(1, 1),
    bF ~ dnorm(0, 1),
    phi ~ dexp(1.5)
  ),
  data = dat,
  chains = 4,
  cores = 4
)
trankplot(m.12h2)
precis(m.12h2)
post <- extract.samples(m.12h2)
dens(exp(post$bF))
PI(post$bF)

postcheck(m.12h2, window = 100)

# 12H3
m.12h3.1 <- ulam(
  alist(
    D ~ dgampois(lambda, phi),
    lambda <- exp(a + bFS*F*S),
    a ~ dnorm(1, 0.5),
    bFS ~ dnorm(0, 0.5),
    phi ~ dexp(1)
  ),
  data = dat,
  chains = 4,
  cores = 4,
  log_lik = T
)
trankplot(m.12h3.1)
precis(m.12h3.1)

m.12h3.2 <- ulam(
  alist(
    D ~ dgampois(lambda, phi),
    lambda <- exp(a + bFP*F*P),
    a ~ dnorm(1, 0.5),
    bFP ~ dnorm(0, 0.5),
    phi ~ dexp(1)
  ),
  data = dat,
  chains = 4,
  cores = 4,
  log_lik = T
)
trankplot(m.12h3.2)
precis(m.12h3.2)

m.12h3.3 <- ulam(
  alist(
    D ~ dgampois(lambda, phi),
    lambda <- exp(a + bFS*F*S + bFP*F*P),
    a ~ dnorm(1, 0.5),
    c(bFS, bFP) ~ dnorm(0, 0.5),
    phi ~ dexp(1)
  ),
  data = dat,
  chains = 4,
  cores = 4,
  log_lik = T
)
trankplot(m.12h3.3)
precis(m.12h3.3)

plot(coeftab(m.12h3.1, m.12h3.2, m.12h3.3), par = c("bFS", "bFP"))
plot(compare(m.12h3.1, m.12h3.2, m.12h3.3, func = WAIC))

dat.m.12h3.1 <- list(F = c(0.1, 0.9), S = 0)
sim.m.12h3.1 <- sim(m.12h3.1, data = dat.m.12h3.1)
dens(sim.m.12h3.1[, 2] - sim.m.12h3.1[, 1])

dat.m.12h3.2 <- list(F = c(0.1, 0.9), P = 0)
sim.m.12h3.2 <- sim(m.12h3.2, data = dat.m.12h3.2)
dens(sim.m.12h3.2[, 2] - sim.m.12h3.2[, 1])

dat.m.12h3.3 <- list(F = c(0.1, 0.9), S = 0, P = 0)
sim.m.12h3.3 <- sim(m.12h3.3, data = dat.m.12h3.3)
dens(sim.m.12h3.3[, 2] - sim.m.12h3.3[, 1])

# 12H4
