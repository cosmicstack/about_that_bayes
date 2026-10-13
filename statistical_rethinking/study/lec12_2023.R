library(tidyverse)
library(rethinking)

data("reedfrogs")
d <- reedfrogs
head(d)

d %>%
  ggplot(aes(x=1:48, y=propsurv)) +
  geom_point() +
  geom_vline(xintercept = c(17, 33), linetype = 3, alpha = 0.5) +
  geom_hline(yintercept = mean(d$propsurv), linetype = 2) +
  scale_x_continuous(name = "Tank", labels = c(1, 16, 32, 48), breaks = c(1, 16, 32, 48), limits = c(1, 48)) +
  scale_y_continuous(name = "Proportion of survival", labels = c(0, 0.5, 1), breaks = c(0, 0.5, 1), limits = c(0, 1)) +
  theme_light()

dat <- list(
  T = 1:48,
  S = d$surv,
  D = d$density,
  G = d$size,
  P = d$pred
)

m1 <- ulam(
  alist(
    S ~ dbinom(D, p),
    logit(p) <- a[T],
    a[T] ~ dnorm(a_bar, sigma),
    a_bar ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ),
  data = dat,
  chains = 4
)
trankplot(m1)
precis(m1, depth = 2)


# ============================================================================ #

N_groups <- 30
N_id <- 200
a0 <- (-2)
bZY <- (-0.5)
g <- sample(1:N_groups, size = N_id, replace = TRUE)
Ug <- rnorm(N_groups, 1.5)
X <- rnorm(N_id, Ug[g])
Z <- rnorm(N_groups)
Y <- rbern(N_id, p = inv_logit(a0 + X + Ug[g] + bZY*Z[g]))

dat <- list(Y = Y, X = X, Z = Z, g = g, Ng = N_groups)

mFE <- ulam(
  alist(
    Y ~ dbern(p),
    logit(p) <- a[g] + bX*X + bZ*Z[g],
    a[g] ~ dnorm(0, 10),
    c(bX, bZ) ~ dnorm(0, 1)
  ),
  data = dat,
  chains = 4
)
trankplot(mFE)
precis(mFE, depth = 2)

posterior <- extract.samples(mFE)
dens(posterior$bX)
dens(posterior$bZ)
