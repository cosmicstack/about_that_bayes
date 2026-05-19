library(tidyverse)
library(rethinking)
library(dagitty)

# Quick recap on spurious association and masked relationships
# Let me use the same symbols for two different DAGs
dag.spurious <- dagitty("dag{Z -> X; Z -> Y}")
coordinates(dag.spurious) <- list(x = c(X = 0, Y = 1, Z = 2), y = c(X = 0, Y = 1, Z = 0))
drawdag(dag.spurious)

dag.masked.1 <- dagitty("dag{Z <-> X; X -> Y; Z -> Y}")
coordinates(dag.masked.1) <- list(x = c(X = 0, Y = 1, Z = 2), y = c(X = 0, Y = 1, Z = 0))
drawdag(dag.masked.1)

dag.masked.2 <- dagitty("dag{X -> Z; X -> Y; Z -> Y}")
coordinates(dag.masked.2) <- list(x = c(X = 0, Y = 1, Z = 2), y = c(X = 0, Y = 1, Z = 0))
drawdag(dag.masked.2)

# while dag.spurious is a spurious association, for a masked relationship,
# dag.masked.1 or dag.masked.2 could work form the data alone (markov equivalency)

dag.masked.3 <- dagitty("dag{U -> Z; U -> X; X -> Y; Z -> Y}")
coordinates(dag.masked.3) <- list(x = c(X = 0, U = 1, Y = 1, Z = 2), y = c(X = 0, U = 0, Y = 1, Z = 0))
drawdag(dag.masked.3)


# 5M4
data("WaffleDivorce")
d <- WaffleDivorce

lds.pop <- read.csv("lds.csv")
lds.pop <- lds.pop %>%
  select(Loc = Abbreviation, lds.prop = Proportion)

d <- d %>%
  inner_join(lds.pop, by = "Loc")

d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)
d$L <- standardize(d$lds.prop)

d %>%
  select(D, M, A, L)

dag.5m4 <- dagitty("dag{A -> M; A -> D; M -> D; L -> M; L ->D}")
coordinates(dag.5m4) <- list(x = c(A = 0, M = 1, D = 1, L = 2), y = c(A = 0, M = 0, D = 1, L = 0))
drawdag(dag.5m4)

# For 5M4, we aren't doing any causal interpretation; just predictive using regression

model.5m4 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A + bM*M + bL*L,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    bL ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(model.5m4)


# SIDE BAR
d.general <- seq(0, 1, 0.1)
d.lds <- seq(0, 1, 0.1)
p <- seq(0, 1, 0.1)

test.grid <- expand.grid(DG = d.general, D.LDS = d.lds, p = p)
test.grid$D <- (1 - test.grid$p) * test.grid$DG + test.grid$p * test.grid$D.LDS

ggplot(test.grid, aes(DG, D)) +
  geom_point()

hist(test.grid$D)

# END SIDE BAR

# 5H1

dag.5h1 <- dagitty("dag{M -> A; A -> D}")
drawdag(dag.5h1)
ME.list <- equivalentDAGs(dag.5h1)
drawdag(ME.list)

impliedConditionalIndependencies(dag.5h1)

# 5H2

model.5h2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA*A + bM*M,
    a ~ dnorm(0, 0.2),
    c(bA, bM) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(model.5h2)

# Testing
s <- sim(model.5h2, data = data.frame(M=seq(-2, 2, length.out = 30), A=0), vars="D")

mean.D <- apply(s, 2, mean)
PI.D <- apply(s, 2, PI)

data.frame(manipulated.M = M_seq, counterfactual.D = mean.D, t(PI.D)) %>%
  ggplot() +
  geom_line(aes(manipulated.M, counterfactual.D)) +
  geom_ribbon(aes(
    x = manipulated.M,
    ymin = X5.,
    ymax = X94.
  ),
  alpha = 0.2
  )

# Halve the marriage age
M_seq <- d$M/2
s <- sim(model.5h2, data = data.frame(M=M_seq, A=d$A), vars=c("D"))

mean.D <- apply(s, 2, mean)
PI.D <- apply(s, 2, PI)

data.frame(manipulated.M = M_seq, counterfactual.D = mean.D, t(PI.D)) %>%
  ggplot() +
  geom_line(aes(manipulated.M, counterfactual.D)) +
  geom_smooth(aes(manipulated.M, counterfactual.D), method = "lm") +
  geom_ribbon(aes(
    x = manipulated.M,
    ymin = X5.,
    ymax = X94.
    ),
    alpha = 0.2
  )

# 5H3
data(milk)
d <- milk

d$K <- standardize(d$kcal.per.g)
d$N <- standardize(d$neocortex.perc)
d$M <- standardize(log(d$mass))

dcc <- d[complete.cases(d$K, d$N, d$M), ]

dag.5h3 <- dagitty("dag{K <- M -> N -> K}")
coordinates(dag.5h3) <- list(x = c(M = 0, K = 1, N = 2), y = c(M = 0, K = 1, N = 0))
drawdag(dag.5h3)

model.5h3 <- quap(
  alist(
    # M -> N -> K
    K ~ dnorm(mu, sigma),
    mu <- a + bN*N + bM*M,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1),
    
    # M -> K
    K ~ dnorm(mu.K, sigma.K),
    mu.K <- aK + bMK*M,
    aK ~ dnorm(0, 0.2),
    bMK ~ dnorm(0, 0.5),
    sigma.K ~ dexp(1)
  ),
  data = dcc
)
precis(model.5h3, depth = 2)

M.seq <- dcc$M * 2
s <- sim(model.5h3, data = data.frame(M = M.seq), vars=c("N", "K"))

N.mean <- apply(s$N, 2, mean)
N.PI <- apply(s$N, 2, PI)
rownames(N.PI) <- c("cf.N.low.CI", "cf.N.high.CI")

K.mean <- apply(s$K, 2, mean)
K.PI <- apply(s$K, 2, PI)
rownames(K.PI) <- c("cf.K.low.CI", "cf.K.high.CI")

dss.plot.df <- data.frame(manipulated.M = M.seq, cf.N = N.mean, t(N.PI), cf.K = K.mean, t(K.PI))

ggplot(dss.plot.df, aes(manipulated.M, cf.N)) +
  geom_line() +
  geom_ribbon(
    aes(
      ymin = cf.N.low.CI,
      ymax = cf.N.high.CI
    ),
    alpha = 0.2
  )

ggplot(dss.plot.df, aes(manipulated.M, cf.K)) +
  geom_line() +
  geom_ribbon(
    aes(
      ymin = cf.K.low.CI,
      ymax = cf.K.high.CI
    ),
    alpha = 0.2
  )
