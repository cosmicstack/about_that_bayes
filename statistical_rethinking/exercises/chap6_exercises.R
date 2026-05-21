library(tidyverse)
library(rethinking)
library(dagitty)

# 6M2
# X -> Z -> Y
N <- 1000
X <- rnorm(N)
Z <- rnorm(N, mean = X, sd = 0.1)
Y <- rnorm(N, mean = Z)

cor(X, Z)

d <- data.frame(X, Z, Y)

m.6m2 <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bX*X + bZ*Z,
    c(a, bX, bZ) ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = d
)
plot(precis(m.6m2))

m.6m2_1 <- quap(
  alist(
    Y ~ dnorm(mu, sigma),
    mu <- a + bX*X,
    c(a, bX) ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m.6m2_1)

# 6H1
# Total Causal Influence on number of Waffle Houses on Divorce Rate
data("WaffleDivorce")
d <- WaffleDivorce
head(d)

dag.1 <- dagitty("dag{
  A -> D
  A -> M -> D
  A <- S -> M
  S -> W -> D
}")

coordinates(dag.1) <- list(
  x = c(S = 0, M = 1, W = 2, A = 0, D = 2),
  y = c(S = 0, W = 0, M = 1, A = 2, D = 2)
)
drawdag(dag.1)
drawdag(equivalentDAGs(dag.1))

impliedConditionalIndependencies(dag.1)
# impliedConditionalIndependencies(equivalentDAGs(dag.1)[1][[1]])

adjustmentSets(dag.1, exposure = "W", outcome = "D")

d$S <- standardize(d$South)
d$W <- standardize(d$WaffleHouses)
d$A <- standardize(d$MedianAgeMarriage)
d$M <- standardize(d$Marriage)
d$D <- standardize(d$Divorce)

d2 <- d %>%
  select(S, A, M, D, W)

head(d2)
summary(d2)

m1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bS*S + bW*W,
    a ~ dnorm(0, 0.2),
    c(bS, bW) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d2
)
precis(m1)
plot(precis(m1))

# 6H2
impliedConditionalIndependencies(dag.1)

m1 <- quap(
  alist(
    A ~ dnorm(mu, sigma),
    mu <- a + bS*S + bW*W,
    a ~ dnorm(0, 0.2),
    c(bS, bW) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d2
)

m2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bS*S + bW*W + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    c(bS, bW, bM, bA) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d2
)

m3 <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bS*S + bW*W,
    a ~ dnorm(0, 0.2),
    c(bS, bW) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d2
)

plot(coeftab(m1, m2, m3), par=c("bS", "bM", "bA", "bW"))

# So D is not d-separated from S | A, M, W. I think we should add S -> D. Let's check:

dag.2 <- dagitty("dag{
  A -> D
  A -> M -> D
  A <- S -> M
  S -> W -> D
  S -> D
}")

impliedConditionalIndependencies(dag.2)
# A _||_ W | S
# M _||_ W | S

# 6H3
# Prep
set.seed(37)
data("foxes")
d <- foxes
head(d)

dag.6h3 <- dagitty("dag{
  A -> F -> G -> W
  A -> F -> W
}")
coordinates(dag.6h3) <- list(
  x = c(G = 1, A = 0, F = 1, W = 2),
  y = c(G = 0, A = 1, F = 1, W = 1)
)
drawdag(dag.6h3)

d$A <- standardize(d$area)
d$F <- standardize(d$avgfood)
d$G <- standardize(d$groupsize)
d$W <- standardize(d$weight)

# Total causal influence of area (A) on weight (W)
adjustmentSets(dag.6h3, exposure = "A", outcome = "W")

m.6h3 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m.6h3)

priors <- extract.prior(m.6h3)
xseq <- c(-3, 3)

mu <- link(m.6h3, post = priors, data=list(A=xseq))
plot(NULL, xlim=xseq, ylim=xseq, xlab = "A (Area)", ylab = "E(Weight)")
for(i in 1:50) lines(xseq, mu[i, ], col=col.alpha("black", 0.3))

# Prior Predictive Sim
mu <- sim(m.6h3, post = priors, data = list(A = xseq))
plot(NULL, xlim=xseq, ylim=c(-6, 6), xlab = "A (Area)", ylab = "W (Weight)")
for(i in 1:50) lines(xseq, mu[i, ], col=col.alpha("black", 0.3))

# Would increasing the area available to each fox make it heavier?
# What I need is a contrast from the current values, to increased values of A

samples <- extract.samples(m.6h3)
EW <- link(m.6h3, post = samples, data = list(A = 0))
EW.cf <- link(m.6h3, post = samples, data = list(A = 1))

contrast <- EW.cf - EW
dens(contrast)
summary(contrast)

# 6H4
adjustmentSets(dag.6h3, exposure = "F", outcome = "W")
m.6h4 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bF*F,
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m.6h4)

# 6H5
adjustmentSets(dag.6h3, exposure = "G", outcome = "W")
m.6h5 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bF*F + bG*G,
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.3),
    bG ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m.6h5)

plot(coeftab(m.6h3, m.6h4, m.6h5), par=c("bF", "bG", "bA"))

# ===

adjustmentSets(dag.6h3, exposure = "F", outcome = "G")
m2 <- quap(
  alist(
    G ~ dnorm(mu, sigma),
    mu <- a + bF*F,
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m2)
