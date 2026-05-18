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
d <- data(WaffleDivorce)
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
impliedConditionalIndependencies(equivalentDAGs(dag.1)[1][[1]])

adjustmentSets(dag.1, exposure = "W", outcome = "D")

