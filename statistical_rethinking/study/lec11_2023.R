library(tidyverse)
library(rethinking)
data(Trolley)

d <- Trolley
head(d)

# Simple case

dat <- list(
  R = d$response,
  A = d$action,
  I = d$intention,
  C = d$contact
)

m1 <- ulam(
  alist(
    R ~ dordlogit(phi, alpha),
    phi <- bA*A + bI*I + bC*C,
    c(bA, bI, bC) ~ dnorm(0, 0.5),
    alpha ~ dnorm(0, 1)
  ),
  data = dat,
  chains = 4,
  cores = 4
)
trankplot(m1)
precis(m1, depth = 2)

# Q: What is the difference between A & B below?
A <- sim(m1, data = list(A=0, I=0, C=0), n = 1e4)
B <- mcreplicate(100, sim(m1, data = list(A=0, I=0, C=0)), mc.cores = 6)

hist(A)
hist(B)

# Add effects of Gender

dat$G <- ifelse(d$male == 1, 2, 1)

m2 <- ulam(
  alist(
    R ~ dordlogit(phi, alpha),
    phi <- bA[G]*A + bI[G]*I + bC[G]*C,
    bA[G] ~ dnorm(0, 0.5),
    bI[G] ~ dnorm(0, 0.5),
    bC[G] ~ dnorm(0, 0.5),
    alpha ~ dnorm(0, 1)
  ),
  data = dat,
  chains = 4,
  cores = 4
)
trankplot(m2)
precis(m2, depth = 2)

# Ordered monotonic predictors
unique(d$edu)
ed.levels <- c(
  "Elementary School",
  "Middle School", 
  "Some High School", 
  "High School Graduate",
  "Some College",
  "Bachelor's Degree",
  "Master's Degree",
  "Graduate Degree"
)

d <- d %>%
  mutate(E = factor(edu, levels = ed.levels, labels = seq(1, 8)))

dat$E <- d$E
dat$a <- rep(2, 7)

m3 <- ulam(
  alist(
    R ~ dordlogit(phi, alpha),
    phi <- bE*sum(delta_j[1:E]) + bA*A + bI*I + bC*C,
    c(bA, bI, bC, bE) ~ dnorm(0, 0.5),
    alpha ~ dnorm(0, 1),
    vector[8]: delta_j <<- append_row(0, delta),
    simplex[7]: delta ~ dirichlet(a)
  ),
  data = dat,
  chains = 4,
  cores = 4
)
trankplot(m3)
precis(m3, depth = 2)

# Let's get crazy

dat$Y <- standardize(d$age)

m4 <- ulam(
  alist(
    R ~ dordlogit(phi, alpha),
    phi <- bE[G]*sum(delta_j[1:E]) + bA[G]*A + bI[G]*I + bC[G]*C + bY[G]*Y,
    alpha ~ dnorm(0, 1),
    bA[G] ~ dnorm(0, 0.5),
    bI[G] ~ dnorm(0, 0.5),
    bC[G] ~ dnorm(0, 0.5),
    bE[G] ~ dnorm(0, 0.5),
    bY[G] ~ dnorm(0, 0.5),
    vector[8]: delta_j <<- append_row(0, delta),
    simplex[7]: delta ~ dirichlet(a)
  ),
  data = dat,
  cores = 4,
  chains = 4,
  threads = 2
)
trankplot(m4)
precis(m4, depth = 2)
