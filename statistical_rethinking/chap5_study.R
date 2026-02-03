library(tidyverse)
library(rethinking)

data("WaffleDivorce")
d <- WaffleDivorce

d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

ggplot(d, aes(A, M)) +
  geom_point()

model5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(model5.1)

model5.2 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(model5.2)

set.seed(10)
prior <- extract.prior(model5.1)
mu <- link(model5.1, data = list(A = c(-2, 2)), post = prior)
# tmp <- link(model5.1)

plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))
for (i in 1:100) lines(c(-2, 2), mu[i, ], col = col.alpha("black", 0.4))

A.seq <- seq(from = -3, to = 3.2, length.out = 50)
mu <- link(model5.1, data = list(A = A.seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)

plot(D ~ A, data = d, col = rangi2)
lines(A.seq, mu.mean, lwd = 2)
shade(mu.PI, A.seq)

library(dagitty)
dag5.1 <- dagitty("dag{A -> D; A -> M; M -> D}")
coordinates(dag5.1) <- list(x = c(A = 0, D = 1, M = 2), y = c(A = 0, D = 1, M = 0))
drawdag(dag5.1)

impliedConditionalIndependencies(dag5.1)

model5.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A + bM * M,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(model5.3)


model5.4 <- quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(model5.4)

plot(
  coeftab(
    model5.1,
    model5.2,
    model5.3,
    model5.4
  ),
  par = c("bA", "bM")
)

# PREDICTOR RESIDUAL PLOTS

mu <- link(model5.4)
mu_mean <- apply(mu, 2, mean)
mu_resid <- d$M - mu_mean

data.frame(marriage_rate_residuals = mu_resid, divorce_rate = d$D) %>%
  ggplot(aes(marriage_rate_residuals, divorce_rate)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x")

model5.5 <- quap(
  alist(
    A ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(model5.5)

mu <- link(model5.5)
mu_mean <- apply(mu, 2, mean)
age_resid <- d$A - mu_mean

data.frame(age_residuals = age_resid, divorce_rate = d$D) %>%
  ggplot(aes(age_residuals, divorce_rate)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x")

# POSTERIOR PREDICTION PLOTS

mu <- link(model5.3)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)

D.sim <- sim(model5.3, n=1e4)
D.PI <- apply(D.sim, 2, PI)

plot(mu.mean ~ d$D, col = rangi2, ylim = range(mu.PI), xlab = "Observed", ylab = "Predicted")
abline(a=0, b=1, lty=2)
for (i in 1:nrow(d)) lines(rep(d$D[i], 2), mu.PI[, i], col = rangi2)
identify(x = d$D, y = mu.mean, labels = d$Loc)

# COUNTERFACTUAL PLOTS

model5.3_A <- quap(
  alist(
   D ~ dnorm(mu, sigma),
   mu <- a + bA * A + bM * M,
   a ~ dnorm(0, 0.2),
   bM ~ dnorm(0, 0.5),
   bA ~ dnorm(0, 0.5),
   sigma ~ dexp(1),
   
   M ~ dnorm(mu_M, sigma_M),
   mu_M <- a_M + bAM * A,
   a_M ~ dnorm(0, 0.2),
   bAM ~ dnorm(0, 0.5),
   sigma_M ~ dexp(1)
  ),
  data = d
)

precis(model5.3_A)

A_seq <- seq(-2, 2, length.out = 30)
sim_dat <- data.frame(A=A_seq)

# The order of vars below is also the order of simulation
s <- sim(model5.3_A, data = data.frame(A=A_seq), vars = c("M", "D"))

s$M[1:5, 1:5]

plot(sim_dat$A, colMeans(s$D), ylim = c(-2,2), type = "l", xlab = "Manipulated A", ylab = "counterfactual D")
shade(apply(s$D, 2, PI), sim_dat$A)
mtext("Total counterfactual effect of A on D")

# Total counterfactual effect of A on D
data.frame(
  A = sim_dat$A,
  D = colMeans(s$D),
  D = t(apply(s$D, 2, PI))
) %>%
  select(A, D, ci_lower = D.5., ci_upper = D.94.) %>%
  ggplot() +
  geom_line(aes(A, D)) +
  geom_ribbon(aes(
    x = A,
    ymin = ci_lower,
    ymax = ci_upper,
  ),
  color = "grey",
  alpha = 0.2
)

# Counterfactual effect of A on M
data.frame(
  A = sim_dat$A,
  M = colMeans(s$M),
  M = t(apply(s$M, 2, PI))
) %>%
  select(A, M, ci_lower = M.5., ci_upper = M.94.) %>%
  ggplot() +
  geom_line(aes(A, M)) +
  geom_ribbon(aes(
    x = A,
    ymin = ci_lower,
    ymax = ci_upper,
  ),
  color = "grey",
  alpha = 0.2
  )
