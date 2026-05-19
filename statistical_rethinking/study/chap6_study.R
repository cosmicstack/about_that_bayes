library(rethinking)
data(milk)
d <- milk

sim.coll <- function(r = 0.9) {
  d$x <- rnorm(nrow(d), mean = r * d$perc.fat, sd = sqrt((1 - r^2) * var(d$perc.fat)))
  m <- lm(kcal.per.g ~ perc.fat + x, data = d)
  sqrt(diag(vcov(m)))[2]
}

rep.sim.coll <- function(r = 0.9, n = 100) {
  stddev <- replicate(n, sim.coll(r))
  mean(stddev)
}

r.seq <- seq(from = 0, to = 0.99, by = 0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r = z, n = 100))
plot(stddev ~ r.seq, type = "l", col = rangi2, lwd = 2, xlab = "correlation")

# POST-TREATMENT BIAS
set.seed(71)
N <- 100
h0 <- rnorm(N, 10, 2)
treatment <- rep(0:1, each = N/2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment*0.4)
h1 <- h0 + rnorm(N, 5 - 3*fungus)
d <- data.frame(h0, h1, treatment, fungus)
precis(d)

m6.3 <- quap(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- h0 * p,
    p <- a + bt*treatment + bf*fungus,
    a ~ dlnorm(0, 0.2),
    bt ~ dnorm(0, 0.1),
    bf ~ dnorm(0, 0.1),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m6.3)

prior <- extract.prior(m6.3)

h0_seq <- seq(from = min(h0), to = max(h0), length.out = 30)
mu_00 <- link(m6.3, post = prior, data = list(h0=h0_seq, treatment=rep(0, 30), fungus=rep(0, 30)))
mu_01 <- link(m6.3, post = prior, data = list(h0=h0_seq, treatment=rep(0, 30), fungus=rep(1, 30)))
mu_10 <- link(m6.3, post = prior, data = list(h0=h0_seq, treatment=rep(1, 30), fungus=rep(0, 30)))
mu_11 <- link(m6.3, post = prior, data = list(h0=h0_seq, treatment=rep(1, 30), fungus=rep(1, 30)))

dens(mu_00$p[, 1])
dens(mu_01$p[, 1])
dens(mu_10$p[, 1])
dens(mu_11$p[, 1])

### So except for treatment == 0 & fungus == 0, all other combinations push p into the negative

plot(NULL, xlim=c(min(h0), max(h0)), ylim=c(0, 30))
for (i in 1:50) lines(h0_seq, mu_00$mu[i, ], col = col.alpha("black", 0.3))

plot(NULL, xlim=c(min(h0), max(h0)), ylim=c(-10, 40))
for (i in 1:50) lines(h0_seq, mu_01$mu[i, ], col = col.alpha("black", 0.3))

plot(NULL, xlim=c(min(h0), max(h0)), ylim=c(-20, 40))
for (i in 1:50) lines(h0_seq, mu_10$mu[i, ], col = col.alpha("black", 0.3))

plot(NULL, xlim=c(min(h0), max(h0)), ylim=c(-20, 40))
for (i in 1:50) lines(h0_seq, mu_11$mu[i, ], col = col.alpha("black", 0.3))


m6.8 <- quap(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- h0*p,
    p <- a + bt*treatment,
    a ~ dlnorm(0, 0.2),
    bt ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m6.8)

## Unobserved variable
N <- 1000
h0 <- rnorm(N, 10, 2)
treatment <- rep(0:1, each=N/2)
M <- rbern(N)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment*0.4 + 0.4*M)
h1 <- h0 + rnorm(N, 5 + 3*M)
d2 <- data.frame(h0, h1, treatment, fungus)

m6.3.1 <- quap(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- h0 * p,
    p <- a + bt*treatment + bf*fungus,
    a ~ dlnorm(0, 0.2),
    bt ~ dnorm(0, 0.1),
    bf ~ dnorm(0, 0.1),
    sigma ~ dexp(1)
  ),
  data = d2
)
precis(m6.3.1)
# precis(m6.3)


m6.8.1 <- quap(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- h0*p,
    p <- a + bt*treatment,
    a ~ dlnorm(0, 0.2),
    bt ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d2
)
precis(m6.8.1)
# precis(m6.8)
