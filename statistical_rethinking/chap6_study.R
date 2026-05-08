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

### POST-TREATMENT BIAS ###
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
    bt ~ dnorm(0, 0.5),
    bf ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
precis(m6.3)

# need to fix
prior <- extract.prior(m6.3)
# h0_seq <- seq(from = min(h0), to = max(h0), length.out = 100)
h0_seq <- c(7, 20)
mu <- link(m6.3, post = prior, data = list(h0=h0_seq, treatment=c(0, 1), fungus=c(0, 1)))


plot(NULL, xlim=c(7, 20), ylim=c(-20, 50))
for (i in 1:50) lines(h0_seq, mu$mu[i, ], col = col.alpha("black", 0.3))

plot(NULL, xlim=c(7, 20), ylim=c(-1, 3))
for (i in 1:50) lines(h0_seq, mu$p[i, ], col = col.alpha("black", 0.3))
