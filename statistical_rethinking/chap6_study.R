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
