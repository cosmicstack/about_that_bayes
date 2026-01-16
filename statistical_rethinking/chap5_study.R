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
    mu <- a * bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

precis(model5.1)

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
