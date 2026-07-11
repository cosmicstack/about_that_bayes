library(tidyverse)
library(rethinking)

data("tulips")
d <- tulips
d$blooms_std <- d$blooms/max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)
head(d)

# 8M4

model.8m4 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a + bW*water_cent + bS*shade_cent + bWS * water_cent * shade_cent,
    a ~ dnorm(0.5, 0.25),
    bW ~ dnorm(0.65, 0.25), # positive contraint
    bS ~ dnorm(-0.65, 0.25), # negative constraint
    bWS ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ),
  data = d
)

priors <- extract.prior(model.8m4)

par(mfrow=c(1,3))
for (s in -1:1) {
  plot(NA, NA, xlim=c(-1, 1), ylim=c(-2, 2.5), xlab="water", ylab="blooms")
  mu <- link(model.8m4, post = priors, data = data.frame(shade_cent=s, water_cent=-1:1))
  for (i in 1:50) lines(-1:1, mu[i, ], col=col.alpha("grey", 0.3))
}

# 8H1
unique(d$bed)

d <- d %>%
  mutate(
    bed_ind = case_when(
        bed == "a" ~ 1,
        bed == "b" ~ 2,
        bed == "c" ~ 3
      )
  )

model.8h1 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a[bed_ind] + bW[bed_ind]*water_cent + bS[bed_ind]*shade_cent + bWS[bed_ind] * water_cent * shade_cent,
    a[bed_ind] ~ dnorm(0.5, 0.25),
    bW[bed_ind] ~ dnorm(0, 0.25),
    bS[bed_ind] ~ dnorm(0, 0.25),
    bWS[bed_ind] ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ),
  data = d
)

precis(model.8h1, depth = 2)

# 8H2
model.8h2 <- quap(
  alist(
    blooms_std ~ dnorm(mu, sigma),
    mu <- a + bW*water_cent + bS*shade_cent + bWS * water_cent * shade_cent,
    a ~ dnorm(0.5, 0.25),
    bW ~ dnorm(0, 0.25),
    bS ~ dnorm(0, 0.25),
    bWS ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ),
  data = d
)

compare(model.8h1, model.8h2, func = "WAIC")

waic.8h1 <- WAIC(model.8h1, pointwise = TRUE)$penalty
psis.8h1 <- PSIS(model.8h1, pointwise = TRUE)$k
data.frame(pareto.k = psis.8h1, waic.penalty = waic.8h1) %>%
  ggplot(aes(x = pareto.k, y = waic.penalty)) +
  geom_point() +
  geom_vline(xintercept = 0.5)


waic.8h2 <- WAIC(model.8h2, pointwise = TRUE)$penalty
psis.8h2 <- PSIS(model.8h2, pointwise = TRUE)$k
data.frame(pareto.k = psis.8h2, waic.penalty = waic.8h2) %>%
  ggplot(aes(x = pareto.k, y = waic.penalty)) +
  geom_point() +
  geom_vline(xintercept = 0.5)

# 8H3
data("rugged")
d <- rugged

d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000), ]

dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse(dd$cont_africa == 1, 1, 2)

model.8h3 <- quap(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = dd
)

waic.8h3 <- WAIC(model.8h3, pointwise = TRUE)$penalty
psis.8h3 <- PSIS(model.8h3, pointwise = TRUE)$k
data.frame(pareto.k = psis.8h3, waic.penalty = waic.8h3) %>%
  ggplot(aes(x = pareto.k, y = waic.penalty)) +
  geom_point() +
  geom_vline(xintercept = 0.5)

dd$country[which(psis.8h3 > 0.5)]

# dstudent with nu = 2
model.8h3 <- quap(
  alist(
    log_gdp_std ~ dstudent(2, mu, sigma),
    mu <- a[cid] + b[cid]*(rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = dd
)

waic.8h3 <- WAIC(model.8h3, pointwise = TRUE)$penalty
psis.8h3 <- PSIS(model.8h3, pointwise = TRUE)$k
data.frame(pareto.k = psis.8h3, waic.penalty = waic.8h3) %>%
  ggplot(aes(x = pareto.k, y = waic.penalty)) +
  geom_point() +
  geom_vline(xintercept = 0.5)

dd$country[which(psis.8h3 > 0.5)]

# 8H4
# Data Prep
data("nettle")
d <- nettle
head(d)

d$lang.per.cap <- d$num.lang/d$k.pop
d$log.lang.per.cap <- log(d$lang.per.cap)

hist(d$log.lang.per.cap)
hist(d$mean.growing.season)
hist(d$sd.growing.season)

d$L <- standardize(d$log.lang.per.cap)
hist(d$L)

d$M <- standardize(d$mean.growing.season)
hist(d$M)

d$S <- standardize(d$sd.growing.season)
hist(d$S)

d$A <- standardize(log(d$area))
hist(d$A)

# Modeling
# A -> L; M, S -> L; A -> M, S

model.8h4.1 <- quap(
  alist(
    L ~ dstudent(1, mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0, 1.2),
    c(bM, bA) ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = d
)

model.8h4.2 <- quap(
  alist(
    L ~ dstudent(1, mu, sigma),
    mu <- a + bS*S + bA*A,
    a ~ dnorm(0, 1.2),
    bS ~ dnorm(0, 1.5),
    bA ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = d
)

model.8h4.3 <- quap(
  alist(
    L ~ dstudent(2, mu, sigma),
    mu <- a + bM*M + bS*S + bMS*M*S + bA*A,
    a ~ dnorm(0, 1.2),
    c(bM, bA) ~ dnorm(0, 1),
    bS ~ dnorm(0, 1.5),
    bMS ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ),
  data = d
)

plot(compare(model.8h4.1, model.8h4.2, model.8h4.3, func = "WAIC"))
precis(model.8h4.3)

# 8H5
data("Wines2012")
d <- Wines2012
d$std.score <- standardize(d$score)
d$judge.ind <- as.integer(d$judge)
d$wine.ind <- as.integer(d$wine)
head(d)

unique(d$judge)
unique(d$wine)

# Try a bad model
model.8h5.bad <- quap(
  alist(
    std.score ~ dstudent(1, mu, sigma),
    mu <- a[wine.ind] + b[judge.ind],
    a[wine.ind] ~ dnorm(0, 0.5),
    b[judge.ind] ~ dnorm(0, 1.5),
    sigma ~ dexp(1)
  ),
  data = d
)

precis(model.8h5.bad, depth = 2)
prior <- extract.prior(model.8h5.bad)

mu <- link(model.8h5.bad, post = prior)

mu.mean <- apply(mu, 2, mean)
hist(mu.mean)
mu.PI <- apply(mu, 2, PI)

data.frame(
  x = 1:180,
  mu.mean,
  t(mu.PI)
) %>%
  ggplot(aes(x, mu.mean)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(
    ymin = X5.,
    ymax = X94.
  ),
  alpha = 0.2)

post <- extract.samples(model.8h5.bad)
mu <- link(model.8h5.bad, post = post)

mu.mean <- apply(mu, 2, mean)
hist(mu.mean)
mu.PI <- apply(mu, 2, PI)

data.frame(
  x = 1:180,
  mu.mean,
  t(mu.PI)
) %>%
  ggplot(aes(x, mu.mean)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.6) +
  geom_ribbon(aes(
    ymin = X5.,
    ymax = X94.
  ),
  alpha = 0.2)

# Proper model
model.8h5 <- quap(
  alist(
    std.score ~ dnorm(mu, sigma),
    mu <- (Q[wine.ind] + O[wine.ind] - H[judge.ind]) * D[judge.ind],
    Q[wine.ind] ~ dnorm(0, 0.5),
    O[wine.ind] ~ dnorm(0, 0.5),
    H[judge.ind] ~ dnorm(0, 0.5),
    D[judge.ind] ~ dlnorm(0, 0.1),
    sigma ~ dexp(1)
  ),
  data = d
)

prior <- extract.prior(model.8h5)
dat <- expand.grid(wine.ind = 1:20, judge.ind = 1:9)
mu <- link(model.8h5, post = prior, data = dat)

mu.mean <- apply(mu, 2, mean)
hist(mu.mean)
mu.PI <- apply(mu, 2, PI)

data.frame(
  x = 1:180,
  mu.mean,
  t(mu.PI)
) %>%
  ggplot(aes(x, mu.mean)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(
    ymin = X5.,
    ymax = X94.
  ),
  alpha = 0.2)

post <- extract.samples(model.8h5)
mu <- link(model.8h5, post = post, data = dat)
hist(apply(mu, 2, mean))

precis(model.8h5, depth = 2)

judge.wine.means <- matrix(NA, nrow = 20, ncol = 9)
inds <- seq(0, 180, 20)
for (i in 1:9) {
  judge.wine.means[, i] <- apply(mu[, (inds[i]+1):inds[i+1]], 2, mean)
}

judge.wine.means
