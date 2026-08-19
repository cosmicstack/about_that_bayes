library(tidyverse)
library(rethinking)

data("chimpanzees")
d <- chimpanzees
head(d)

d$treatment <- 1 + d$prosoc_left + 2*d$condition

xtabs(~ treatment + prosoc_left + condition, d)

dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  treatment = as.integer(d$treatment)
)

m.test <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    # a ~ dnorm(0, 10)
    a ~ dnorm(0, 1.5)
  ),
  data = dat_list,
  cores = 4,
  chains = 4
)

traceplot(m.test)

prior <- extract.prior(m.test, n=1e4)
p <- inv_logit(prior$a)
dens(p, adj = 0.1, show.HPDI = T)

m.test2 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + b[treatment],
    a ~ dnorm(0, 1.5),
    # b[treatment] ~ dnorm(0, 10)
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = dat_list,
  cores = 4,
  chains = 4
)

traceplot(m.test2)

prior <- extract.prior(m.test2, n = 1e4)
p <- sapply(1:4, function(k) inv_logit(prior$a + prior$b[, k]))
dens(abs(p[, 3] - p[, 4]), adj = 0.1)


m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = dat_list,
  cores = 4,
  chains = 4,
  log_lik = TRUE
)

precis(m11.4, depth = 2)
traceplot(m11.4)

d <- chimpanzees
d$treatment <- 1 + d$prosoc_left + 2*d$condition
d$side <- d$prosoc_left + 1
d$cond <- d$condition + 1

d_aggregated <- aggregate(
  d$pulled_left,
  list(
    treatment = d$treatment,
    actor = d$actor,
    side = d$side,
    cond = d$cond
  ),
  sum
)
colnames(d_aggregated)[5] <- "left_pulls"

dat_list <- with(d_aggregated, list(
  left_pulls = left_pulls,
  treatment = treatment,
  actor = actor,
  side = side,
  cond = cond
))

m11.6 <- ulam(
  alist(
    left_pulls ~ dbinom(18, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ),
  data = dat_list,
  cores = 4,
  chains = 4,
  log_lik = TRUE
)

traceplot(m11.6)

plot(precis(m11.6, depth = 2))

data("UCBadmit")
d <- UCBadmit
head(d)

dat_list <- list(
  admit = d$admit,
  applications = d$applications,
  gid = ifelse(d$applicant.gender=="male", 1, 2)
)

m11.7 <- ulam(
  alist(
    admit ~ dbinom(applications, p),
    logit(p) <- a[gid],
    a[gid] ~ dnorm(0, 1.5)
  ),
  data = dat_list,
  chains = 4,
  log_lik = TRUE
)

precis(m11.7, depth = 2)
post <- extract.samples(m11.7)
diff.a <- post$a[, 1] - post$a[, 2]
diff.p <- inv_logit(post$a[, 1]) - inv_logit(post$a[, 2])
diff.p1 <- inv_logit(post$a[, 1] - post$a[, 2])
diff.p2 <- post$p[, 1] - post$p[, 2]

precis(list(diff.a=diff.a, diff.p=diff.p, diff.p1=diff.p1, diff.p2=diff.p2))

postcheck(m11.7)
