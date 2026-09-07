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


# ============================================================================ #

data("Kline")
d <- Kline
head(d)

d$P <- scale(log(d$population))
d$contact_id <- ifelse(d$contact == "high", 2, 1)

dat <- list(
  T = d$total_tools,
  P = d$P,
  cid = d$contact_id
)

m11.9 <- ulam(
  alist(
    T ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(3, 0.5)
  ),
  data = dat,
  chains = 4,
  log_lik = TRUE
)
traceplot(m11.9)

m11.10 <- ulam(
  alist(
    T ~ dpois(lambda),
    log(lambda) <- a[cid] + b[cid]*P,
    a[cid] ~ dnorm(3, 0.5),
    b[cid] ~ dnorm(0, 0.2)
  ),
  data = dat,
  chains = 4,
  log_lik = TRUE
)
traceplot(m11.10)

compare(m11.9, m11.10, func = PSIS)

dat2 <- list(
  T = d$total_tools,
  P = d$population,
  cid = d$contact_id
)

m11.11 <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- exp(a[cid]) * P^b[cid]/g,
    a[cid] ~ dnorm(1, 1),
    b[cid] ~ dexp(1),
    g ~ dexp(1)
  ),
  data = dat2,
  chains = 8,
  cores = 4,
  log_lik = TRUE
)
traceplot(m11.11)

precis(m11.11, depth = 2)

num_days <- 30
y <- rpois(num_days, 1.5)
num_weeks <- 4
y_new <- rpois(num_weeks, 0.5*7)
y_all <- c(y, y_new)
exposure <- c(rep(1, 30), rep(7, 4))
monastery <- c(rep(0, 30), rep(1, 4))
d <- data.frame(
  y = y_all,
  days = exposure,
  monastery = monastery
)

head(d)

d$log_days <- log(d$days)

m11.12 <- ulam(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- log_days + a + b*monastery,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1)
  ),
  data = list(y = d$y, log_days = d$log_days, monastery = d$monastery),
  cores = 4,
  chains = 4,
  log_lik = TRUE,
  iter = 8000
)
traceplot(m11.12)
precis(m11.12, depth = 2)

post <- extract.samples(m11.12)
lambda.1 <- exp(post$a)
lambda.2 <- exp(post$a + post$b)

precis(data.frame(lambda.1, lambda.2))
