library(rethinking)
library(tidyverse)

data("rugged")
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$log_gdp), ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse(dd$cont_africa == 1, 1, 2)

dat_slim <- list(
  log_gdp_std = dd$log_gdp_std,
  rugged_std = dd$rugged_std,
  cid = as.integer(dd$cid)
)

## EXAMPLE ##
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = dat_slim,
  chains = 4,
  cores = 4,
  warmup = 100,
  iter = 1000
)

precis(m9.1, depth = 2)

prior <- extract.prior(m9.1)
mu <- link(m9.1, post = prior)
## ======= ##

# 9M1
m.9m1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dunif(0, 1)
  ),
  data = dat_slim,
  chains = 4,
  cores = 4
)

precis(m.9m1, depth = 2)

# 9M2
m.9m2 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dexp(0.3),
    sigma ~ dexp(1)
  ),
  data = dat_slim,
  chains = 4,
  cores = 4
)

precis(m.9m2, depth = 2)

plot(coeftab(m.9m1, m.9m2), pars=c("b[1]", "b[2]"))

# 9M3
warmup <- seq(100, 1000, 100)
n.eff.mat <- matrix(NA, nrow = 5, ncol = 10)

for (i in 1:10) {
  m.9m3 <- ulam(
    alist(
      log_gdp_std ~ dnorm(mu, sigma),
      mu <- a[cid] + b[cid] * (rugged_std - 0.215),
      a[cid] ~ dnorm(1, 0.1),
      b[cid] ~ dnorm(0, 0.3),
      sigma ~ dexp(1)
    ),
    data = dat_slim,
    chains = 4,
    cores = 4,
    warmup = warmup[i],
    iter = 2000
  )
  
  n.eff.mat[, i] <- precis(m.9m3, depth = 2)$ess_bulk
}

data.frame(parameters = c("a[1]", "a[2]", "b[1]", "b[2]", "sigma"), n.eff.mat) %>%
  pivot_longer(cols = starts_with("X"), names_to = "warmup.iters", names_prefix = "X", values_to = "n.eff") %>%
  mutate(
    warmup.iters = as.integer(warmup.iters)*100
  ) %>%
  ggplot(aes(warmup.iters, n.eff, color = parameters)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(100, 1000, 100))

# 9H1

mp <- ulam(
  alist(
    a ~ dnorm(0, 1),
    b ~ dcauchy(0, 1)
  ),
  data = list(y = 1),
  chains = 1
)

precis(mp)
traceplot(mp)

# 9H1
data("WaffleDivorce")
d <- WaffleDivorce
head(d)
d$A <- standardize(d$MedianAgeMarriage)
d$M <- standardize(d$Marriage)
d$D <- standardize(d$Divorce)

dat.divorce <- list(
  A = d$A,
  M = d$M,
  D = d$D
)

m5.1 <- ulam(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0 , 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dat.divorce,
  chains = 1
)

m5.2 <- ulam(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM * M,
    a ~ dnorm(0 , 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dat.divorce,
  chains = 1
)

m5.3 <- ulam(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A + bM * M,
    a ~ dnorm(0 , 0.2),
    bA ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = dat.divorce,
  chains = 1
)

# Checked all three models with 1 chain first, and then 4 chains to check individual
# chains are converging, then reverted to 1 chain for actual sampling

compare(m5.1, m5.2, m5.3, func = WAIC, log_lik = TRUE)

# 9H3
N <- 100
set.seed(909)
height <- rnorm(N, 10, 2)
leg_prop <- runif(N, 0.4, 0.5)
leg_left <- leg_prop * height + rnorm(N, 0, 0.02)
leg_right <- leg_prop * height + rnorm(N, 0, 0.02)
d <- data.frame(height, leg_left, leg_right)

m6.1 <- ulam(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10, 100),
    c(bl, br) ~ dnorm(2, 10),
    sigma ~ dexp(1)
  ),
  data = d,
  chains = 4,
  cores = 4,
  start = list(
    a = 10,
    bl = 0,
    br = 0.1,
    sigma = 1
  )
)
precis(m6.1)
traceplot(m6.1)
trankplot(m6.1)

m6.2 <- ulam(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10, 100),
    c(bl, br) ~ dnorm(2, 10),
    sigma ~ dexp(1)
  ),
  data = d,
  chains = 4,
  cores = 4,
  constraints = list(br="lower=0"),
  start = list(
    a = 10,
    bl = 0,
    br = 0.1,
    sigma = 1
  )
)

precis(m6.2)
traceplot(m6.2)
trankplot(m6.2)

plot(coeftab(m6.1, m6.2))
plot(coeftab(m6.1, m6.2), pars=c("br", "bl"))

m6.1_samples <- extract.samples(m6.1, pars=c("br", "bl"))
m6.2_samples <- extract.samples(m6.2, pars=c("br", "bl"))

data.frame(m6.1_samples) %>%
  select(br, bl) %>%
  pivot_longer(cols = c(br, bl), names_to = "param", values_to = "value") %>%
  ggplot(aes(x = value, fill = param)) +
  geom_density(alpha = 0.4) +
  labs(title = "Unconstrained")

data.frame(m6.2_samples) %>%
  select(br, bl) %>%
  pivot_longer(cols = c(br, bl), names_to = "param", values_to = "value") %>%
  ggplot(aes(x = value, fill = param)) +
  geom_density(alpha = 0.4) +
  labs(title = "Constrained")

# Simpler method: learn
bind_rows(
  Unconstrained = as.data.frame(extract.samples(m6.1, pars = c("bl", "br"))),
  Constrained   = as.data.frame(extract.samples(m6.2, pars = c("bl", "br"))),
  .id = "model"
) %>%
  pivot_longer(c(bl, br), names_to = "param") %>%
  ggplot(aes(value, fill = param)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ model)

# 9H4
WAIC(m6.1, log_lik = TRUE)
WAIC(m6.2, log_lik = TRUE)

# 9H5
weeks <- 1e4
positions <- 0
population <- as.integer(runif(10, min = 1e3, max = 1e4))
current <- sample(seq(1, 10), 1)

for (i in 1:weeks) {
  positions[i] <- current
  proposal.pos <- current + sample(c(-1,1), size = 1)
  
  proposal.pos <- ifelse(proposal.pos > 10, 1, proposal.pos)
  proposal.pos <- ifelse(proposal.pos < 1, 10, proposal.pos)
  
  prob_move <- population[proposal.pos]/population[current]
  current <- ifelse(runif(1) < prob_move, proposal.pos, current)
}

ggplot(data = data.frame(x=1:10, y=population), aes(x, y)) +
  geom_col()

data.frame(positions) %>%
  group_by(positions) %>%
  summarize(n = n()) %>%
  ggplot(aes(positions, n)) +
  geom_col()

data.frame(x = 1:length(positions), y = positions) %>%
  ggplot(aes(x, y)) +
  geom_line(alpha=0.5)

# 9H6
binom_ll <- function(p, n=9, k=6) {
  log.lik <- log(factorial(n) / (factorial(n-k) * factorial (k))) + k * log(p) + (n-k) * log(1 - p)
  log.lik
}

data.frame(p = p.grid) %>%
  mutate(
    log.l = map_dbl(p, binom_ll)
  ) %>%
  ggplot() +
  geom_line(aes(p, log.l))

tosses <- 1e4
positions <- 0
current <- sample(seq(0, 1, 0.01), 1)

for (i in 1:tosses) {
  positions[i] <- current
  proposal.pos <- current + sample(c(-0.01,0.01), size = 1)
  
  proposal.pos <- ifelse(proposal.pos >= 1, 0.99, proposal.pos)
  proposal.pos <- ifelse(proposal.pos <= 0, 0.01, proposal.pos)
  
  prob_move <- binom_ll(proposal.pos) / binom_ll(current)
  current <- ifelse(runif(1) < prob_move, proposal.pos, current)
}

hist(positions)

data.frame(x = 1:length(positions), y = positions) %>%
  ggplot(aes(x, y)) +
  geom_line(alpha=0.5)

# 9H7
binom_ll <- function(p, n=9, k=6) {
  p <- ifelse(p == 0, 0.01, p)
  p <- ifelse(p == 1, 0.99, p)
  
  log.lik <- log(factorial(n) / (factorial(n-k) * factorial (k))) + k * log(p) + (n-k) * log(1 - p)
  -log.lik
}

grad <- function(p, n=9, k=6) {
  del <- (k/p) - ((n-k)/(1-p))
  -del
}

q.vals <- 0
current.q <- sample(seq(0, 1, 0.01), 1)
q.vals[1] <- current.q
for (i in 2:tosses) {
  result <- HMC2(binom_ll, grad, 0.03, 11, current.q)
  q.new <- result$q
  q.vals[i] <- q.new
  current.q <- q.new
}

