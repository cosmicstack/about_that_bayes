library(tidyverse)
library(rethinking)
library(dagitty)

# 7E2
entropy <- function(p) {
  if (sum(p) != 1) {
    stop("Sum of probability vector must be 1")
  }
  valid.idx = which(p!=0)
  if (length(p) == length(valid.idx)) {
    -sum(p*log(p))  
  } else {
    -sum(p[valid.idx]*log(p[valid.idx]))
  }
}

entropy(c(0.7, 0.3))

# 7E3
entropy(c(0.2, 0.25, 0.25, 0.3))

# 7E4
entropy(c(1/3, 1/3, 1/3, 0))

# 7M4
# What happens to the effective number of parameters, as measured by PSIS or
# WAIC, as a prior becomes more concentrated?
data("WaffleDivorce")
d <- WaffleDivorce
d$A <- standardize(d$MedianAgeMarriage)
d$M <- standardize(d$Marriage)
d$D <- standardize(d$Divorce)

model <- quap(
  alist(
    D ~ dstudent(2, mu, sigma),
    mu <- a + bA*A + bM*M,
    a ~ dnorm(0, 0.2),
    c(bA, bM) ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = d
)

precis(model)
sum(PSIS(model, pointwise = TRUE)$penalty)
WAIC(model)

prior_sigma <- 1.1 - seq(0.1, 1, by=0.1)
psis <- 0
waic <- 0

for (i in 1:length(prior_sigma)) {
  s <- prior_sigma[i]
  model <- quap(
    alist(
      D ~ dstudent(2, mu, sigma),
      mu <- a + bA*A + bM*M,
      a ~ dnorm(0, 1),
      c(bA, bM) ~ dnorm(0, s),
      sigma ~ dexp(1)
    ),
    data = d
  )
  psis[i] <- PSIS(model)$penalty
  waic[i] <- WAIC(model)$penalty
}

data.frame(prior_sigma, psis, waic) %>%
  ggplot() +
  geom_line(aes(prior_sigma, psis, color = "PSIS")) +
  geom_line(aes(prior_sigma, waic, color = "WAIC")) +
  scale_color_manual(name = "Metric", values = c("PSIS"="blue", "WAIC"="red")) +
  labs(x = "sigma for bA, bM", y = "Metric Value")

# 7H1
data("Laffer")
d <- Laffer
head(d)
d$rate <- standardize(d$tax_rate)
d$revenue <- standardize(d$tax_revenue)

ggplot(d) +
  geom_point(aes(rate, revenue), alpha = 0.6) +
  theme_bw()

h1.model1 <- quap(
  alist(
    revenue ~ dnorm(mu, exp(log_sigma)),
    mu <- a + b*rate,
    a ~ dnorm(0, 0.5),
    b ~ dnorm(0, 0.5),
    log_sigma ~ dnorm(0, 1)
  ),
  data = d
)
mu <- link(h1.model1)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
h1.model.data <- data.frame(rate=d$rate, revenue=d$revenue, mu.mean, t(mu.PI))
colnames(h1.model.data) <- c("rate", "revenue", "avg", "low.ci", "high.ci")

ggplot(h1.model.data) +
  geom_point(aes(rate, revenue), alpha = 0.5) +
  geom_line(aes(rate, avg)) +
  geom_ribbon(aes(rate, ymin = low.ci, ymax = high.ci), alpha = 0.3)

vars <- NA
for (i in 1:6) {
  vars[i] <- paste("b[", i, "]*rate^", i, sep = "")
}

create_formula <- function(pow) {
  str2lang(paste("mu <- a +", paste(vars[1:pow], collapse = " + "))) 
}

# for (i in 1:6) {
#   model.formula <- create_formula(i)
#   h1.model <- quap(
#     alist(
#       revenue ~ dnorm(mu, exp(log_sigma)),
#       as.formula(model.formula),
#       a ~ dnorm(0, 0.5),
#       b ~ dnorm(0, 0.5),
#       log_sigma ~ dnorm(0, 1)
#     ),
#     data = d,
#     start = list(b=rep(0, i))
#   )
#   mu <- link(h1.model)
#   mu.mean <- apply(mu, 2, mean)
#   mu.PI <- apply(mu, 2, PI)
#   h1.model.data <- data.frame(rate=d$rate, revenue=d$revenue, mu.mean, t(mu.PI))
#   colnames(h1.model.data) <- c("rate", "revenue", "avg", "low.ci", "high.ci")
#   
#   ggplot(h1.model.data) +
#     geom_point(aes(rate, revenue), alpha = 0.5) +
#     geom_line(aes(rate, avg)) +
#     geom_ribbon(aes(rate, ymin = low.ci, ymax = high.ci), alpha = 0.3)
# }

fit_poly <- function(pow) {
  flist <- c(
    # alist(revenue ~ dnorm(mu, exp(log_sigma))),
    alist(revenue ~ dstudent(2, mu, exp(log_sigma))),
    list(create_formula(pow)),                 
    alist(
      a ~ dnorm(0.5, 1),
      b ~ dnorm(0, 100),              
      log_sigma ~ dnorm(0, 1)
    )
  )
  quap(flist, data = d, start = list(b = rep(0, pow)))
}

plot_poly <- function(model, pow) {
  mu_samp <- link(model)
  pred <- data.frame(
    rate    = d$rate,
    revenue = d$revenue,
    avg     = apply(mu_samp, 2, mean),
    t(apply(mu_samp, 2, PI))
  )
  colnames(pred) <- c("rate", "revenue", "avg", "low.ci", "high.ci")
  pred <- pred[order(pred$rate), ]            
  
  ggplot(pred, aes(rate)) +
    geom_ribbon(aes(ymin = low.ci, ymax = high.ci), alpha = 0.3) +
    geom_point(aes(y = revenue), alpha = 0.5) +
    geom_line(aes(y = avg)) +
    labs(title = paste("Degree", pow))
}

models <- lapply(1:6, fit_poly)
plots  <- Map(plot_poly, models, 1:6)

for (p in plots) print(p)

# 7H2

tmp_waic <- WAIC(models[[1]], pointwise = TRUE)$penalty
tmp_psis.k <- PSIS(models[[1]], pointwise = TRUE)$k

outlier.ind <- rep(0, length(tmp_psis.k))
outlier.ind[which(tmp_psis.k > 0.5)] <- 1

ggplot(data.frame(PSIS.pareto.k = tmp_psis.k, WAIC.penalty = tmp_waic, outlier.ind)) +
  geom_point(aes(PSIS.pareto.k, WAIC.penalty, color = factor(outlier.ind)))

# 7H3
island1 <- rep(0.2, 5)
island2 <- c(0.8, 0.1, 0.05, 0.025, 0.025)
island3 <- c(0.05, 0.15, 0.7, 0.05, 0.05)

entropy(island1)
entropy(island2)
entropy(island3)

kl_div <- function(p, q) {
  sum(p*(log(p) - log(q)))
}

kl_mat <- matrix(data=NA, nrow = 3, ncol = 3)
dat <- list(island1, island2, island3)

for (i in 1:3) {
  for (j in 1:3) {
    kl_mat[i, j] <- kl_div(dat[[i]], dat[[j]])
  }
}
kl_mat

# 7H4
d <- sim_happiness(seed=1977, N_years=1000)
d2 <- d[d$age > 17, ]
d2$A <- (d2$age - 18) / (65 - 18)

d2$mid <- d2$married + 1

m6.9 <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a[mid] + bA*A,
    a[mid] ~ dnorm(0, 1),
    bA ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ),
  data = d2
)
precis(m6.9, depth = 2)

m6.10 <- quap(
  alist(
    happiness ~ dnorm(mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 1),
    bA ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ),
  data = d2
)
precis(m6.10)

compare(m6.9, m6.10, func = "PSIS")

# 7H5
set.seed(37)
data("foxes")
d <- foxes
head(d)

dag.6h3 <- dagitty("dag{
  A -> F -> G -> W
  A -> F -> W
}")
coordinates(dag.6h3) <- list(
  x = c(G = 1, A = 0, F = 1, W = 2),
  y = c(G = 0, A = 1, F = 1, W = 1)
)
drawdag(dag.6h3)

d$A <- standardize(d$area)
d$F <- standardize(d$avgfood)
d$G <- standardize(d$groupsize)
d$W <- standardize(d$weight)

h5.m1 <- quap(
  alist(
    W ~ dstudent(4, mu, sigma),
    mu <- a + bF*F + bG*G + bA*A,
    a ~ dnorm(0, 0.2),
    c(bF, bG, bA) ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = d
)

h5.m2 <- quap(
  alist(
    W ~ dstudent(3, mu, sigma),
    mu <- a + bF*F + bG*G,
    a ~ dnorm(0, 0.2),
    c(bF, bG) ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = d
)

h5.m3 <- quap(
  alist(
    W ~ dstudent(3, mu, sigma),
    mu <- a + bG*G + bA*A,
    a ~ dnorm(0, 0.2),
    c(bG, bA) ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = d
)

h5.m4 <- quap(
  alist(
    W ~ dstudent(2, mu, sigma),
    mu <- a + bF*F,
    a ~ dnorm(0, 0.2),
    c(bF) ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = d
)

h5.m5 <- quap(
  alist(
    W ~ dstudent(4, mu, sigma),
    mu <- a + bA*A,
    a ~ dnorm(0, 0.2),
    c(bA) ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = d
)

plot(compare(h5.m1, h5.m2, h5.m3, h5.m4, h5.m5, func = "WAIC"))
