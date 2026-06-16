library(tidyverse)
library(rethinking)

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

