setwd("/Users/krishanth/Code/r/students_guide_to_bayes")
library(tidyverse)

# 13.1
# 1
fPriorPredictive <- function(NumSamples, a, b, N) {
  draws <- 0
  for (i in 1:NumSamples) {
    theta <- rbeta(1, a, b)
    draws[i] <- rbinom(1, N, theta)
  }
  draws
}

X <- fPriorPredictive(1000, 1, 1, 100)
hist(X)
mean(X)

# 2
posterior_samples <- rbeta(10000, 7, 95)
posterior_dist <- dbeta(posterior_samples, 7, 95)

ggplot(data.frame(x = 1:10000, y = posterior_samples)) +
  geom_density(aes(y)) +
  lims(x = c(0, 1))

mean(posterior_samples)

# 3
p <- seq(0, 1, length = 100)
Adist <- dbeta(p, 7, 95)

BSamples <- rbeta(100, 7, 95)
Bdist <- dbeta(BSamples, 7, 95)

ggplot() +
  geom_line(data = data.frame(x = p, y = Adist), aes(x, y), color = "red", alpha = 0.5) +
  geom_line(data = data.frame(x = BSamples, y = Bdist), aes(x, y), color = "blue", alpha = 0.5)


# 4
true_mean <- 7/102
sample_mean <-  matrix(NA, nrow = 100, ncol = 1000)

for (i in 1:1000) {
  for (j in 1:100) {
    X <- rbeta(i, 7, 95)
    sample_mean[j, i] <- mean(X)
  }
}

data.frame(sample_mean) %>%
  pivot_longer(cols=starts_with("X"), names_to = "sample_size", names_prefix = "X", values_to = "mean") %>%
  ggplot() +
  geom_boxplot(aes(sample_size, mean), fill = "white", outliers = FALSE) +
  scale_x_discrete(breaks = seq(1, 1000, 100), labels = seq(1, 1000, 100))

errorEst <- abs(sample_mean - true_mean)

ggplot(data.frame(x = 1:1000, y = apply(errorEst, 2, sd))) +
  geom_point(aes(x,y)) +
  geom_smooth(aes(x, y))

# 5
tru_var <- 7*95/((7+95)^2*(7+95+1))

sampled_var <- matrix(NA, nrow = 100, ncol = 1000)

for (i in 1:1000) {
    X <- rbeta(100, 7, 95)
    sampled_var[, i] <- X
}

vars <- apply(sampled_var, 2, var)

ggplot(data.frame(variance = vars)) +
  geom_histogram(aes(variance)) +
  geom_vline(xintercept = tru_var)
