library(tidyverse)

p <- seq(0,1,0.01)
lh <- data.frame(p = p)
lh <- lh %>%
  mutate(
    lik1 = map(p, function (x) {dbinom(1, 10, prob = x)})
  )

ggplot(lh, aes(p, as.numeric(lik1))) +
  geom_line() +
  geom_point()

integrate(function (x) {dbinom(1, 10, prob = x)}, 0, 1)

x <- seq(0,10,1)

data.frame(x) %>%
  mutate(pdf = map(x, function(x) {dbinom(x, 10, 0.1)})) %>%
  ggplot() +
  geom_point(aes(x, as.numeric(pdf)))

lh <- lh %>%
  mutate(
    beta_prior = map(p, function (x) {dbeta(x,1,1)}),
    beta_posterior = map(p, function (x) {dbeta(x, 1, 10)})
  )

ggplot(lh) +
  geom_line(aes(p, as.numeric(beta_prior), color = "Prior")) +
  geom_line(aes(p, as.numeric(beta_posterior), color = "Posterior")) +
  geom_line(aes(p, as.numeric(lik1), color = "Likelihood"))

new_data7 <- data.frame(p) %>%
  mutate(
    lik1 = map(p, function (x) {dbinom(7, 100, prob = x)}),
    beta_prior = map(p, function (x) {dbeta(x,1,1)}),
    beta_posterior = map(p, function (x) {dbeta(x, 7, 94)})
  )

ggplot(new_data7) +
  geom_line(aes(p, as.numeric(beta_prior), color = "Prior")) +
  geom_line(aes(p, as.numeric(beta_posterior), color = "Posterior")) +
  geom_line(aes(p, as.numeric(lik1), color = "Likelihood"))

new_data4 <- data.frame(p) %>%
  mutate(
    lik1 = map(p, function (x) {dbinom(4, 100, prob = x)}),
    beta_prior = map(p, function (x) {dbeta(x,1,1)}),
    beta_posterior = map(p, function (x) {dbeta(x, 4, 97)})
  )

ggplot(new_data7) +
  geom_line(aes(p, as.numeric(beta_posterior), color = "Posterior w/ 7")) +
  geom_line(data = new_data4, aes(p, as.numeric(beta_posterior), color = "Posterior w/ 4"), inherit.aes = F)

sample_posterior <- function(a, b) {
  theta <- rbeta(1, a, b)
  data_x <- rbinom(1, 100, prob = theta)
  data_x
}

sample <- 0
for (i in 1:10000) {
  sample[i] <- sample_posterior(12, 190)
}

hist(sample)

epil <- read.csv("/Users/krishanth/Code/r/students_guide_to_bayes/All_data/conjugate_epil.csv")
alpha <- 4
beta <- 0.25

alpha_n <- alpha + sum(epil$x)
beta_n <- beta + length(epil$x)

data.frame(theta = seq(1, 20, 0.01)) %>%
  mutate(posterior = map(theta, function(x) {dgamma(x, shape = alpha_n, rate = beta_n)})) %>%
  ggplot() +
  geom_line(aes(theta, as.numeric(posterior)))

#Always remember the posterior is a probability distribution

#Posterior Predictive Distribution

alpha_pred <- alpha_n + sum(epil$x)
beta_pred <- beta_n + length(epil$x)

data.frame(theta = seq(1, 20, 0.01)) %>%
  mutate(posterior = map(theta, function(x) {dgamma(x, shape = alpha_pred, rate = beta_pred)})) %>%
  ggplot() +
  geom_line(aes(theta, as.numeric(posterior)))

epil_posterios <- function(a, b) {
  theta <- rgamma(1, shape = a, rate = b)
  prob <- rpois(1, theta)
}

sample <- 0
for (i in 1:10000) {
  sample[i] <- epil_posterios(alpha_n, beta_n)
}

sample %>%
  data.frame(sample) %>%
  ggplot() +
  geom_histogram(aes(sample, y = ..density..), bins = 20, center = 0.5, color = "white")

light <- read.csv("/Users/krishanth/Code/r/students_guide_to_bayes/All_data/conjugate_newcomb.csv")
