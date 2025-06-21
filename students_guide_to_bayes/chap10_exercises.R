#Prob set 10
library(tidyverse)
library(ggridges)
library(ggforce)

#Poisson likelihood

#Prior
df <- data.frame(x = seq(0,20,0.1)) %>%
  mutate(prior = map(x, function (x) {dgamma(x, 3, 0.5)}))

ggplot(df, aes(x, as.numeric(prior))) +
  geom_line()

outbreaks <- c(3, 7, 4, 10, 11)

alpha <- 3 + sum(outbreaks)
beta <- 0.5 + length(outbreaks)

df <- df %>%
  mutate(posterior = map(x, function(x) dgamma(x, alpha, beta)))

ggplot(df, aes(x, as.numeric(posterior))) +
  geom_line()

sample_ppd <- function(alpha, beta) {
  sample <- 0
  for (i in 1:10000) {
    l <- rgamma(1, alpha, beta)
    sample[i] <- rpois(1, lambda = l)
  }
  sample
}

sample <- sample_ppd(alpha, beta)
hist(sample)

#10.2
sleep <- read.csv("/Users/krishanth/Code/r/students_guide_to_bayes/All_data/evaluation_sleepstudy.csv")
model <- lm(Reaction ~ Days, data = sleep)
summary(model)

ggplot(sleep, aes(Days, Reaction)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(sleep, aes(x = fct_reorder(factor(Subject), Reaction, .fun = median), y = Reaction)) +
  geom_boxplot()

ggplot(sleep, aes(Days, Reaction)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, size = 0.3) +
  facet_wrap(vars(Subject), nrow = 6)
  
sleepPosterior <- read.csv("/Users/krishanth/Code/r/students_guide_to_bayes/All_data/evaluation_sleepPosteriors.csv")
glimpse(sleepPosterior)

genData <- function(alpha, beta, sigma, t) {
  rnorm(1, mean = (alpha + beta * t), sd = sigma)
}

genData(1, 1, 1)

data.frame(day = seq(0,9,1), val = NA_real_) %>%
  pivot_wider(names_from = "day", values_from = "val",names_prefix = "day_")

sleepPosterior <- sleepPosterior %>%
  mutate(
    day0 = pmap(list(alpha, beta, sigma, 0), genData),
    day1 = pmap(list(alpha, beta, sigma, 1), genData),
    day2 = pmap(list(alpha, beta, sigma, 2), genData),
    day3 = pmap(list(alpha, beta, sigma, 3), genData),
    day4 = pmap(list(alpha, beta, sigma, 4), genData),
    day5 = pmap(list(alpha, beta, sigma, 5), genData),
    day6 = pmap(list(alpha, beta, sigma, 6), genData),
    day7 = pmap(list(alpha, beta, sigma, 7), genData),
    day8 = pmap(list(alpha, beta, sigma, 8), genData),
    day9 = pmap(list(alpha, beta, sigma, 9), genData)
  )

sleepPosterior %>%
  select(starts_with("day")) %>%
  pivot_longer(cols = starts_with("day"), names_to = "Day", values_to = "Reaction") %>%
  unnest(Reaction) %>%
  ggplot(aes(Day, Reaction)) +
    geom_violin() +
    geom_sina(size = 0.1)

#10.3
discoveries <- read.csv("/Users/krishanth/Code/r/students_guide_to_bayes/All_data/evaluation_discoveries.csv")
glimpse(discoveries)

ggplot(discoveries, aes(time, discoveries)) +
  geom_col()

alpha <- 8
beta <- 1.2

p_discoveries <- data.frame(x = seq(0, 12, 0.1)) %>%
  mutate(prior = map(x, function(x) {dgamma(x, alpha, beta)}))

ggplot(p_discoveries, aes(x, as.numeric(prior))) +
  geom_line()

pos_alpha <- alpha + sum(discoveries$discoveries)
pos_beta <- beta + length(discoveries$time)

p_discoveries <- p_discoveries %>%
  mutate(
    posterior = map(x, function(x) {dgamma(x, pos_alpha, pos_beta)})
  )

ggplot(p_discoveries, aes(x)) +
  geom_line(aes(y = as.numeric(prior))) +
  geom_line(aes(y = as.numeric(posterior)))

ppc <- data.frame(x1 = matrix(nrow = 100, ncol = 1))
for (j in 1:20) {
  x <- 0
  for (i in 1:100) {
    l <- rgamma(1, pos_alpha, pos_beta)
    x[i] <- rpois(1, lambda = l)
  }
  ppc[, ncol(ppc)+1] <- x 
}

ppc <- ppc %>%
  select(starts_with("V")) %>%
  cbind(V1 = discoveries$discoveries) %>%
  mutate(t = 1:100)

bayesp <- ppc %>%
  pivot_longer(cols = starts_with("V"), names_to = "sample", values_to = "ppc") %>%
  select(sample, ppc) %>%
  group_by(sample) %>%
  summarize(max = max(ppc)) %>%
  arrange(sample) %>%
  mutate(bayes = ifelse(max >= 12, TRUE, FALSE))

ppc %>%
  pivot_longer(cols = starts_with("V"), names_to = "sample", values_to = "ppc") %>%
  left_join(bayesp) %>%
  ggplot(aes(t, ppc)) +
  geom_col(aes(fill = bayes)) +
  facet_wrap(vars(sample), nrow = 7)

#10.4
X <- c(2, 7, 4, 5, 4, 5, 6, 4, 4, 4, 5, 6, 5, 7, 6, 2, 4, 6, 6, 6)



df <- data.frame(X = seq(1, 10, 1)) %>%
  mutate(
    prior11 = map(X, function (x) {dbeta(x, 1, 1)}),
    prior21 = map(X, function (x) {dbeta(x, 2, 1)}),
    prior31 = map(X, function (x) {dbeta(x, 3, 1)}),
    prior41 = map(X, function (x) {dbeta(x, 4, 1)}),
    prior51 = map(X, function (x) {dbeta(x, 5, 1)}),
    prior61 = map(X, function (x) {dbeta(x, 6, 1)}),
    prior71 = map(X, function (x) {dbeta(x, 7, 1)}),
    prior81 = map(X, function (x) {dbeta(x, 8, 1)}),
    prior91 = map(X, function (x) {dbeta(x, 9, 1)}),
    prior101 = map(X, function (x) {dbeta(x, 10, 1)})
  )