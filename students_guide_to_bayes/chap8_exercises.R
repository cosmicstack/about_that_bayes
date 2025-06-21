library(tidyverse)
library(scatterplot3d)

theta <- 0.8

alpha <- seq(0.05, 1, 0.05)
beta <- alpha * (1 - theta)/theta

beta_var <- function(a, b) {
  a * b /((a +b)^2 * (a + b + 1))
}

df <- data.frame(alpha, beta) %>%
  mutate(varb = map2(alpha, beta, beta_var))

scatterplot3d(df$alpha, df$beta, df$varb)
plot(df$alpha, df$varb)
plot(df$beta, df$varb)


# 8.2
#Graph the likelihood in p_a, p_b space

lik <- function(pa, pb) {
  (factorial(10)*pa*pb*(1-pa-pb))/(factorial(6)*factorial(3)*factorial(1))
}

rand.sum <- function(n){
  x <- sort(runif(n-1))
  c(x,1) - c(0,x)
}

df <- data.frame(t(replicate(100,rand.sum(3))))
colnames(df) <- c("pa", "pb", "pc")
df <- df %>%
  mutate(likh = map2(pa, pb, lik))

ggplot(df, aes(pa, pb, z = as.numeric(likh))) +
  geom_contour()