setwd("/Users/krishanth/Code/r/students_guide_to_bayes")
library(tidyverse)

fairground_game <- function() {
  entry <- rbinom(1, 1, 0.5)
  if (entry == 1) {
    H <- rbinom(1, 2, 0.5)
    if (H == 2) {
      2
    } else if (H == 1) {
      1
    } else {
      0
    }
  } else {
    0
  }
}

# Checking Expected Value of winnings when there is no entry fee
avg_winning <- 0
for (i in 1:100) {
  w <- 0
  for (j in 1:10000) {
    w[j] <- fairground_game()
  }
  avg_winning[i] <- mean(w)
}

hist(avg_winning)

game_count <- function(balance, entry) {
  games <- 0
  while (balance > 0) {
    winnings <- fairground_game() - entry
    balance <- balance + winnings
    games <- games + 1
  }
  games
}

game_count(10, 1)

games_played <- 0
for (i in 1:10000) {
  games_played[i] <- game_count(10, 1)
}

hist(games_played)
mean(games_played)

q12p4 <- function(balance, entry, games) {
  numGames <- 0
  for (i in 1:games) {
    if (balance >= entry) {
      winnings <- fairground_game() - entry
      balance <- balance + winnings
      numGames <- numGames + 1
    } else {
      break
    } 
  }
  numGames
}

q12p4(10, 0.49, 100)

bals <- 0
for (i in 1:10000) {
  bals[i] <- q12p4(10, 0.49, 100)
}

mean(bals < 100)

################################################################################

integrand <- function(x) {
  y <- ifelse(
    x < 0.9735,
    (1/1.33485)*(1/sqrt(2*pi))*exp(-x^2/2),
    ifelse(
      x <= 5,
      0.186056,
      0
    )
  )
  return(y)
}

x <- seq(0, 5, 0.01)
y <- integrand(x)
plot(x, y)

tmp <- integrate(integrand, lower = 0, upper = 5)
tmp$value

upper <- max(y)

independent_sampling <- function(n) {
  sample_x <- 0
  sample_y <- 0
  accept <- 0
  for (i in 1:n) {
    sample_x[i] <- runif(1, 0, 5)
    y <- runif(1, 0, upper)
    sample_y[i] <- integrand(sample_x[i])
    accept[i] <- ifelse(y < sample_y[i], 1, 0)
  }
  return(data.frame(x = sample_x, y = sample_y, accept = accept))
}

samples <- independent_sampling(10000)

ggplot(samples, aes(x, fill = factor(accept))) +
  geom_histogram(position = "stack", bins = 30, center = 1.8)

print(mean(samples$x[samples$accept == 1], 100))
print(var(samples$x[samples$accept == 1]))

mean_dist <- 0
for (j in 1:1000) {
  df <- independent_sampling(10000)
  mean_dist[j] <- mean(df$x[df$accept == 1], 100)
}

hist(mean_dist)
mean(mean_dist)
var(mean_dist)

a_mean <- integrate(function (x) x*integrand(x), 0, 5)$value
a_var <- integrate(function (x) x^2*integrand(x), 0, 5)$value - a_mean^2

g_method <- function(n) {
  g_mean <- 0
  for (k in 1:n) {
    fgx <- runif(1, 0, 5)
    ffx <- integrand(fgx)
    g_mean[k] <- fgx*ffx*5
  }
  return(mean(g_mean))
}

g_method(10000)

################################################################################

x <- rnorm(500000)
f <- x^3
mean(f)/2

################################################################################
#12.4

markov_coin <- function(n) {
  sampling <- 0
  for (i in 1:10000) {
    x <- rbernoulli(n)
    sampling[i] <- mean(x)
  }
  hist(sampling)
}

markov_coin(1000)

mc_sim <- function(n, epsilon) {
  P <- matrix(c(0.5 + epsilon, 0.5 + epsilon, 0.5 - epsilon, 0.5 - epsilon), nrow = 2, ncol = 2)
  rownames(P) <- c("H", "T")
  colnames(P) <- c("H", "T")
  states_vector <- vector(mode = "character", length = n)
  state <- ifelse(rbernoulli(1) == 1, "H", "T")
  states_vector[1] <- state
  for (i in 2:n) {
    state <- ifelse(which(rmultinom(1, 1, P[state, ]) == 1) == 1, "H", "T")
    states_vector[i] <- state
  }
  states_vector <- ifelse(states_vector == "H", 1, 0)
  return(states_vector)
}

epsilon <- seq(0, 0.5, 0.05)

ep_val <- 0
n_val <- 0
mean_val <- 0

for (i in 1:length(epsilon)) {
  for (j in 1:1000) {
    ep_val[1000*(i-1)+j] <- epsilon[i]
    n_val[1000*(i-1)+j] <- 100
    mean_val[1000*(i-1)+j] <- mean(mc_sim(100, epsilon[i]))
  }
}

df <- data.frame(epsilon = ep_val, n = n_val, mean = mean_val) %>%
  mutate(error = abs(0.5 - mean))

ggplot(df, aes(epsilon, mean, color = factor(epsilon))) +
  geom_boxplot()

df %>%
  filter(epsilon == 0.25) %>%
  select(mean) %>%
  ggplot(aes(mean)) +
  geom_histogram(bins = 30, fill = "white", color = "black")

df %>%
  select(epsilon, error) %>%
  group_by(epsilon) %>%
  summarise(avg_error = mean(error)) %>%
  ggplot(aes(epsilon, avg_error)) +
  geom_line() +
  geom_point()

s <- 0
indep <- 0
dep <- 0
for (i in 1:100) {
  for (j in 1:1000) {
    indep[1000*(i-1)+j] <- mean(mc_sim(i, 0))
    dep[1000*(i-1)+j] <- mean(mc_sim(i, 9/20))
    s[1000*(i-1)+j] <- i
  }
}

df2 <- data.frame(s = s, indep, dep) %>%
  group_by(s) %>%
  summarize(avg_dep_error = sd(dep), avg_indep_error = sd(indep)) %>%
  mutate(
    avg_dep_error = avg_dep_error/sqrt(i),
    avg_indep_error = avg_indep_error/sqrt(i)
  ) %>%
  pivot_longer(cols = c(avg_indep_error, avg_dep_error), names_to = "dependency", values_to = "error")

ggplot(df2, aes(s, error, color = dependency)) +
  geom_line() +
  geom_point()

indep <- 0
dep <- 0
for (i in 1:100) {
  ind_mean <- mean(mc_sim(i, 0))
  indep[i] <- sqrt((ind_mean*(1-ind_mean))/i)
  dep_mean <- mean(mc_sim(i, 9/20))
  dep[i] <- sqrt((dep_mean*(1-dep_mean))/i)
}

df2 <- data.frame(s = 1:100, indep, dep) %>%
  pivot_longer(cols = c(indep, dep), names_to = "dependency", values_to = "error")

ggplot(df2, aes(s, error, color = dependency)) +
  geom_line() +
  geom_point()

################################################################################
#12.6.1

coin_prng <- function(n) {
  X <- 0
  for (i in 1:n) {
    toss <- rbernoulli(2)
    if (toss[1] == FALSE & toss[2] == FALSE) {
      X[i] <- NA
    } else {
      if (toss[1] == TRUE & toss[2] == TRUE) {
        X[i] <- 1
      } else {
        X[i] <- 0
      }
    }
  }
  X[!is.na(X)]
}

Y <- coin_prng(10000)
hist(Y)

#12.6.2

coin_std_normal <- function(n) {
  x <- 0
  for (i in 1:n) {
      x[i] <- (sqrt(1000) * (mean(rbinom(1000, 1, 0.5)) - 0.5))/0.5
  }
  return(x)
}

y <- coin_std_normal(10000)
mean(y)
sd(y)
hist(y)

################################################################################
#12.7.2

lcg <- function(a, b, m, s, n) {
  x <- 0
  x[1] <- s
  for (i in 2:n) {
    x[i] <- (a*s + b) %% m
    s <- x[i]
  }
  return(x/m)
}

y <- lcg(2, 3, 10, 5, 1000)

y1 <- lcg(1229, 1, 2048, 1, 10000)
hist(y1)

x1 <- y1[seq(1, 10000, 2)]
x2 <- y1[seq(2, 10000, 2)]
plot(x1, x2)

y2 <- lcg(1597, 51749, 244944, 1, 10000)
hist(y2)

x1 <- y2[seq(1, 10000, 2)]
x2 <- y2[seq(2, 10000, 2)]
plot(x1, x2)
