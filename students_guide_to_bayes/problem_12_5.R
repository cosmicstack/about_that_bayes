setwd("/Users/krishanth/Code/r/students_guide_to_bayes")
library(igraph)
library(tidyverse)

# Get neighborhood matrix
df <- data.frame(
  from = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6),
  to = c(2, 6, 1, 3, 2, 4, 3, 5, 4, 6, 5, 1)
)
g <- graph_from_data_frame(df, directed = TRUE)
adj <- as_adjacency_matrix(g)
adj <- as.matrix(adj)

# Generate transition probability matrix
gen_transition_pmatrix <- function(neighborhood_mat, epsilon) {
  tpm <- matrix(data = NA, nrow = 6, ncol = 6)
  for (i in 1:6) {
    for (j in 1:6) {
      tpm[i, j] <- (1-epsilon)/6 + neighborhood_mat[i, j] * epsilon/2
    }
  }
  return(tpm)
}

## QUESTION 1 ##

mcmc_sim <- function(n, prob_mat) {
  if (n == 1) {
    states_vec <- 0
    states_vec[1] <- sample(seq(1, 6), 1)
    return(states_vec)
  } else {
    states_vec <- 0
    states_vec[1] <- sample(seq(1, 6), 1)
    for (i in 2:n) {
      states_vec[i] <- sample(seq(1, 6), 1, prob = prob_mat[states_vec[i-1], ])
    }
    return(states_vec)  
  }
}

prob_mat <- gen_transition_pmatrix(adj, 0)
tmp_sample <- mcmc_sim(1000, prob_mat)
mean(tmp_sample)

eps_vals <- seq(0, 1, 0.1)
mean_sim <- matrix(data = NA, nrow = 1000, ncol = length(eps_vals))
for (i in 1:length(eps_vals)) {
  prob_mat <- gen_transition_pmatrix(adj, eps_vals[i])
  for (k in 1:1000) {
    sample <- mcmc_sim(1000, prob_mat)
    mean_sim[k, i] <- mean(sample)
  }
}

colnames(mean_sim) <- eps_vals
as.data.frame(mean_sim) %>%
  pivot_longer(cols = everything(), names_to = "epsilon", values_to = "sample_mean") %>%
  ggplot(aes(epsilon, sample_mean)) +
  geom_boxplot()

## QUESTION 3 ##

S = c(5, 10, 100)

error_mat <- matrix(data = NA, nrow = 3, ncol = 11)

for (k in 1:3) {
  mean_sim <- matrix(data = NA, nrow = 1000, ncol = length(eps_vals))
  for (i in 1:length(eps_vals)) {
    prob_mat <- gen_transition_pmatrix(adj, eps_vals[i])
    for (j in 1:1000) {
      sample <- mcmc_sim(S[k], prob_mat)
      mean_sim[j, i] <- mean(sample)
    }
    error_mat[k, i] <- sd(mean_sim[,i])
  }
}

row.names(error_mat) <- S
colnames(error_mat) <- eps_vals

as.data.frame(error_mat) %>%
  pivot_longer(cols = everything(), names_to = "epsilon", values_to = "std_error") %>%
  mutate(
    sample_size = c(rep(5, 11), rep(10, 11), rep(100, 11))
  ) %>%
  ggplot(aes(as.numeric(epsilon), std_error)) +
  geom_line(aes(color = factor(sample_size))) +
  ylim(0, NA) +
  theme_bw()

## QUESTION 4 ##

S <- seq(1, 100)
eps <- c(0, 1)
error_mat <- matrix(data = NA, nrow = 100, ncol = 2)

for (k in 1:length(S)) {
  mean_sim <- matrix(data = NA, nrow = 1000, ncol = length(eps))
  for (i in 1:length(eps)) {
    prob_mat <- gen_transition_pmatrix(adj, eps[i])
    for (j in 1:1000) {
      sample <- mcmc_sim(S[k], prob_mat)
      mean_sim[j, i] <- mean(sample)
    }
    error_mat[k, i] <- sd(mean_sim[,i])
  }
}

colnames(error_mat) <- c("e0", "e1")
as.data.frame(error_mat) %>%
  ggplot(aes(x = 1:100)) +
  geom_line(aes(y = e0), color = "blue") +
  geom_line(aes(y = e1), color = "red") +
  geom_hline(yintercept = error_mat[100, 2])
