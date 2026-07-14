library(rethinking)
library(tidyverse)

# King Markov examples
weeks <- 52*25
positions <- 0
current <- sample(seq(1, 10), 1)

for (i in 1:weeks) {
  positions[i] <- current
  proposal <- current + sample(c(-1,1), size = 1)
  
  proposal <- ifelse(proposal > 10, 1, proposal)
  proposal <- ifelse(proposal < 1, 10, proposal)
  
  prob_move <- proposal/current
  current <- ifelse(runif(1) < prob_move, proposal, current)
}

hist(positions)
data.frame(x = 1:length(positions), y = positions) %>%
  ggplot(aes(x, y)) +
    geom_line(alpha=0.5)
