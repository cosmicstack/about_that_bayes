library(tidyverse)
#6.1 Check if it is a valid probability distribution
j_prob <- function(x) {
  0.5^(x[1]+x[2]) * 0.5^(2-x[1]-x[2]) * 0.9^(x[3]+x[4]) * 0.1^(2-x[3]-x[4]) * 0.5^2
}

j_prob(c(0,1,0,0))

df <- data.frame(no=1:16, expand.grid(replicate(4, 0:1, simplify = FALSE))) %>%
  nest(data = -no) %>%
  mutate(prob = map(data, j_prob)) %>%
  unnest(prob)

sum(df$Var1)

#6.2
data <- read.csv("/Users/krishanth/Code/r/students_guide_to_bayes/All_data/denominator_NBCoins.csv")
data$X17