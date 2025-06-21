library(tidyverse)

perm <- function(n) {
  x <- seq(676, 676-n+1, -1)
  result <- 1
  for (i in 1:length(x)) {
    result <- result * x[i]
  }
  result
}

people <- seq(1:100)
prob <- 0
for (j in 1:100) {
  prob[j] <- 1 - perm(people[j])/676^people[j]  
}

data <- data.frame(people, prob)

ggplot(data, aes(people, prob)) +
  geom_line()