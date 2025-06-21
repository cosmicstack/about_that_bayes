library(tidyverse)
#4.1
data <- read.csv('/Users/krishanth/Code/r/students_guide_to_bayes/All_data/likelihood_blogVisits.csv', header = F)
head(data)
lambda_h <- 1/mean(data$V1)
lam <- seq(0, 10, 0.01)
ll <- 0
for (i in 1:1001) {
  ll[i] <- 50*log(lam[i]) - sum(lam[i] * data$V1)
}
ll_df <- data.frame('x' = lam, 'y' = ll)
ggplot(ll_df, aes(x, y)) +
  geom_line()

lambda_h - 1.960 * sd(data$V1)/sqrt(50)
lambda_h + 1.960 * sd(data$V1)/sqrt(50)

# Using lambda_h calculate probabilities
exp_prob <- function(t) {
  exp(-lambda_h * t)
}
exp_prob(5)

sim <- rexp(50, rate = lambda_h)
new_data <- data.frame("real" = data$V1, "sim" = sim)

ggplot(new_data) +
  geom_histogram(aes(x = real), binwidth = 0.5, center = 0.25, alpha = 0.5, fill = 'red') +
  geom_histogram(aes(x = sim), binwidth = 0.5, center = 0.25, alpha = 0.4)

################### IMPORTANT LINES BELOW: HOW TO DO MLE

ll_lomax <- function(alpha, beta) {
  n <- length(data$V1)
  res <- n*log(alpha) + n*alpha*log(beta) - (alpha + 1)*sum(log(beta + data$V1))
  return(-res)
}

stats4::mle(ll_lomax, start = list(alpha=1, beta=1), method = "BFGS")

###################


#4.2
data2 <- read.csv('/Users/krishanth/Code/r/students_guide_to_bayes/All_data/likelihood_NewYorkCrimeUnemployment.csv')
summary(data2)
data2$Violent_crime_count = as.numeric(gsub(",", "", data2$Violent_crime_count))
data2$Population = as.numeric(gsub(",", "", data2$Population))
ggplot(data2) +
  aes(Population, Violent_crime_count) +
  geom_point()

ll_poisson <- function(theta) {
  res <- sum(data2$Violent_crime_count * log(data2$Population * theta)) - sum(data2$Population * theta) - sum(log(factorial(data2$Violent_crime_count)))
  return (-res)
}

stats4::mle(ll_poisson, start = list(theta = 1), method = "BFGS")