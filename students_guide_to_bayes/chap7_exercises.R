library(tidyverse)
data <- read.csv("/Users/krishanth/Code/r/students_guide_to_bayes/All_data/posterior_gdpInfantMortality.csv")

ggplot(data, aes(gdp, infant.mortality)) +
  geom_point() +
  geom_smooth(method = "lm")

# 7.2 
freqFit <- lm(infant.mortality ~ gdp, data = data)
summary(freqFit)

data <- data %>%
  mutate(log.gdp = log(gdp), log.inf.mortality = log(infant.mortality))

ggplot(data, aes(log.gdp, log.inf.mortality)) +
  geom_point() +
  geom_smooth(method = "lm")

freqLogFit <- lm(log.inf.mortality ~ log.gdp, data = data)
freqLogFitOut <- summary(freqLogFit)

freqLogFitOut$coefficients[1,1] - qnorm(0.9)*freqLogFitOut$coefficients[1,2]
freqLogFitOut$coefficients[1,1] + qnorm(0.9)*freqLogFitOut$coefficients[1,2]

freqLogFitOut$coefficients[2,1] - qnorm(0.9)*freqLogFitOut$coefficients[2,2]
freqLogFitOut$coefficients[2,1] + qnorm(0.9)*freqLogFitOut$coefficients[2,2]

mcmc <- read.csv("/Users/krishanth/Code/r/students_guide_to_bayes/All_data/posterior_posteriorsgdpInfantMortality.csv")

#alpha
mean(mcmc$alpha) - qnorm(0.9)*sd(mcmc$alpha)
mean(mcmc$alpha) + qnorm(0.9)*sd(mcmc$alpha)

#beta
mean(mcmc$beta) - qnorm(0.9)*sd(mcmc$beta)
mean(mcmc$beta) + qnorm(0.9)*sd(mcmc$beta)

#sigma
mean(mcmc$sigma) - qnorm(0.9)*sd(mcmc$sigma)
mean(mcmc$sigma) + qnorm(0.9)*sd(mcmc$sigma)

##
# Now to sample from the prior predictive distribution I have to go through the steps
# For each iteration i,
# First, sample parameters
# Then, sample datum from model using parameters from step one
# Save datum
##

data$priordist <- NA
for (i in 1:length(data$log.gdp)) {
  alpha <- rnorm(1, 0, sqrt(10))
  beta <- rnorm(1, 0, sqrt(10))
  sigma <- rnorm(1, 0, sqrt(5))
  data$priordist[i] <- alpha + beta * data$log.gdp[i] + sigma
}

min(data$infant.mortality[!is.na(data$infant.mortality)])
max(data$infant.mortality[!is.na(data$infant.mortality)])

min(data$priordist[!is.na(data$priordist)])
max(data$priordist[!is.na(data$priordist)])

data$postdist <- NA
for (i in 1:length(data$log.gdp)) {
  alpha <- rnorm(1, mean(mcmc$alpha), sd(mcmc$alpha))
  beta <- rnorm(1, mean(mcmc$beta), sd(mcmc$beta))
  sigma <- rnorm(1, mean(mcmc$sigma), sd(mcmc$sigma))
  data$postdist[i] <- alpha + beta * data$log.gdp[i] + sigma
}

ggplot(data) +
  geom_point(aes(log.gdp, log.inf.mortality), color = 'blue', alpha = 0.5) +
  geom_point(aes(log.gdp, postdist), color = 'red', alpha = 0.5)

# 7.3
brain_data <- read.csv("/Users/krishanth/Code/r/students_guide_to_bayes/All_data/posterior_brainData.csv")
colnames(brain_data) <- c("x")

summary(brain_data$x)

