library(tidyverse)
data <- read_csv('/Users/krishanth/Code/r/students_guide_to_bayes/All_data/subjective_overfitShort.csv')

ggplot(data, aes(x, y)) +
  geom_point()

ols_model <- lm(y ~ x, data = data)
summary(ols_model)

predict(ols_model, data.frame('x' = data$x))

ggplot(data, aes(x, y)) +
  geom_point() +
  geom_abline(slope=ols_model$coefficients[2], intercept = ols_model$coefficients[1])

quint_model <- lm(y ~ poly(x, 5, raw=TRUE), data = data)
summary(quint_model)

y_pred <- predict(quint_model)
q_data <- data.frame('x' = data$x, 'y' = data$y, y_pred)

ggplot(q_data) +
  geom_point(aes(x, y)) +
  geom_abline(slope=ols_model$coefficients[2], intercept = ols_model$coefficients[1]) +
  geom_smooth(aes(x, y_pred), se = FALSE)

## WITH MORE DATA

data <- read_csv('/Users/krishanth/Code/r/students_guide_to_bayes/All_data/subjective_overfitLong.csv')