library(tidyverse)
library(rethinking)

# 12M1
ratings <- c(12, 36, 7, 41)
ratings_prop <- ratings/sum(ratings)
ratings_prop_cumsum <- cumsum(ratings_prop)
ratings_prop_cumsum_odds <- log(ratings_prop_cumsum/(1-ratings_prop_cumsum))

# 12M2
data.frame(x=1:4, cumul_prop=ratings_prop_cumsum, prop=ratings_prop) %>%
  mutate(prev_cumul = lag(cumul_prop, default = 0)) %>%
  ggplot() +
  geom_col(aes(x, cumul_prop), width = 0.05, fill="grey") +
  geom_point(aes(x, cumul_prop), shape = 1) +
  geom_line(aes(x, cumul_prop)) +
  geom_rect(
    aes(
      xmin = x + 0.05 / 2,
      xmax = x + 0.05 * 1.5,
      ymin = prev_cumul,
      ymax = cumul_prop
    ),
    fill = "skyblue"
  ) +
  theme_bw()

# 12H1
data("Hurricanes")
d <- Hurricanes
head(d)

dat <- list(
  D = d$deaths,
  F = d$femininity
)

m.12h1.1 <- ulam(
  alist(
    D ~ dpois(lambda),
    log(lambda) <- a + bF*F,
    a ~ dnorm(1, 0.5),
    bF ~ dnorm(0, 0.3)
  ),
  data = dat,
  chains = 4,
  cores = 4
)

m.12h1.2 <- ulam(
  alist(
    D ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(1, 0.5)
  ),
  data = dat,
  chains = 4,
  cores = 4
)

trankplot(m.12h1.1)
trankplot(m.12h1.2)

plot(coeftab(m.12h1.1, m.12h1.2))
