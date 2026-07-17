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

data("rugged")
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$log_gdp), ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse(dd$cont_africa == 1, 1, 2)

dat_slim <- list(
  log_gdp_std = dd$log_gdp_std,
  rugged_std = dd$rugged_std,
  cid = as.integer(dd$cid)
)

# using data as list instead of data.frame as length could be variable and
# data.frame doesn't take variable lengths for individual columns

m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm(mu, sigma),
    mu <- a[cid] + b[cid] * (rugged_std - 0.215),
    a[cid] ~ dnorm(1, 0.1),
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ),
  data = dat_slim,
  chains = 4,
  cores = 4,
  warmup = 100,
  iter = 1000
)

precis(m9.1, depth = 2)
pairs(m9.1)
plot(precis(m9.1, depth = 2, prob = 0.89))
show(m9.1)

traceplot(m9.1)

post <- extract.samples(m9.1)

hist(post$a[, 2])

tmp <- post$a[, 2]
tmp_std <- standardize(tmp)
norm_std <- rnorm(2000)

dens(tmp_std)
dens(norm_std)

trankplot(m9.1)
