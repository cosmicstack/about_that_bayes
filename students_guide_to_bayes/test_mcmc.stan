data {
  real Y [10];
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  for(i in 1:10) {
    Y[i] ~ normal(mu, sigma);
  }
  mu ~ normal(1.5, 0.1);
}