data {
  int<lower=0> N;
  vector[N] y;
  vector[N] sigma_y;
}
parameters {
  vector[N] eta;
  real mu_theta;
  real<lower=0,upper=100> sigma_eta;
  real xi;
}
transformed parameters {
  real<lower=0> sigma_theta;
  vector[N] theta;

  theta <- mu_theta + xi * eta;
  sigma_theta <- fabs(xi) / sigma_eta;
}
model {
  mu_theta ~ normal(0, 100);
  sigma_eta ~ inv_gamma(1, 1); //prior distribution can be changed to uniform

  eta ~ normal(0, sigma_eta);
  xi ~ normal(0, 5);
  y ~ normal(theta,sigma_y);
}
