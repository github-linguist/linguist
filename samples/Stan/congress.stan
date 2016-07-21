data {
  int<lower=0> N;
  vector[N] incumbency_88;
  vector[N] vote_86;
  vector[N] vote_88;
}
parameters {
  vector[3] beta;
  real<lower=0> sigma;
}
model {
    vote_88 ~ normal(beta[1] + beta[2] * vote_86
                     + beta[3] * incumbency_88,sigma);
}
