# Multilogit Stan

data {
  int<lower=2> K;             // number of categories
  int<lower=2> D;             // number of predictors
  int<lower=0> N;             // number of observations
  matrix[N, D] x;             // predictors
  matrix[N, D] xt;             // tests
  int<lower=1,upper=K> y[N];  // observations
}
parameters {
  matrix[K, D] beta;      // slopes
}
model {
  matrix[N, K] gamma;
  gamma = x * beta';

  // prior
  to_vector(beta) ~ normal(0, 5);

  // likelihood
  for (n in 1:N)
    y[n] ~ categorical_logit(gamma[n]');
}
generated quantities{
    matrix[N, K] gamma2;
  gamma2 = xt * beta';
    for (n in 1:N)
    y[n] = categorical_logit_rng(gamma2[n]');
}