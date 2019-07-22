data {
  int N;
  int Nt;
  int x1_N;                      //  number of categories in x1
  int x2_N;                      //  number of categories in x2
  int x3_N;                      //  number of categories in x3
  int x4_N;                      //  number of categories in x4
  int x5_N;                      //  number of categories in x5
  int x6_N;                      //  number of categories in x6
  int x7_N;                      //  number of categories in x7
                 

  int<lower=1, upper=x1_N> x1[N];
  int<lower=1, upper=x2_N> x2[N];
  int<lower=1, upper=x3_N> x3[N];
  int<lower=1, upper=x4_N> x4[N];
  int<lower=1, upper=x5_N> x5[N];
  int<lower=1, upper=x6_N> x6[N];
  int<lower=1, upper=x7_N> x7[N];
  
  int<lower=1, upper=x1_N> x1t[Nt];
  int<lower=1, upper=x2_N> x2t[Nt];
  int<lower=1, upper=x3_N> x3t[Nt];
  int<lower=1, upper=x4_N> x4t[Nt];
  int<lower=1, upper=x5_N> x5t[Nt];
  int<lower=1, upper=x6_N> x6t[Nt];
  int<lower=1, upper=x7_N> x7t[Nt];
  
  
  int<lower=0, upper=1> y[N];
}

parameters {
  real intercept;
  vector[x1_N] x1_coeff;
  vector[x2_N] x2_coeff;
  vector[x3_N] x3_coeff;
  vector[x4_N] x4_coeff;
  vector[x5_N] x5_coeff;
  vector[x6_N] x6_coeff;
  vector[x7_N] x7_coeff;
  
  
  real<lower=0> sigma_x1;
  real<lower=0> sigma_x2;
  real<lower=0> sigma_x3;
  real<lower=0> sigma_x4;
  real<lower=0> sigma_x5;
  real<lower=0> sigma_x6;
  real<lower=0> sigma_x7;

}

transformed parameters {
  vector[x1_N] normalized_x1_coeff;
  vector[x2_N] normalized_x2_coeff;
  vector[x3_N] normalized_x3_coeff;
  vector[x4_N] normalized_x4_coeff;
  vector[x5_N] normalized_x5_coeff;
  vector[x6_N] normalized_x6_coeff;
  vector[x7_N] normalized_x7_coeff;
 
  normalized_x1_coeff = x1_coeff - x1_coeff[1];
  normalized_x2_coeff = x2_coeff - x2_coeff[1];
  normalized_x3_coeff = x3_coeff - x3_coeff[1];
  normalized_x4_coeff = x4_coeff - x4_coeff[1];
  normalized_x5_coeff = x5_coeff - x5_coeff[1];
  normalized_x6_coeff = x6_coeff - x6_coeff[1];
  normalized_x7_coeff = x7_coeff - x7_coeff[1];

}

model {
  vector[N] p;                  // probabilities
  // priors
  intercept ~ normal(0, 20);
  sigma_x1 ~ cauchy(0, 5);
  sigma_x2 ~ cauchy(0, 5);
  sigma_x3 ~ cauchy(0, 5);
  sigma_x4 ~ cauchy(0, 5);
  sigma_x5 ~ cauchy(0, 5);
  sigma_x6 ~ cauchy(0, 5);
  sigma_x7 ~ cauchy(0, 5);

  // level 1
  x1_coeff ~ normal(0, sigma_x1);
  x2_coeff ~ normal(0, sigma_x2);
  x3_coeff ~ normal(0, sigma_x3);
  x4_coeff ~ normal(0, sigma_x4);
  x5_coeff ~ normal(0, sigma_x5);
  x6_coeff ~ normal(0, sigma_x6);
  x7_coeff ~ normal(0, sigma_x7);


  // level 2
  for (i in 1:N) {
    p[i] <- x1_coeff[x1[i]] + x2_coeff[x2[i]] + x3_coeff[x3[i]] + x4_coeff[x4[i]]+  x5_coeff[x5[i]]+  x6_coeff[x6[i]]+ x7_coeff[x7[i]];
    
  }
  y ~ bernoulli_logit(intercept + p);
}

generated quantities{
  vector[Nt] y_test;
  vector[Nt] pt;
  for (i in 1:Nt) {
    pt[i] <- x1_coeff[x1t[i]] + x2_coeff[x2t[i]] + x3_coeff[x3t[i]] + x4_coeff[x4t[i]]+  x5_coeff[x5t[i]]+  x6_coeff[x6t[i]]+  x7_coeff[x7t[i]];
    y_test[i] = bernoulli_rng(inv_logit(intercept + pt[i]));
  }
}
