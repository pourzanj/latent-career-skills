data {
  int<lower=2> K;
  int<lower=0> N;
  int<lower=1> D;
  int<lower=1,upper=4> y[N];
  matrix[N,D] X;
  real<lower=0> tau;
}
parameters {
  vector[D] beta;
  ordered[K-1] c;
}
model {
  vector[N] mu = X*beta;
  beta ~ normal(0,tau);
  for (n in 1:N)
    y[n] ~ ordered_logistic(mu[n], c);
}
