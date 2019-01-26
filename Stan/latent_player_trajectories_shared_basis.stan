data {
  int N; // num players
  int M; // dimension of data vectors
  int L; // num of latent dimensions
  int S; // total num of observations
  
  matrix[M,S] X;
  matrix[M,S] sigma;
  matrix[L,S] Z;
  
  int obs_counts[N];
}

parameters {
  vector[M] mu;
  matrix[M,L] W[N];
}

transformed parameters {
  matrix[M,S] X_tilde;
  {
    int start;
    // create expected mean data from latent trajectories
    start = 1;
    for(i in 1:N) {
      X_tilde[,start:start + obs_counts[i] - 1] = rep_matrix(mu, obs_counts[i]) + W[i] * Z[:,start:start + obs_counts[i] - 1];
      start = start + obs_counts[i];
    }
  }
}

model {
  for(i in 1:N) {
    to_vector(W[i]) ~ normal(0,5);
  }
  // likelihood
  // iterate over players then the covariates in the data vector
  for(s in 1:S) {
    for(m in 1:M) {
      X[m,s] ~ normal(X_tilde[m,s], sigma[m,s]);
    }
  }
}

generated quantities {
  matrix[M,S] X_rep;

  for(s in 1:S) {
    for(m in 1:M) {
      X_rep[m,s] = normal_rng(X_tilde[m,s], sigma[m,s]);
    }
  }
}
