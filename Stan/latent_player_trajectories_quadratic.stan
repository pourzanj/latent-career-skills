data {
  int N; // num players
  int M; // dimension of data vectors
  int L; // num of latent dimensions
  int S; // total num of observations
  
  matrix [M,S] X;
  matrix[M,S] sigma;
  real t[S]; // mean-centered age of player for each obs in X
  
  int player_id[S]; // identifies which player obs. t[s] and X[s] belongs to
  int season_id[S]; // identifies which season t[s] and X[s] belongs to
}
parameters {
  // level 1: latent trajectory parameters for each player and latent skill
  // a and b are zeros of the quadratic so much be ordered to avoid
  // a switching identifiability as Richard pointed out.
  // c is constrained to be negative because a player trajectory must go up then down
  ordered[2] ab[N,L];
  real<upper=0.0> c[N, L];
  
  // level 2: factor model to generate stats from latent trajectories
  vector[M] mu;
  matrix[M,L] W_raw;
  vector<lower=0>[L] Lambda;
}
transformed parameters {
  matrix[L,S] Z;
  matrix[M,S] X_tilde;
  matrix[M,L] W = qr_Q(W_raw)[,1:L];
  
  // evaluate latent trajectories at the times we observed data
  for(s in 1:S) {
    for(l in 1:L) {
      Z[l,s] = c[player_id[s],l] * (t[s] - ab[player_id[s],l][1]) * (t[s] - ab[player_id[s],l][2]);
    }
  }
  
  // create expected mean data from latent trajectories
  X_tilde = rep_matrix(mu, S) + W * diag_pre_multiply(Lambda, Z);
}
model {
  
  // level 1 parameters
  for(n in 1:N) {
    for(l in 1:L) {
      ab[n,l][1] ~ normal(-8.0, 2.0);
      ab[n,l][2] ~ normal(4.0, 2.0);
    }
  }

  to_array_1d(c) ~ normal(-1.0, 1.0);
  
  // level 2 parameters
  to_array_1d(W_raw) ~ normal(0,1);
  
  // likelihood
  // iterate over players then the covariates in the data vector
  for(s in 1:S) {
    for(m in 1:M) {
      X[m,s] ~ normal(X_tilde[m,s], sigma[m,s]);
    }
  }
}
