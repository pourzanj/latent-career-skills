require(tidyverse)
require(reshape2)
require(rstan)
require(splines)
options(mc.cores = parallel::detectCores())

forward_simulate_shared_basis <- function(N, M, L) {
  players <-
    tibble(player_id = 1:N) %>%
    mutate(num_seasons = sample(1:5, N, replace = TRUE)) %>%
    mutate(starting_age = rnorm(N, -10, 1.0))
  
  # helper function that creates each players latent trajectories based on id, num_seasons and starting age
  # use the parameters to evaluate what the players' latent skills are at the begining of each season
  compute_latent_trajectories <- function(player_id, num_seasons, starting_age) {
    expand.grid(player_season = 1:num_seasons) %>%
      mutate(t = starting_age + player_season) %>%
      mutate(player_id = player_id) %>%
      select(player_id, player_season, t) #%>%
      #inner_join(latent_skill_parameters) 
  }
  
  latent_skill_per_season <-
    players %>%
    pmap(compute_latent_trajectories) %>%
    bind_rows() %>%
    as_tibble %>%
    select(player_id, player_season, t)
  
  shared_basis <- t(bs(latent_skill_per_season$t + latent_skill_per_season$player_season, 
                       seq(
                         from = min(latent_skill_per_season$t), 
                         to = max(latent_skill_per_season$t + latent_skill_per_season$player_season)), 
                       degree = L, 
                       intercept = TRUE))
  
  n_basis = nrow(shared_basis)
  W <- array(rnorm(N * M * n_basis, 0, 5), c(N, M, n_basis))
  
  mu <- runif(M, 0, 1)
  # draw noise by multiplying N(0,1) draw by the individual sigma_itj for each stat
  # sigma_itj represents the noise level of the jth stat for the ith player at seasons t
  # and is essentially a normalizer depending much data we have for that stat e.g. if the stat
  # is the player's FG% on drives then then sigma_ijt would be the number of field goals they attempted on drives that seasons
 
  X_tilde <- array(0, c(M,nrow(latent_skill_per_season)));
  dim(X_tilde)
  start = 1
  for(i in 1:N) {
    X_tilde[,start:start + players$num_seasons[i] - 1] = W[i,,] %*% shared_basis[,start:start + players$num_seasons[i] - 1]
    start = start + players$num_seasons[i]
  }
  
  S <- dim(X_tilde)[2]
  sigma <- matrix(1/rgamma(M*S, shape = 2.0, rate = 2.0), nrow = M) %>% sqrt()
  eps <- sigma * matrix(rnorm(M*S), nrow = M)
  # add noise to X_tilde to get X.
  X <- X_tilde + eps
  
  # return all synthetically generate data as list
  list(N = N,
       M = M,
       S = S,
       players = players,
       shared_basis = shared_basis,
       latent_skill_per_season = latent_skill_per_season,
       W = W,
       mu = mu,
       sigma = sigma,
       X = X,
       obs_counts = players$num_seasons
  )
}

# simulate and infer in Stan
synthetic_data <- forward_simulate_shared_basis(N = 10, M = 3, L = 3)

dat <- list(
  N = synthetic_data$N,
  M = synthetic_data$M,
  L = nrow(synthetic_data$shared_basis),
  S = synthetic_data$S,
  
  X = synthetic_data$X,
  sigma = synthetic_data$sigma,
  Z = synthetic_data$shared_basis,
  obs_counts = synthetic_data$obs_counts
)

fit <- stan(file = "Stan/latent_player_trajectories_shared_basis.stan", data = dat, chains = 4, iter = 2000, refresh = 20)

# plot posterior predictives of data
extract(fit, pars = c("X_rep"))$X_rep[,1,1:10] %>%
  melt() %>%
  as_tibble() %>%
  set_names(c("sample_id", "time_id", "value")) %>%
  inner_join(tibble(time_id = 1:10, t = synthetic_data$latent_skill_per_season$t[1:10])) %>%
  ggplot(aes(t, value, group = sample_id)) +
  geom_point(alpha = 0.1) +
  geom_point(aes(t, V1, group = NULL), color = "red", data =
               tibble(time_id = 1:10, t = synthetic_data$latent_skill_per_season$t[1:10]) %>% bind_cols(as_tibble(t(synthetic_data$X[,1:10]))) %>% select(-V2)
             )

W_fit <- extract(fit, pars = c("W"))$W

full_t <- seq(from = min(synthetic_data$latent_skill_per_season$t),
              to =  max(synthetic_data$latent_skill_per_season$t + synthetic_data$latent_skill_per_season$player_season),
              by = .1)

full_basis <- t(bs(full_t, 
                   seq(
                     from = min(synthetic_data$latent_skill_per_season$t), 
                     to = max(synthetic_data$latent_skill_per_season$t + synthetic_data$latent_skill_per_season$player_season),
                     1), 
                   degree = 10, 
                   intercept = TRUE))
attributes(full_basis) <- attributes(full_basis)["dim"]

# Plot the underlying basis functions
df <- full_basis %>% as.tibble() %>% melt() %>% set_names(c("time","value"))
df$row_id <- 1:nrow(full_basis)
df$time <- as.numeric(str_replace(df$time, "V", ""))
df$time <- full_t[df$time]

df %>% ggplot(aes(time, value, group = factor(row_id))) + geom_line(aes(color=factor(row_id)))

# Plot a single sample set of trajectories for a single player
sample_idx = 10
player_idx = 1
start_idxs = cumsum(synthetic_data$players$num_seasons)
sample_trajectory <- 
  W_fit[sample_idx,player_idx,,] %*% full_basis %>% 
  as.tibble %>%
  melt() %>% set_names(c("time", "value"))

sample_trajectory$row_id <- 1:synthetic_data$M
sample_trajectory$time <- as.numeric(str_replace(sample_trajectory$time,"V", ""))
sample_trajectory$time <- full_t[sample_trajectory$time]

sample_trajectory %>% ggplot(aes(time, value, group = factor(row_id))) + geom_line(aes(color = factor(row_id))) 
  
# Plot the true trajectories of the same player
true_trajectory <- 
  synthetic_data$W[player_idx,,] %*% full_basis %>% 
  as.tibble %>%
  melt() %>% set_names(c("time", "value"))

true_trajectory$row_id <- 1:synthetic_data$M
true_trajectory$time <- as.numeric(str_replace(true_trajectory$time,"V", ""))
true_trajectory$time <- full_t[true_trajectory$time]

true_trajectory %>% ggplot(aes(time, value, group = factor(row_id))) + geom_line(aes(color = factor(row_id))) 
