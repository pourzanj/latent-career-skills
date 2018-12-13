library(mvtnorm)

forward_simulate_quadratic <- function(N, M, L) {
  
  # draw fake players with fake number of seasons and starting age
  players <-
    tibble(player_id = 1:N) %>%
    mutate(num_seasons = sample(1:5, N, replace = TRUE)) %>%
    mutate(starting_age = rnorm(N, -10, 1.0))
  
  # draw latent skill quadratic curves for each of the players L skills
  latent_skill_parameters <-
    expand.grid(player_id = 1:N, skill = 1:L) %>%
    as_tibble %>%
    mutate(a = rnorm(nrow(.), -8.0, 1.0),
           b = rnorm(nrow(.), 4.0, 1.0),
           c = -abs(rnorm(nrow(.), 0.0, 1.0)))
  
  # helper function that creates each players latent trajectories based on id, num_seasons and starting age
  # use the parameters to evaluate what the players' latent skills are at the begining of each season
  compute_latent_trajectories <- function(player_id, num_seasons, starting_age) {
    expand.grid(player_season = 1:num_seasons,
                skill = 1:L) %>%
      mutate(t = starting_age + player_season) %>%
      mutate(player_id = player_id) %>%
      select(player_id, player_season, t, skill) %>%
      inner_join(latent_skill_parameters) %>%
      mutate(z = c*(t-a)*(t-b))
  }
  
  latent_skill_per_season <-
    players %>%
    pmap(compute_latent_trajectories) %>%
    bind_rows() %>%
    as_tibble %>%
    select(player_id, player_season, t, skill, z) %>%
    spread(skill, z)
  
  # sample Factor model parameters W, and Lambda
  W <-
    matrix(rnorm(M*L), ncol = L) %>%
    qr %>%
    qr.Q
  
  Lambda <- runif(L, 0, 100) %>% abs %>% sort(decreasing = TRUE)
  
  # use latent skills per season to create expected stats levels X_tilde
  # X_tilde = W Lambda Z. x_it = mu + W*Lambda*z_it + eps_it
  Z <-
    latent_skill_per_season %>%
    select(-player_id, -player_season, -t) %>%
    as.matrix %>%
    t()
  
  mu <- runif(M, 0, 100)
  
  X_tilde <- mu + W %*% (Lambda * Z)
  
  # draw noise by multiplying N(0,1) draw by the individual sigma_itj for each stat
  # sigma_itj represents the noise level of the jth stat for the ith player at seasons t
  # and is essentially a normalizer depending much data we have for that stat e.g. if the stat
  # is the player's FG% on drives then then sigma_ijt would be the number of field goals they attempted on drives that seasons
  S <- dim(X_tilde)[2]
  sigma <- matrix(1/rgamma(M*S, shape = 2.0, rate = 2.0), nrow = M)
  eps <- sigma * matrix(rnorm(M*S), nrow = M)
  
  # add noise to X_tilde to get X.
  X <- X_tilde + eps
  
  # return all synthetically generate data as list
  list(N = N,
       M = M,
       L = L,
       S = S,
       players = players,
       latent_skill_parameters = latent_skill_parameters,
       latent_skill_per_season = latent_skill_per_season,
       W = W,
       Z = Z,
       mu = mu,
       sigma = sigma,
       X = X
       )
}

# simulate and infer in Stan
synthetic_data <- forward_simulate_quadratic(100, 10, 3)

dat <- list(
  N = synthetic_data$N,
  M = synthetic_data$M,
  L = synthetic_data$L,
  S = synthetic_data$S,
  
  X = synthetic_data$X,
  sigma = synthetic_data$sigma,
  t = synthetic_data$latent_skill_per_season$t,
  
  player_id = synthetic_data$latent_skill_per_season$player_id,
  season_id = synthetic_data$latent_skill_per_season$player_season
)