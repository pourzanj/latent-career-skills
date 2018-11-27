game <- read_csv("data/sample-pbp-log/(2016-06-10)-0041500404-GSW@CLE.csv") %>%
  select(a1:h5, team, event_type, period, play_length, points, type, num, outof) %>%
  filter(type != "Free Throw Technical") %>%
  select(-type)

#(1) attempts a field goal,
#(2) misses a shot and does not get the offensive rebound,
#(3) turns the ball over (some sources add “turnovers that are assigned to teams” for a more precise possession calculation),
#(4) goes to the line for two or three shots and either makes the last shot or does not get the rebound of a missed last shot.

# possession ends once there is one of the following
# 1) miss
# 2) shot (make)
# 3) foul (offensive)
# 4) free throw
# 5) turnover
# 6) violation (shotclock)
# can't tell offensive foul and shotclock so lets just take miss, shot, free throw, tunover
possessions <-
  game %>%
  filter(event_type %in% c("miss", "shot", "free throw", "turnover")) %>%
  
  # combine free throws into single possession
  mutate(prev_points = lag(points),
         next_points = lead(points)) %>%
  mutate(points = ifelse(event_type == "free throw", ifelse(num == 2 & outof == 2, points + prev_points, points), points)) %>%
  mutate(first_out_of_second_freethrow = ifelse(event_type == "free throw", ifelse(num == 1 & outof == 2, TRUE, FALSE), FALSE)) %>%
  filter(!first_out_of_second_freethrow) %>%
  select(-first_out_of_second_freethrow) %>%
  mutate(is_and_1 = ifelse(event_type == "free throw", ifelse(num == 1 & outof == 1, TRUE, FALSE), FALSE)) %>%
  mutate(next_is_and_1 = lead(is_and_1)) %>%
  mutate(points = ifelse(next_is_and_1, points + next_points, points)) %>%
  filter(!is_and_1) %>%
  select(-prev_points, -next_points, -is_and_1, -next_is_and_1, -num, -outof, -event_type)
  
gsw_posessions <- possessions %>% filter(team == "GSW")
gsw_players <- gsw_posessions %>% select(a1:a5) %>% as.matrix %>% as.vector %>% unique
cle_players <- gsw_posessions %>% select(h1:h5) %>% as.matrix %>% as.vector %>% unique

check_if_name_in_away <- function(name, posessions) {
  posessions %>%
    mutate(name_in_away = a1 == name | a2 == name | a3 == name | a4 == name | a5 == name) %>%
    pull(name_in_away)
}

check_if_name_in_home <- function(name, posessions) {
  posessions %>%
    mutate(name_in_home = h1 == name | h2 == name | h3 == name | h4 == name | h5 == name) %>%
    pull(name_in_home)
}

offensive_player_indicators <- 
  map(gsw_players, check_if_name_in_away, posessions = gsw_posessions) %>%
  map(as_tibble) %>%
  bind_cols %>%
  set_names(gsw_players) %>%
  mutate_all(as.integer)

defensive_player_indicators <- 
  map(cle_players, check_if_name_in_home, posessions = gsw_posessions) %>%
  map(as_tibble) %>%
  bind_cols %>%
  set_names(cle_players) %>%
  mutate_all(as.integer) %>%
  mutate_all(function(x) -1*x)

design_matrix <-
  bind_cols(gsw_posessions %>% select(points), 
  bind_cols(offensive_player_indicators, defensive_player_indicators)) %>%
  na.omit %>%
  mutate(points = points + 1)

fit <- lm(points ~ ., data = design_matrix)

dat <-
  list(K = 4,
       N = nrow(design_matrix),
       D = ncol(design_matrix)-1,
       y = design_matrix %>% pull(points),
       X = design_matrix %>% select(-points) %>% as.matrix,
       tau = 10)
fit <- stan("Stan/ordinal_logistic_regression.stan", data = dat, chains = 1, iter = 1000)
