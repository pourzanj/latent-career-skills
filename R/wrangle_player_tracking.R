require(tidyverse)

#################
# Catch and Shoot
#################
catch_shoot_2013_14 <- read_csv("data/csvs 2/catch_shoot_2013-14.csv") %>% mutate(season = "2013-14", season_start_date = as.Date("2013-11-01")) %>% select(-X1) %>% select(season, season_start_date, everything())
catch_shoot_2014_15 <- read_csv("data/csvs 2/catch_shoot_2014-15.csv") %>% mutate(season = "2014-15", season_start_date = as.Date("2014-11-01")) %>% select(-X1) %>% select(season, season_start_date, everything())
catch_shoot_2015_16 <- read_csv("data/csvs 2/catch_shoot_2015-16.csv") %>% mutate(season = "2015-16", season_start_date = as.Date("2015-11-01")) %>% select(-X1) %>% select(season, season_start_date, everything())
catch_shoot_2016_17 <- read_csv("data/csvs 2/catch_shoot_2016-17.csv") %>% mutate(season = "2016-17", season_start_date = as.Date("2016-11-01")) %>% select(-X1) %>% select(season, season_start_date, everything())
catch_shoot_2017_18 <- read_csv("data/csvs 2/catch_shoot_2017-18.csv") %>% mutate(season = "2017-18", season_start_date = as.Date("2017-11-01")) %>% select(-X1) %>% select(season, season_start_date, everything())

catch_shoot <- bind_rows(catch_shoot_2013_14, catch_shoot_2014_15, catch_shoot_2015_16, catch_shoot_2016_17, catch_shoot_2017_18)

#################
# Get Age
#################
# first get all the uniqe player IDs that are in our catch and shoot table
# pulling all profiles takes about 6 minutes on Arya's Macbook Pro
unique_player_ids <- catch_shoot %>% pull(PLAYER_ID) %>% unique()
player_profiles <- player_profiles(player_ids = unique_player_ids) %>%
  select(idPlayer, namePlayer, dateBirth, yearSeasonFirst, heightInches, weightLBS)