## Updated in August 2020 to get data up through 2019
## Updated in March 2021 to get deuce updates
## Script to make data used in our analysis, specifically
##
## 1. gs.rda
## 2. gs_players.rda
## 3. gs_pbp.rda
## 4. gs_partial.rda

devtools::load_all("../") ## Load court sports functions
library(deuce)
library(dplyr)


## Load WTA and ATP matches from deuce package
data(atp_matches)
data(wta_matches)

## Clean data
gs_v3 <- clean_matches_data(atp_matches = atp_matches,
                         wta_matches = wta_matches,
                         start_year = 2013,
                         end_year = 2019,
                         include_quals = FALSE,
                         level = "Grand Slams")



## Extract sets, points, and games
gs_v3 <- extract_sgp(gs_v3)

usethis::use_data(gs_v3, overwrite = TRUE)

## Make into player data
gs_players_v3 <- matches_to_player_data(gs_v3,
                                     w_opp_vars = c("loser_rank"),
                                     l_opp_vars = c("winner_rank"),
                                     opp_var_names = c("opponent_rank")
)

usethis::use_data(gs_players_v2, overwrite = TRUE)

###############################################

### pbp data
data(gs_point_by_point)

gs_point_by_point %>% filter(year >= 2013 & year <= 2019) %>%
  select(year, slam) %>% table()



gs_pbp_v3 <- as.data.frame(summarize_pbp(gs_point_by_point,
                        start_year = 2013,
                        end_year = 2019))

usethis::use_data(gs_pbp_v3, overwrite = TRUE)

data(gs_v3)

## Join gs_pbp and gs together

gs_join <- join_gs_pbp(gs_pbp_v3, gs_v3)

gs_partial_v3 <- as.data.frame(combine_fields(gs_join))


head(gs_partial_v3[, c("winner_name", "loser_name", "player1", "player2", "w_n_sv")])

usethis::use_data(gs_partial_v3, overwrite = TRUE)

## gs_partial_players
devtools::load_all("../") ## Load court sports functions
data(gs_partial_v3)
gs_partial_players_v3 <- matches_to_player_data(gs_partial_v3)

usethis::use_data(gs_partial_players_v3, overwrite = TRUE)
