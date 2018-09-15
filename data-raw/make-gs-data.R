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
gs <- clean_matches_data(atp_matches = atp_matches,
                         wta_matches = wta_matches,
                         start_year = 2013,
                         end_year = 2017,
                         include_quals = FALSE,
                         level = "Grand Slams")



## Extract sets, points, and games
gs <- extract_sgp(gs)

devtools::use_data(gs, overwrite = TRUE)

## Make into player data
gs_players <- matches_to_player_data(gs,
                                     w_opp_vars = c("loser_rank"),
                                     l_opp_vars = c("winner_rank"),
                                     opp_var_names = c("opponent_rank")
)

devtools::use_data(gs_players, overwrite = TRUE)

###############################################

### pbp data
data(gs_point_by_point)



gs_pbp <- as.data.frame(summarize_pbp(gs_point_by_point,
                        start_year = 2013,
                        end_year = 2017))

devtools::use_data(gs_pbp, overwrite = TRUE)

data(gs)

## Join gs_pbp and gs together

gs_join <- join_gs_pbp(gs_pbp, gs)

gs_partial <- as.data.frame(combine_fields(gs_join))


head(gs_partial[, c("winner_name", "loser_name", "player1", "player2", "w_n_sv")])

devtools::use_data(gs_partial, overwrite = TRUE)

## gs_partial_players
devtools::load_all("../") ## Load court sports functions
data(gs_partial)
gs_partial_players <- matches_to_player_data(gs_partial)

devtools::use_data(gs_partial_players, overwrite = TRUE)
