## SKG
## September 13, 2018
## Happy grand opening tepper quad
## Analyzing grand slam point by point

library(deuce)
library(dplyr)
library(forcats)

data(gs_point_by_point)

dim(gs_point_by_point)
summary(gs_point_by_point)

## too long
gs_pbp <- gs_point_by_point
na_col_inds <- colSums(is.na(gs_pbp))
complete_cols <- which(na_col_inds == 0)
names(gs_pbp)[-complete_cols]
gs_complete <- gs_pbp %>% dplyr::select(names(gs_pbp[complete_cols])) %>%
    filter(year >= 2013 & year <= 2017)
gs_complete$tournament <- factor(gs_complete$slam) %>% 
    forcats::fct_collapse(
                 "US Open" = c("usopen"),
                 "French Open" = c("frenchopen"),
                 "Wimbledon" = c("wimbledon"),
                 "Australian Open" = c("ausopen"))



#######################

## Mutating into match stats
match_stats <- gs_complete %>% group_by(match_id, player1, player2, Tour,  
                                       slam, year, tournament) %>%
    dplyr::summarize(ave_serve_speed_p1 = mean(Speed_MPH[PointServer == 1]),
                     ave_serve_speed_p2 = mean(Speed_MPH[PointServer == 2]),
                     n_aces_p1 = sum(P1Ace),
                     n_aces_p2 = sum(P2Ace),
                     n_winners_p1 = sum(P1Winner),
                     n_winners_p2 = sum(P2Winner),
                     n_netpt_wpct_p1 = sum(P1NetPointWon) / sum(P1NetPoint),
                     n_netpt_wpct_p2 = sum(P1NetPointWon) / sum(P1NetPoint),
                     n_bp_wpct_p1 = sum(P1BreakPointWon) / sum(P1BreakPoint),
                     n_bp_wpct_p1 = sum(P2BreakPointWon) / sum(P2BreakPoint),
                     n_netpt_wpct_p2 = sum(P1NetPointWon) / sum(P1NetPoint),
                     n_ue_p1 = sum(P1UnfErr),
                     n_ue_p2 = sum(P2UnfErr),
                     n_sv_wpct_p1 = sum((PointServer == 1) * (PointWinner == 1 ) )/
                         sum(PointServer == 1),
                     n_sv_wpct_p2 = sum((PointServer == 2) * (PointWinner == 2 ) )/
                         sum(PointServer == 2),
                     a1 = sort(c(player1, player2))[1],
                     a2 = sort(c(player1, player2), decreasing = TRUE)[1])
head(match_stats[, c("player1", "player2", "a1", "a2")])


devtools::load_all("../../")

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

# devtools::use_data(gs, overwrite = TRUE)

## Make into player data
gs_sub <- gs %>% group_by(match_id, tournament, year,  winner_name, loser_name, winner_rank,
                        loser_rank, winner_age, winner_ioc, Retirement,
                        w_pointswon, w_gameswon, w_setswon,
                        l_pointswon, l_gameswon, l_setswon,
                        round) %>%
    summarize(a1 = sort(c(winner_name, loser_name))[1],
              a2 = sort(c(winner_name, loser_name), decreasing = TRUE)[1])
head(gs_sub[, c("winner_name", "loser_name", "a1", "a2")])

gs_join <- left_join(gs_sub, match_stats, by = c("tournament", "year", "a1", "a2"))



head(gs_join[, c("player1", "winner_name", "player2", "loser_name")])
gs_partial_pbp <- na.omit(gs_join)

dim(gs_partial_pbp)

devtools::use_data(gs_partial_pbp, overwrite = TRUE)
