## SKG
## September 13, 2018
## Happy grand opening tepper quad
## Analyzing grand slam point by point

library(deuce)
library(dplyr)
library(forcats)
library(ggplot2)

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

gs_partial_pbp$Week <- gs_partial_pbp$round %>%
    forcats::fct_collapse("Week 1" = c("R128","R32", "R64" ),
                          "Week 2" = c("R16", "QF", "SF", "F"))

dim(gs_partial_pbp)

devtools::use_data(gs_partial_pbp, overwrite = TRUE)




## get court

court <- gs_partial_pbp %>% group_by(tournament, Tour, year,
                                     week) %>%
    summarize(n_retire = sum(Retirement),
              ave_sets = mean(w_setswon + l_setswon),
              ave_aces = mean(n_aces_p1 + n_aces_p2),
              ave_ue = mean(n_ue_p1 + n_ue_p2),
              ave_winners = mean(n_winners_p1 + n_winners_p2),
              med_rank = median(c(loser_rank, winner_rank)),
              n_obs = length(loser_rank)
)




ggplot(court, aes(y=ave_sets, group = tournament, col = tournament)) +
              geom_boxplot() + 
    facet_grid(week~Tour)

ggplot(court, aes(y=ave_aces, group = tournament, col = tournament)) +
              geom_boxplot() + 
              facet_grid(~Tour)

ggplot(court, aes(y=ave_ue, group = tournament, col = tournament)) +
              geom_boxplot() + 
              facet_grid(~Tour)

ggplot(court, aes(y=ave_ue / ave_sets, group = tournament,
                  col = tournament)) +
    geom_boxplot() + 
    facet_grid(~Tour)

ggplot(court, aes(y=n_retire, group = tournament,
                  col = tournament)) +
    geom_boxplot() + 
    facet_grid(~Tour)

ggplot(court, aes(x = year,y=ave_sets, col = tournament)) +
    geom_point() + facet_grid(~Tour)


## Completely missing 2015 WTA data
table(gs_partial_pbp$year)

## Shows data from PBP to year to year
table(gs_partial_pbp$year, gs_partial_pbp$round) /
    table(gs$year, gs$round)

table(gs_partial_pbp$year, gs_partial_pbp$round,
      gs_partial_pbp$Tour, gs_partial_pbp$tournament)
## Missing 3 women wimbledon finals, 2 women us open, 1 fo, 1 aus (2015 has zero womens recorded matches)
## 1 mens us open, 1 aus 2014, 2013

###
nadal <- gs_partial_pbp %>% 
