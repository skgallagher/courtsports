## August 21, 2020
## Script to get number of matches of these three between 2013-2019


devtools::load_all()
library(dplyr)
data(gs_players_v2)
    
goat <- gs_players_v2 %>%
    filter(name %in% c("Serena Williams",
                       "Roger Federer",
                       "Rafael Nadal"))

tab <- goat %>% group_by(name,
                  tournament) %>%
    summarize(n_played = n())

tab %>% 
    print(n = 100)

tab %>% group_by(name) %>%
    summarize(n = sum(n_played))

data(wta_matches)
table(gs_partial_v2$league, gs_partial_v2$year)
