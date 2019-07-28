## Script to get Federer individual model
## SKG July 20, 2019


devtools::load_all("~/courtsports")
data(gs_partial_players)

player_name <- "Novak Djokovic"
data <- gs_partial_players
ref <- "French Open"
start_lower <- TRUE
min_year <- 2013
max_year <- 2017
seed <- 2020
test_prop <- 0
#
out_list <- courtsports::model_individual(
                              player_name = player_name,
                              data = data,
                              ref = ref,
                              start_lower = start_lower,
                              test_prop = test_prop,
                              min_year = min_year,
                              max_year = max_year,
                              seed = seed)
#
summary(out_list$final_model)
par(mfrow = c(2,2))
plot(out_list$final_model)
print(out_list$n_obs)

