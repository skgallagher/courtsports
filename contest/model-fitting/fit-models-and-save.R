load("gs_players.rda")
load("gs_partial_players.rda")

gs_players$name_int = as.integer(as.factor(gs_players$name))
gs_players$hand_fac = as.factor(gs_players$hand)
gs_players$ioc_fac = as.factor(gs_players$ioc)
gs_players$atp = gs_players$league == "ATP"
gs_players$year_fac = as.factor(gs_players$year)
gs_players$late_round = gs_players$round >= "R16"
gs_players$seeded = gs_players$rank <= 32
gs_players$opponent_seeded = gs_players$opponent_rank <= 32

set.seed(091418)
nocourt_logistic = glm(did_win ~ ioc_fac + late_round + log(rank) + log(opponent_rank) + year_fac + atp, data = gs_players, family = "binomial")
base_logistic = glm(did_win ~ ioc_fac +  tournament + late_round + log(rank) + log(opponent_rank) + year_fac + atp, data = gs_players, family = "binomial")
country_logistic = glmer(did_win ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |ioc_fac), data = gs_players, family = "binomial")
ind_logistic = glmer(did_win ~  ioc_fac + late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name_int), data = gs_players, family = "binomial")
ind_logistic_noioc = glmer(did_win ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name_int), data = gs_players, family = "binomial")
ind_int_logistic = glmer(did_win ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + tournament + (1|name_int), data = gs_players, family = "binomial")
ind_year_logistic = glmer(did_win ~ late_round + log(rank) + log(opponent_rank) + atp + tournament + (0+year_fac|name_int), data = gs_players, family = "binomial")
n_aces_mod = lmer(n_aces ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name), data = gs_partial_players)
n_winners_mod = lmer(n_winners ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name), data = gs_partial_players)
n_net_mod = lmer(n_netpt_w ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name), data = gs_partial_players)
n_ue_mod = lmer(n_ue ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name), data = gs_partial_players)

devtools::use_data(nocourt_logistic,
                   base_logistic,
                   country_logistic,
                   ind_logistic,
                   ind_logistic_noioc,
                   ind_int_logistic,
                   ind_year_logistic,
                   n_aces_mod,
                   n_winners_mod,
                   n_net_mod,
                   n_ue_mod)
