library(courtsports)
library(tidyverse)
library(broom)
library(lme4)
library(dplyr)
library(patchwork)

my_theme <-  theme_bw() + # White background, black and white theme
  theme(axis.text = element_text(size = 24),
        text = element_text(size = 28,
                            family="serif"),
        plot.title = element_text(hjust = 0.5, size=34),
        plot.subtitle = element_text(hjust = 0.5))

league_colors <- c("#0C2340", "#902BA3")
win_colors <- c("#336699", "#339966")

# with yellow for us
tournament_colors <- c("#0297DB", "#b06835", "#ffe500", "#54008b")

graphic_width_long <- 16
graphic_width_short <- 12

# write/save graphs locally?
save_graph <- FALSE

load("data/gs_players_v2.rda")
load("data/gs_partial_players_v2.rda")

gs_players <- gs_players_v2
gs_partial_players <- gs_partial_players_v2

## Define variables for modeling
gs_players$name_int = as.integer(as.factor(gs_players$name))
gs_players$name_fac = as.factor(gs_players$name)
gs_players$hand_fac = as.factor(gs_players$hand)
gs_players$ioc_fac = as.factor(gs_players$ioc)
gs_players$atp = gs_players$league == "ATP"
gs_players$year_fac = as.factor(gs_players$year)
gs_players$late_round = gs_players$round >= "R16"
gs_players$seeded = gs_players$rank <= 32
gs_players$opponent_seeded = gs_players$opponent_rank <= 32

atp_players = gs_players$name[gs_players$league == "ATP"]
gs_partial_players %>%
  mutate(
    name_int = as.integer(as.factor(name)),
    name_fac = as.factor(name),
    ioc_fac = as.factor(ioc),
    atp = ifelse(name %in% atp_players, 1, 0),
    year_fac = as.factor(year),
    late_round = round >= "R16",
    seeded = rank <= 32,
    opponent_seeded = opponent_rank <= 32,
    ace_pct = n_aces/total_points
  ) -> gs_partial_players

## Fit models used in paper
set.seed(091418)
ind_logistic_noioc = lme4::glmer(did_win ~ late_round + log(rank) + log(opponent_rank) +
                                   year_fac + atp + (0 + tournament |name_fac),
                                 data = gs_players, family = "binomial", nAGQ =0)

logistic_newmod = lme4::glmer(did_win ~ 0 + late_round + log(rank) + log(opponent_rank) +
                                    scale(age) + (0 + tournament |name_fac),
                                 data = gs_players, family = "binomial", nAGQ =0)

aces_new = lme4::glmer(cbind(n_aces, total_points - n_aces) ~ late_round + log(rank) + log(opponent_rank) + scale(age) + atp +
                        (0 + tournament |name_fac),
                      data = gs_partial_players, family = "binomial", nAGQ = 0)

nets_new = lme4::glmer(cbind(n_netpt_w, total_points - n_netpt_w) ~ late_round + log(rank) + log(opponent_rank) + scale(age) + atp +
                         (0 + tournament |name_fac),
                       data = gs_partial_players, family = "binomial", nAGQ = 0)

ue_new = lme4::glmer(cbind(n_ue, total_points - n_ue) ~ late_round + log(rank) + log(opponent_rank) + scale(age) + atp +
                        (0 + tournament |name_fac),
                       data = gs_partial_players, family = "binomial", nAGQ = 0)

logistic = function(x) return(1/(1+exp(-x)))

more_players = c(
  "Roger Federer",
  "Andy Murray",
  "David Ferrer",
  "Grigor Dimitrov",
  "Kei Nishikori",
  "Kevin Anderson",
  "Marin Cilic",
  "Nick Kyrgios",
  "Novak Djokovic",
  "Stan Wawrinka",
  "John Isner",
  "David Goffin",
  "Rafael Nadal",
  "Angelique Kerber",
  "Sloane Stephens",
  "Simona Halep",
  "Madison Keys",
  "Elina Svitolina",
  "Serena Williams",
  "Venus Williams"
)

more_players_tour = tibble(
  name = more_players,
  tour = c(rep("ATP", 13), rep("WTA", 7))
)

fixed.effects.plot = tidy(logistic_newmod) %>%
   filter(group == "fixed") %>%
   mutate(
     name = case_when(
       term == "late_roundFALSE" ~ "Late Round (F)",
       term == "late_roundTRUE" ~ "Late Round (T)",
       term == "log(rank)" ~ "Rank (log)",
       term == "log(opponent_rank)" ~ "Opp. Rank (log)",
       term == "scale(age)" ~ "Age (scaled)",
       TRUE ~ term
     )
   ) %>%
   ggplot(., aes(x = name,
                 y = estimate,
                 ymin = estimate - 2*std.error,
                 ymax = estimate + 2*std.error)) +
   geom_errorbar(width = .7, position = position_dodge(), size = 1.5) +
   my_theme +
   theme(axis.text.x=element_text(angle = 35, hjust = 1)) +
   ggtitle("Fixed effects") +
   scale_color_brewer("",palette = "Dark2") +
   theme(legend.position = "none") +
   xlab("") +
   ylab("Coefficient") +
   guides(col = guide_legend(nrow=2)) +
   geom_hline(yintercept = 0, color = "grey") +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())

ranef_plot = as.data.frame(ranef(logistic_newmod, condVar = TRUE)) %>%
   mutate(tournament = gsub("tournament", "", term)) %>%
   filter(grp %in% c("Roger Federer", "Rafael Nadal", "Serena Williams")) %>%
   left_join(., more_players_tour, by = c("grp" = "name")) %>%
   ggplot(., aes(x = grp, y = condval, ymin = condval- 2*condsd, ymax = condval + 2*condsd, color = tournament, group = tournament)) +
   geom_errorbar(position = position_dodge(width = .5), size = 1.5) +
   scale_color_manual("", values=tournament_colors) +
   #gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) +
   my_theme +
   coord_flip() +
   labs(title = "Random Effects",
        xlab = " ",
        ylab = "Coefficient") +
   xlab("") +
   ylab("Coefficient") +
   theme(legend.position = "bottom") +
   guides(col = guide_legend(nrow=2,
                             override.aes = list(size = 7))) +
   geom_hline(yintercept = 0, color = "grey") +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())

fixed.effects.plot + ranef_plot + plot_layout(widths = 1)
if(save_graph) ggsave("paper_results/plots/logistic-parameter-effects.jpg", width = 16, height = 8)





logistic_ranef = as.data.frame(ranef(logistic_newmod, condVar = TRUE)) %>%
   mutate(tournament = gsub("tournament", "", term)) %>%
   filter(grp %in% more_players) %>%
   left_join(., more_players_tour, by = c("grp" = "name")) %>%
   ggplot(., aes(x = grp, y = condval, ymin = condval- 2*condsd, ymax = condval + 2*condsd, color = tournament, group = tournament)) +
   geom_errorbar(position = position_dodge(width = .5), size = 1.5) +
   scale_color_manual("", values=tournament_colors) +
   #gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) +
   my_theme +
   coord_flip() +
   labs(title = "Logistic Regression Coefficients",
        subtitle = "Random Player Effects",
        xlab = " ",
        ylab = "Coefficient") +
   xlab("") +
   ylab("Coefficient") +
   theme(legend.position = "bottom") +
   facet_wrap(vars(tour), scales = "free_y", ncol = 2) +
   guides(col = guide_legend(nrow=1,
                             override.aes = list(size = 7))) +
   geom_hline(yintercept = 0, color = "grey") +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())

logistic_ranef
if(save_graph) ggsave("paper_results/plots/logistic-ranef-many.jpg", width = 12, height = 12)


attr(VarCorr(logistic_newmod)$name_fac, "correlation") %>%
  tbl_df %>%
  transmute(
    `Aus. Open` = `tournamentAustralian Open`,
    `French Open` = `tournamentFrench Open`,
    `US Open` = `tournamentUS Open`,
    `Wimbledon` = `tournamentWimbledon`,
    Var2 = c("Aus. Open", "French Open", "US Open", "Wimbledon")
  ) %>%
  tidyr::pivot_longer(-c(Var2), names_to = "Var1", values_to = "value") %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = win_colors[2], high = win_colors[1], mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       space = "Lab", name="Correlation") +
  geom_text(aes(label = round(value, 3), family = "serif", size = 14)) +
  labs(x = "", y = "",
       title = "Correlation Matrix for Random Effects")  +
  my_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

#logistic_ranef / (plot_spacer() + logistic_corr + plot_spacer()) + plot_layout(heights = c(2,1))
if(save_graph) ggsave("paper_results/plots/correlation.jpg", width = 12, height = 6)

tidy(aces_new) %>%
  filter(., group == 'fixed') %>%
  mutate(., model = "Aces") %>%
  bind_rows(
    tidy(ue_new) %>%
      filter(., group == 'fixed') %>%
      mutate(., model = "UE")
  ) %>%
  bind_rows(
    tidy(nets_new) %>%
      filter(., group == 'fixed') %>%
      mutate(., model = "Net")
  ) %>%
  mutate(
    name = case_when(
      term == "(Intercept)" ~ " Intercept",
      term == "late_roundTRUE" ~ "Late Round",
      term == "log(rank)" ~ "Rank (log)",
      term == "log(opponent_rank)" ~ "Opp. Rank (log)",
      term == "scale(age)" ~ "Age (scaled)",
      term == "atp" ~ "ATP",
      TRUE ~ term
    )
    ) %>%
  ggplot(., aes(x = name, y = estimate,
                ymin = estimate - 2*std.error,
                ymax = estimate + 2*std.error,
                col = model,
                group = model)) +
  geom_errorbar(width = .7, position = position_dodge(), size = 1.5) +
  my_theme +
  # coord_flip() +
  theme(axis.text.x=element_text(angle = 35, hjust = 1)) +
  ggtitle("Fixed effects") +
  scale_color_brewer("",palette = "Dark2") +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("Coefficient") +
  guides(color = guide_legend(override.aes = list(size = 7)))  +
  ylim(c(-4.5, 2)) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

if(save_graph) ggsave("paper_results/plots/linear-fixed-effects.jpg", width = 12, height = 8)


ranef_ue_many = as.data.frame(ranef(ue_new, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  filter(grp %in% more_players) %>%
  ggplot(., aes(x = grp, y = condval, ymin = condval- 2*condsd, ymax = condval + 2*condsd, color = tournament)) +
  geom_errorbar(width = .5, position = "dodge",size = 1.5) +
  xlab("") +
  ylab("Coefficient") +
  scale_color_manual("", values=tournament_colors) +
  #gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) +
  my_theme +
  coord_flip() +
  labs(subtitle = "Unforced Errors") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow=1,
                            override.aes = list(size = 7))) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank())

ranef_aces_many = as.data.frame(ranef(aces_new, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  filter(grp %in% more_players) %>%
  ggplot(., aes(x = grp, y = condval, ymin = condval- 2*condsd, ymax = condval + 2*condsd, color = tournament)) +
  geom_errorbar(width = .5, position = "dodge",size = 1.5) +
  xlab("") +
  ylab("Coefficient") +
  scale_color_manual("", values=tournament_colors) +
  #gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) +
  my_theme +
  coord_flip() +
  labs(subtitle = "Aces") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow=1,
                            override.aes = list(size = 7))) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ranef_net_many = as.data.frame(ranef(nets_new, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  filter(grp %in% more_players) %>%
  ggplot(., aes(x = grp,
                y = condval,
                ymin = condval- 2*condsd,
                ymax = condval + 2*condsd,
                color = tournament)) +
  geom_errorbar(width = .5, position = "dodge",size = 1.5) +
  xlab("") +
  ylab("Coefficient") +
  scale_color_manual("", values=tournament_colors) +
  #gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) +
  my_theme +
  coord_flip() +
  labs(subtitle = "Net Points Won") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow=1,
                            override.aes = list(size = 7))) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank())

ranef_aces_many + ranef_net_many + ranef_ue_many +
  plot_annotation(title = 'Random Player Effects',
                  theme = my_theme) +
  plot_layout(widths = 1, guides = "collect") & theme(legend.position = 'bottom')

if(save_graph) ggsave("paper_results/plots/linear-ranef-many.jpg", width = 12, height = 12)

ue_corr = attr(VarCorr(ue_new)$name_fac, "correlation") %>%
  tbl_df %>%
  transmute(
    `Aus. Open` = `tournamentAustralian Open`,
    `French Open` = `tournamentFrench Open`,
    `US Open` = `tournamentUS Open`,
    `Wimbledon` = `tournamentWimbledon`,
    Var2 = c("Aus. Open", "French Open", "US Open", "Wimbledon")
  ) %>%
  tidyr::pivot_longer(-c(Var2), names_to = "Var1", values_to = "value") %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = win_colors[2], high = win_colors[1], mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       space = "Lab", name="Correlation") +
  # labels = c("-1.0", "-0.5", "  0.0", "  0.5", "  1.0")) +
  geom_text(aes(label = round(value, 2), family = "serif", size = 14)) +
  labs(x = "", y = "",
       title = "Y = Unforced Errors")  +
  my_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

aces_corr = attr(VarCorr(aces_new)$name_fac, "correlation") %>%
  tbl_df %>%
  transmute(
    `Aus. Open` = `tournamentAustralian Open`,
    `French Open` = `tournamentFrench Open`,
    `US Open` = `tournamentUS Open`,
    `Wimbledon` = `tournamentWimbledon`,
    Var2 = c("Aus. Open", "French Open", "US Open", "Wimbledon")
  ) %>%
  tidyr::pivot_longer(-c(Var2), names_to = "Var1", values_to = "value") %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = win_colors[2], high = win_colors[1], mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       space = "Lab", name="Correlation") +
  # labels = c("-1.0", "-0.5", "  0.0", "  0.5", "  1.0")) +
  geom_text(aes(label = round(value, 2), family = "serif", size = 14)) +
  labs(x = "", y = "",
       title = "Y = Aces")  +
  my_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

nets_corr = attr(VarCorr(nets_new)$name_fac, "correlation") %>%
  tbl_df %>%
  transmute(
    `Aus. Open` = `tournamentAustralian Open`,
    `French Open` = `tournamentFrench Open`,
    `US Open` = `tournamentUS Open`,
    `Wimbledon` = `tournamentWimbledon`,
    Var2 = c("Aus. Open", "French Open", "US Open", "Wimbledon")
  ) %>%
  tidyr::pivot_longer(-c(Var2), names_to = "Var1", values_to = "value") %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = win_colors[2], high = win_colors[1], mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       space = "Lab", name="Correlation") +
  # labels = c("-1.0", "-0.5", "  0.0", "  0.5", "  1.0")) +
  geom_text(aes(label = round(value, 2), family = "serif", size = 14)) +
  labs(x = "", y = "",
       title = "Y = Points won at net")  +
  my_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

ggpubr::ggarrange(aces_corr, nets_corr, ue_corr,
                  nrow =1,
                  ncol = 3,
                  legend="none")

aces_corr + nets_corr + ue_corr + plot_layout(widths = 1)
ggsave("paper_results/plots/linear-models-corr-matrices.jpg", width = 20, height = 6)


## why is Nadal bad now?

gs_partial_players_v2 %>%
  filter(name == "Rafael Nadal") %>%
  group_by(tournament) %>%
  summarize(avg_n_net = mean(n_netpt_w),
            avg_n_ace = mean(n_aces),
            avg_n_ue = mean(n_ue),
            avg_p_net = mean(n_netpt_w/total_points),
            avg_p_ace = mean(n_aces/total_points),
            avg_p_ue = mean(n_ue/total_points),
            avg_length = mean(total_points))

gs_partial_players_v2 %>%
  filter(name == "Roger Federer") %>%
  group_by(tournament) %>%
  summarize(avg_n_net = mean(n_netpt_w),
            avg_n_ace = mean(n_aces),
            avg_n_ue = mean(n_ue),
            avg_p_net = mean(n_netpt_w/total_points),
            avg_p_ace = mean(n_aces/total_points),
            avg_p_ue = mean(n_ue/total_points),
            avg_length = mean(total_points))

## Appendix things

tidy(logistic_newmod) %>%
  filter(group == "fixed") %>%
  dplyr::select(-group) %>%
  knitr::kable(format = "latex", digits = 3)

tidy(nets_new) %>%
  filter(group == "fixed") %>%
  dplyr::select(-group) %>%
  knitr::kable(format = "latex", digits = 3)

tidy(aces_new) %>%
  filter(group == "fixed") %>%
  dplyr::select(-group) %>%
  knitr::kable(format = "latex", digits = 3)

tidy(ue_new) %>%
  filter(group == "fixed") %>%
  dplyr::select(-group) %>%
  knitr::kable(format = "latex", digits = 3)

## Model Selection

set.seed(091418)
logistic_newmod = lme4::glmer(did_win ~ 0 + late_round + log(rank) + log(opponent_rank) +
                                age + (0 + tournament |name_fac),
                              data = gs_players, family = "binomial", nAGQ =0)
nocourt_logistic = glm(did_win ~ 0 + ioc_fac + late_round + log(rank) + log(opponent_rank) + age,
                       data = gs_players, family = "binomial")
base_logistic = glm(did_win ~ 0 + ioc_fac +  tournament + late_round + log(rank) + log(opponent_rank) + age,
                    data = gs_players, family = "binomial")
country_logistic = lme4::glmer(did_win ~ 0 + late_round + log(rank) + log(opponent_rank) + age + (0 + tournament |ioc_fac),
                               data = gs_players, family = "binomial", nAGQ =0)
ind_logistic =  lme4::glmer(did_win ~  0 + ioc_fac + late_round + log(rank) + log(opponent_rank) + age + (0 + tournament |name_int),
                            data = gs_players, family = "binomial", nAGQ =0)
ind_logistic_noioc =  lme4::glmer(did_win ~ 0 + late_round + log(rank) + log(opponent_rank) + age + (0 + tournament |name_int),
                                  data = gs_players, family = "binomial", nAGQ =0)
ind_int_logistic =  lme4::glmer(did_win ~ 0 + late_round + log(rank) + log(opponent_rank) + tournament + age + (1|name_int),
                                data = gs_players, family = "binomial", nAGQ =0)
ind_year_logistic =  lme4::glmer(did_win ~ 0 + late_round + log(rank) + log(opponent_rank) + tournament + age + (0+year_fac|name_int),
                                 data = gs_players, family = "binomial", nAGQ =0)


model_names = c("nocourt_logistic", "base_logistic", "country_logistic", "ind_logistic",
                "ind_logistic_noioc", "ind_int_logistic", "ind_year_logistic")
model.sums = data.frame(model = model_names, aic = rep(NA, length(model_names)), edf = rep(NA,length(model_names)))
model.ests = data.frame()
ran.effects = data.frame()
for(mod in 1:nrow(model.sums)){
  name = model_names[mod]
  model.sums$aic[mod] = extractAIC(get(name))[2]
  model.sums$edf[mod] = extractAIC(get(name))[1]
  betas = tidy(get(name))
  betas$model = name
  if("group" %in% colnames(betas)){
    model.ests = rbind(model.ests, filter(betas, group == "fixed") %>%
                         dplyr::select(., -c("group")))
  }
  else model.ests = rbind(model.ests, betas)
}

model.sums$name = c("no_court", "no_random_ef", "country_ef", "ind_ef", "ind_no_ioc", "ind_intercept", "ind_year_ef")
model.sums$fixed = c("No country, no tournament",
                     "all",
                     "no country, no tournament",
                     "no tournament",
                     "no country, no tournament",
                     "no country",
                     "no country, no year")
model.sums$random = c("none",
                      "none",
                      "tournament (by country)",
                      "tournament (by individual)",
                      "tournament (by individual)",
                      "intercept (by individual)",
                      "year (by individual)")

knitr::kable(model.sums[,c("name", "fixed", "random", "aic", "edf")], format = "latex",
             row.names = FALSE, col.names = c("Model", "Fixed Effects", "Random Effects", "AIC", "EDF"),
             caption = "\\label{tab:model.sums}AIC summary of the seven logistic regression models fitted. Including individual effects for each grand slam, without including any country effects, leads to the best model fit according to AIC.",
             booktabs = TRUE) %>%
  kableExtra::kable_styling(latex_options = "striped")


model.ests %>%
  filter(., !grepl("ioc", term)) %>%
  ggplot(., aes(x = term, y = estimate, col = model, ymin = estimate - 2*std.error, ymax = estimate + 2*std.error)) +
  geom_errorbar(width = .7, position = position_dodge()) +
  my_theme +
  # coord_flip() +
  theme(axis.text.x=element_text(angle = 30, hjust = 1)) +
  ggtitle("Fixed effects") +
  scale_color_brewer("",palette = "Dark2") +
  theme(legend.position = "bottom", legend.text=element_text(size=8),
        # legend.margin=margin(0, 0, 0, 0),
        legend.margin=margin(t = 0, unit='cm'),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  xlab("") +
  ylab("Estimate") +
  guides(col = guide_legend(nrow=2))


