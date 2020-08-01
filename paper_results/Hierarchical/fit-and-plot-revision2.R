library(courtsports)
library(tidyverse)
library(broom)
library(lme4)
library(dplyr)

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

gs_partial_players %>%
  mutate(
    name_int = as.integer(as.factor(name)),
    name_fac = as.factor(name),
    ioc_fac = as.factor(ioc),
    atp = (Tour == "atp"),
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
                                   age + (0 + tournament |name_fac),
                                 data = gs_players, family = "binomial", nAGQ =0)

n_aces_mod = lme4::lmer(n_aces ~ late_round + log(rank) + log(opponent_rank) +
                          year_fac + atp + (0 + tournament |name_fac),
                        data = gs_partial_players)

aces_new = lme4::glmer(cbind(n_aces, total_points - n_aces) ~ late_round + log(rank) + log(opponent_rank) + age + atp + 
                        (0 + tournament |name_fac),
                      data = gs_partial_players, family = "binomial", nAGQ = 0)

n_net_mod = lme4::lmer(n_netpt_w ~ late_round + log(rank) + log(opponent_rank) +
                         year_fac + atp + (0 + tournament |name_fac),
                       data = gs_partial_players, control = lmerControl(optimizer = "bobyqa"))

nets_new = lme4::glmer(cbind(n_netpt_w, total_points - n_netpt_w) ~ late_round + log(rank) + log(opponent_rank) + age + atp + 
                         (0 + tournament |name_fac),
                       data = gs_partial_players, family = "binomial", nAGQ = 0)

n_ue_mod = lmer(n_ue ~ late_round + log(rank) + log(opponent_rank) +
                  year_fac + atp + (0 + tournament |name_fac),
                data = gs_partial_players, control = lmerControl(optimizer = "bobyqa"))

ue_new = lme4::glmer(cbind(n_ue, total_points - n_ue) ~ late_round + log(rank) + log(opponent_rank) + age + atp + 
                         (0 + tournament |name_fac),
                       data = gs_partial_players, family = "binomial", nAGQ = 0)


 fixed.effects.plot = tidy(logistic_newmod) %>%
   filter(group == "fixed") %>%
   mutate(
     name = case_when(
       term == "late_roundFALSE" ~ "Late Round (F)",
       term == "late_roundTRUE" ~ "Late Round (T)",
       term == "log(rank)" ~ "Rank (log)",
       term == "log(opponent_rank)" ~ "Opp. Rank (log)",
       term == "age" ~ "Age"
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
 
 logistic = function(x) return(1/(1+exp(-x)))
 
 as.data.frame(ranef(logistic_newmod, condVar = TRUE)) %>%
   mutate(tournament = gsub("tournament", "", term)) %>%
   filter(grp %in% more_players) %>%
   ggplot(., aes(x = grp, y = condval, ymin = condval- 2*condsd, ymax = condval + 2*condsd, color = tournament)) +
   geom_errorbar(width = .5, position = "dodge",size = 1.5) +
   scale_color_manual("", values=tournament_colors) +
   gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) + 
   my_theme +
   coord_flip() +
   labs(title = "Logistic Regression Coefficients",
        subtitle = "Random Player Effect",
        xlab = " ",
        ylab = "Coefficient") +
   xlab("") +
   ylab("Coefficient") +
   theme(legend.position = "bottom") +
   guides(col = guide_legend(nrow=1,
                             override.aes = list(size = 7))) +
   geom_hline(yintercept = 0, color = "grey") +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) 
 
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
      term == "age" ~ "Age",
      term == "atpTRUE" ~ "ATP"
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
  geom_hline(yintercept = 0, color = "grey") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

more_players = c(
  "Roger Federer",
  "Andy Murray",
  "David Ferrer",
  "Fernando Verdasco",
  "Grigor Dimitrov",
  "Kei Nishikori",
  "Kevin Anderson",
  "Marin Cilic",
  "Nick Kyrgios",
  "Novak Djokovic",
  "Rafael Nadal",
  "Roberto Bautista Agut",
  "Sam Querrey",
  "Agnieszka Radwanska",
  "Angelique Kerber",
  "Petra Kvitova",
  "Serena Williams",
  "Venus Williams",
  "Victoria Azarenka"
)

ranef_ue_many = as.data.frame(ranef(ue_new, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  filter(grp %in% more_players) %>%
  ggplot(., aes(x = grp, y = condval, ymin = condval- 2*condsd, ymax = condval + 2*condsd, color = tournament)) +
  geom_errorbar(width = .5, position = "dodge",size = 1.5) +
  xlab("") +
  ylab("Coefficient") +
  scale_color_manual("", values=tournament_colors) +
  gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) + 
  my_theme +
  coord_flip() +
  labs(title = "Unforced Errors",
       subtitle = "Random Player Effect") +
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
  gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) + 
  my_theme +
  coord_flip() +
  labs(title = "Aces",
       subtitle = "Random Player Effect") +
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
  gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) + 
  my_theme +
  coord_flip() +
  labs(title = "Points won at net",
       subtitle = "Random Player Effect") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow=1,
                            override.aes = list(size = 7))) +
  geom_hline(yintercept = 0, color = "grey") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank())

ggpubr::ggarrange(ranef_aces_many, ranef_net_many, ranef_ue_many,
                  nrow =1,
                  ncol = 3,
                  common.legend = TRUE,
                  legend="bottom",
                  widths = c(.45, .27, .27))

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

## Appendix things

tidy(logistic_newmod) %>%
  filter(group == "fixed") %>%
  dplyr::select(-group) %>%
  knitr::kable(format = "latex", digits = 3)

tidy(aces_new) %>%
  filter(group == "fixed") %>%
  dplyr::select(-group) %>%
  knitr::kable(format = "latex", digits = 3)

tidy(nets_new) %>%
  filter(group == "fixed") %>%
  dplyr::select(-group) %>%
  knitr::kable(format = "latex", digits = 3)

tidy(ue_new) %>%
  filter(group == "fixed") %>%
  dplyr::select(-group) %>%
  knitr::kable(format = "latex", digits = 3)
