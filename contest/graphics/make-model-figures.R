library(courtsports)
library(tidyr)
library(broom)
library(lme4)
library(ggpubr)
library(gridExtra)

load("../../data/gs_players.rda")
load("../../data/gs_partial_players.rda")
load("../../data/nocourt_logistic.rda")
load("../../data/base_logistic.rda")
load("../../data/country_logistic.rda")
load("../../data/ind_int_logistic.rda")
load("../../data/ind_logistic.rda")
load("../../data/ind_logistic_noioc.rda")
load("../../data/ind_year_logistic.rda")

load("../../data/n_aces_mod.rda")
load("../../data/n_net_mod.rda")
load("../../data/n_ue_mod.rda")
load("../../data/n_winners_mod.rda")

my_theme <-  theme_bw() + # White background, black and white theme
  theme(axis.text = element_text(size = 12),
        text = element_text(size = 14,
                            family="serif"),
        plot.title = element_text(hjust = 0.5, size=16))

tournament_colors <- c("#0297DB", "#b06835", "#0C2340", "#54008b")
league_colors <- c("#0C2340", "#902BA3")
win_colors <- c("#336699", "#339966")

# with yellow for us
tournament_colors <- c("#0297DB", "#b06835", "#ffe500", "#54008b")

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

fixed.effects.plot = model.ests %>%
  filter(., !grepl("ioc", term)) %>% 
  filter(., model == 'ind_logistic_noioc') %>%
  ggplot(., aes(x = term, y = estimate, col = model, ymin = estimate - 2*std.error, ymax = estimate + 2*std.error)) +
  geom_errorbar(width = .7, position = position_dodge(), size = 1.5) +
  my_theme +
  # coord_flip() +
  theme(axis.text.x=element_text(angle = 35, hjust = 1, size = 10)) +
  ggtitle("Fixed effects") +
  scale_color_brewer("",palette = "Dark2") +
  theme(legend.position = "none") + 
  xlab("") +
  ylab("Estimate") +
  scale_x_discrete("", breaks = waiver(), labels = c("Intercept", "ATP", "Late round", "Op Rank (log)", "Rank (log)", "2014", "2015", "2016", "2017"))+
  guides(col = guide_legend(nrow=2))

ran_tourn = rbind(ranef(ind_logistic)$name_int, ranef(ind_logistic_noioc)$name_int)
ran_tourn$model = c(rep("ind_logistic", nrow(ranef(ind_logistic)$name_int)), rep("ind_logistic_noioc", nrow(ranef(ind_logistic)$name_int)))

gs_players$name_int = as.integer(as.factor(gs_players$name))
name_lookup = unique(cbind(gs_players$name, gs_players$name_int))
name_lookup = name_lookup[order(as.integer(name_lookup[,2])),]
top_three_ranef = as.data.frame(ranef(ind_logistic_noioc, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  mutate(name = name_lookup[,1][as.integer(grp)]) %>% 
  filter(., name == "Serena Williams" | name == "Rafael Nadal" | name == "Roger Federer") %>%
  ggplot(., aes(x = name, y = condval, ymin = condval- 2*condsd, ymax = condval + 2*condsd, color = tournament)) +
  geom_errorbar(width = .5, position = "dodge",size = 1.5) + 
  xlab("") + 
  ylab("Estimate") + 
  scale_color_manual("", values=tournament_colors) +
  my_theme +
  coord_flip() +
  ggtitle("Random Effects") +
  theme(legend.position = "bottom") + 
  guides(col = guide_legend(nrow=2)) + 
  theme(legend.text=element_text(size=8))

grid.arrange(fixed.effects.plot, top_three_ranef, nrow = 1, widths = c(.4, .6),
             top = textGrob("Parameter estimates from logistic model",
                            gp=gpar(fontsize=20, fontfamily="serif")))

fixed.effects.plot.allmods = model.ests %>%
  filter(., !grepl("ioc", term)) %>% 
  ggplot(., aes(x = term, y = estimate, col = model, ymin = estimate - 2*std.error, ymax = estimate + 2*std.error)) +
  geom_errorbar(width = .7, position = position_dodge(), size = 1.5) +
  my_theme +
  # coord_flip() +
  theme(axis.text.x=element_text(angle = 35, hjust = 1, size = 10)) +
  ggtitle("Fixed effects") +
  scale_color_brewer("",palette = "Dark2") +
  theme(legend.position = "none") + 
  xlab("") +
  ylab("Estimate") +
  scale_x_discrete("", 
                   breaks = waiver(), 
                   labels = c("Intercept", "ATP", "Late round", "Op Rank (log)", "Rank (log)", "French", "US", "Wimbledon", "2014", "2015", "2016", "2017")) +
  guides(col = guide_legend(nrow=2))

aces_ranef = as.data.frame(ranef(n_aces_mod, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  filter(., grp == "Serena Williams" | grp == "Rafael Nadal" | grp == "Roger Federer") %>%
  ggplot(., aes(x = as.character(grp), y = condval, ymin = condval- 2*condsd, ymax = condval + 2*condsd, col = tournament)) +
  geom_errorbar(height = .5, position = position_dodge(), size = 1.5) +
  ylab("Estimate") + 
  xlab("") +
  ggtitle("Aces") + 
  scale_color_manual("", values=tournament_colors) +
  my_theme +
  coord_flip() +
  theme(legend.position = "none")

nets_ranef = as.data.frame(ranef(n_net_mod, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  filter(., grp == "Serena Williams" | grp == "Rafael Nadal" | grp == "Roger Federer") %>%
  ggplot(., aes(x = as.character(grp), y = condval, ymin = condval- 2*condsd, ymax = condval + 2*condsd, col = tournament)) +
  geom_errorbar(height = .5, position = position_dodge(), size = 1.5) +
  ylab("Estimate") + 
  xlab("") +
  ggtitle("Net Wins") + 
  scale_color_manual("", values=tournament_colors) +
  my_theme +
  theme(axis.text.y = element_blank()) +
  coord_flip() # +
# theme(legend.position = "none")

ue_ranef = as.data.frame(ranef(n_ue_mod, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  filter(., grp == "Serena Williams" | grp == "Rafael Nadal" | grp == "Roger Federer") %>%
  ggplot(., aes(x = as.character(grp), y = condval, ymin = condval- 2*condsd, ymax = condval + 2*condsd, col = tournament)) +
  geom_errorbar(height = .5, position = position_dodge(), size = 1.5) +
  ylab("Estimate") + 
  xlab("") +
  ggtitle("Unforced Errors") + 
  scale_color_manual("", values=tournament_colors) +
  my_theme +
  theme(axis.text.y = element_blank()) +
  coord_flip()# + 
#theme(legend.text=element_text(size=10))

#grid.arrange(aces_ranef, nets_ranef, ue_ranef, nrow = 1, widths = c(.38, .22, .45))

fixed.effects.plot.linear <- tidy(n_aces_mod) %>%
  filter(., group == 'fixed') %>%
  mutate(., model = "Aces") %>%
  bind_rows(
    tidy(n_ue_mod) %>%
      filter(., group == 'fixed') %>%
      mutate(., model = "UE")
  ) %>%
  bind_rows(
    tidy(n_net_mod) %>%
      filter(., group == 'fixed') %>%
      mutate(., model = "Net")
  ) %>%
  ggplot(., aes(x = term, y = estimate, 
                ymin = estimate - 2*std.error, 
                ymax = estimate + 2*std.error,
                col = model,
                group = model)) + 
  geom_errorbar(width = .7, position = position_dodge(), size = 1.5) +
  my_theme +
  # coord_flip() +
  theme(axis.text.x=element_text(angle = 35, hjust = 1, size = 10)) +
  ggtitle("Fixed effects") +
  scale_color_brewer("",palette = "Dark2") +
  theme(legend.position = "bottom") + 
  xlab("") +
  ylab("Estimate") +
  scale_x_discrete("", 
                   breaks = waiver(), 
                   labels = c("Intercept", "ATP", "Late round", "Op Rank (log)", "Rank (log)", "2014", "2015", "2016", "2017"))


pdf(file="../graphics/model-figures/linear-fixed-effects.pdf",width=6,height=4)
fixed.effects.plot.linear
dev.off()

pdf(file="../graphics/model-figures/linear-player-effects.pdf",width=6,height=4)
ggarrange(aces_ranef, nets_ranef, ue_ranef, nrow=1, ncol=3,
          common.legend = TRUE, legend="bottom", widths = c(.45, .27, .27)) %>%
  grid.arrange(top = textGrob("Player-level effects",
                              gp=gpar(fontsize=20, fontfamily="serif")))
dev.off()

pdf(file="../graphics/model-figures/logistic-parameter-effects.pdf",width=6,height=4)
grid.arrange(fixed.effects.plot, top_three_ranef, nrow = 1, widths = c(.4, .6),
             top = textGrob("Parameter estimates from logistic model",
                            gp=gpar(fontsize=20, fontfamily="serif")))
dev.off()

pdf(file="../graphics/model-figures/all-mods-fixed-effects.pdf",width=6,height=4)
grid.arrange(fixed.effects.plot.allmods)
dev.off()

