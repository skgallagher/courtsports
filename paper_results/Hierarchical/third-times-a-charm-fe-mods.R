aces_fe = lme4::glmer(cbind(n_aces, total_points - n_aces) ~ late_round + log(rank) + log(opponent_rank) + scale(age) +  atp + tournament + 
                        (0 + tournament |name_fac),
                      data = gs_partial_players, family = "binomial", nAGQ = 0)

nets_fe = lme4::glmer(cbind(n_netpt_w, total_points - n_netpt_w) ~ late_round + log(rank) + log(opponent_rank) + scale(age) +  atp + tournament +
                        (0 + tournament |name_fac),
                      data = gs_partial_players, family = "binomial", nAGQ = 0)

ue_fe = lme4::glmer(cbind(n_ue, total_points - n_ue) ~ late_round + log(rank) + log(opponent_rank) + scale(age) +  atp + tournament + 
                      (0 + tournament |name_fac),
                    data = gs_partial_players, family = "binomial", nAGQ = 0)

logistic = function(x) return(1/(1+exp(-x)))


fixef_3 = tidy(aces_fe) %>%
  filter(., effect == 'fixed') %>%
  mutate(., model = "Aces") %>%
  bind_rows(
    tidy(ue_fe) %>%
      filter(., effect == 'fixed') %>%
      mutate(., model = "UE")
  ) %>%
  bind_rows(
    tidy(nets_fe) %>%
      filter(., effect == 'fixed') %>%
      mutate(., model = "Net")
  ) %>%
  mutate(
    intercept = ifelse(term == "(Intercept)", TRUE, FALSE)
  ) %>%
  ggplot(., aes(y = term, x = estimate,
                xmin = estimate - 2*std.error,
                xmax = estimate + 2*std.error,
                col = model,
                group = model)) +
  geom_errorbar(width = .7, position = position_dodge(), size = 1.5) +
  my_theme +
  ggtitle("Fixed effects") +
  scale_color_brewer("",palette = "Dark2") +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("Coefficient") +
  guides(color = guide_legend(override.aes = list(size = 7)))  +
  geom_vline(xintercept = 0, color = "grey") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())

fixef_3 + ggforce::facet_col(vars(intercept), scales = 'free', space = 'free')


ranef_ue_many = as.data.frame(ranef(ue_fe, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  filter(grp %in% more_players) %>%
  ggplot(., aes(x = grp, y = exp(condval), ymin = exp(condval- 2*condsd), ymax = exp(condval + 2*condsd), color = tournament)) +
  geom_errorbar(width = .5, position = "dodge",size = 1.5) +
  xlab("") +
  ylab("Log Odds") +
  scale_color_manual("", values=tournament_colors) +
  #gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) +
  my_theme +
  coord_flip() +
  labs(subtitle = "Unforced Errors") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow=1,
                            override.aes = list(size = 7))) +
  geom_hline(yintercept = 1, color = "grey") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank()) +
  scale_y_log10()

ranef_aces_many = as.data.frame(ranef(aces_fe, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  filter(grp %in% more_players) %>%
  ggplot(., aes(x = grp, y = exp(condval), ymin = exp(condval- 2*condsd), ymax = exp(condval + 2*condsd), color = tournament)) +
  geom_errorbar(width = .5, position = "dodge",size = 1.5) +
  xlab("") +
  ylab("Log Odds") +
  scale_color_manual("", values=tournament_colors) +
  #gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) +
  my_theme +
  coord_flip() +
  labs(subtitle = "Aces") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow=1,
                            override.aes = list(size = 7))) +
  geom_hline(yintercept = 1, color = "grey") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_log10()

ranef_net_many = as.data.frame(ranef(nets_fe, condVar = TRUE)) %>%
  mutate(tournament = gsub("tournament", "", term)) %>%
  filter(grp %in% more_players) %>%
  ggplot(., aes(x = grp,
                y = exp(condval),
                ymin = exp(condval- 2*condsd),
                ymax = exp(condval + 2*condsd),
                color = tournament)) +
  geom_errorbar(width = .5, position = "dodge",size = 1.5) +
  xlab("") +
  ylab("Log Odds") +
  scale_color_manual("", values=tournament_colors) +
  #gghighlight::gghighlight((condval- 2*condsd > 0) | (condval + 2*condsd < 0)) +
  my_theme +
  coord_flip() +
  labs(subtitle = "Net Points Won") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow=1,
                            override.aes = list(size = 7))) +
  geom_hline(yintercept = 1, color = "grey") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank())

ranef_aces_many + ranef_net_many + ranef_ue_many +
  plot_annotation(title = 'Random Player Effects',
                  theme = my_theme) +
  plot_layout(widths = 1, guides = "collect") & theme(legend.position = 'bottom')
