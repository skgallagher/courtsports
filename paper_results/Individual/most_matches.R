## Script to get Federer individual model
## SKG July 20, 2019

# devtools::install_github("skoval/deuce")
# devtools::install_github("skgallagher/courtsports")

##library("courtsports")
devtools::load_all()
library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(gridExtra)
library(knitr)
library(broom)
library(kableExtra)
# devtools::install_github("benjaminleroy/ggDiagnose")
#library(ggDiagnose)


data(gs_partial_players_v2)
########################################################################
## THeme
#library(extrafont)
#font_import()
my_theme <-  theme_bw() + # White background, black and white theme
  theme(axis.text = element_text(size = 24),
        text = element_text(size = 28,
                            family="serif"), #"FreeSerif"
        plot.title = element_text(hjust = 0.5, size=34),
        plot.subtitle = element_text(hjust = 0.5))

my_theme <-  theme_bw(base_size = 18) + # White background, black and white theme
  theme(axis.text = element_text(size = 12),
        text = element_text(size = 14,
                            family="serif"),
        plot.title = element_text( size=16))

tournament_colors <- c("#0297DB", "#b06835", "#0C2340", "#54008b")
league_colors <- c("#0C2340", "#902BA3")
win_colors <- c("#336699", "#339966")

# with yellow for us
tournament_colors <- c("#0297DB", "#b06835", "#ffe500", "#54008b")

graphic_width_long <- 16
graphic_width_short <- 12

# write/save graphs locally?
save_graph <- FALSE
###########################################################################

player_name <- "Rafael Nadal"
data <- gs_partial_players_v2
ref <- "French Open"
start_lower <- FALSE
min_year <- 2013
max_year <- 2020
seed <- 2020
set.seed(seed)
test_prop <- 0.5
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
r2 <- with(summary(out_list$final_model), 1 - deviance/null.deviance)
## double check
best_mod <- glm(out_list$step_model$formula,
               data = out_list$data_sub[-out_list$train_inds,],
               family = binomial,
               weights = total_points)

#######################################3
## The rest
data(gs_players_v2)
top_player_names <- gs_players %>%
  group_by(name, league) %>% tally() %>%
  arrange(desc(n)) %>%
  group_by(league) %>%
  top_n(n = 10, wt = n) %>%
  arrange(league) %>%
  rename(Tour = league)
usethis::use_data(top_player_names, overwrite = TRUE)


data(gs_partial_players_v2)
gs_partial_players_v2 <- gs_partial_players_v2 %>%
  rename(Tour = league)

with(gs_partial_players_v2, table(year, tournament, Tour))


## women is 19 * 2 = (4 missing in 2015, 2 in 2018, 2 in 2019)
## men is 23 * 2
top_players <- gs_partial_players_v2 %>%
  filter(name %in% top_player_names$name[top_player_names$n > 40])

print(top_players, n = 45)

top_player_names <- top_players %>%
  group_by(name, Tour) %>%
  tally() %>%
  filter(n > 40)

## Build a list of data frames
# devtools::load_all("~/courtsports")
L <- nrow(top_player_names %>% filter(n > 40))
player_list <- vector(mode = "list", length = L)
mods <- vector(mode = "list", length = L)
data <- mods <- vector(mode = "list", length = L)
ref <- "French Open"
alpha <- .05
sum_df <- data.frame(name = rep("nm", L),
                     tour = "blah",
                     n_obs = 0,
                     n_cov = 0,
                     R2 = 0,
                     max_vif = 0, stringsAsFactors = FALSE)
for(ii in 1:L){
    player_name <- top_player_names$name[ii]
    print(player_name)
    tour <- top_player_names$Tour[ii]
    data_train <- top_players %>% filter(name == player_name)
    if(nrow(data_train) < 40){
      print("Not enough observations to fit a model")
      next
    }
    out_list <- courtsports::model_individual(
                              player_name = player_name,
                              data = top_players,
                              ref = ref,
                              start_lower = start_lower,
                              test_prop = 0,
                              min_year = 2013,
                              max_year = 2019,
                              seed = seed)
    mod <- out_list$final_model
    mods[[ii]] <- mod
    data[[ii]] <- out_list$data_sub
    pval <-   data.frame(pval = rownames(summary(mod)$coef),
                         val = summary(mod)$coefficients[,4],
                         stringsAsFactors = FALSE)
    ##
    pval_df <- data.frame(pval = names(coef(mod)),
                          stringsAsFactors = FALSE)
    pval_j <- left_join(pval_df, pval, by = "pval")
    df <- data.frame(name = player_name,
                                  tour = tour,
                                  var = names(coef(mod)),
                     val = coef(mod),
                     pos = ifelse(coef(mod) > 0, 1, -1),
                     pos_bonf = ifelse(pval_j$val < alpha / L,
                                ifelse(coef(mod) > 0, 1, -1),
                                NA),
                     se = sqrt(diag(vcov(mod))),
                     pval = pval_j$pval)
    player_list[[ii]] <- df
    ## summary_df
    sum_df$name[ii] <- player_name
    sum_df$n_obs[ii] <- out_list$n_obs
    sum_df$n_cov[ii] <- length(mod$coef)
    mod2 <- lm(mod$formula, data = mod$data)
    vifs <- tryCatch({car::vif(mod2)},
        error = function(e) NA)
    sum_df$max_vif[ii] <- ifelse(!is.na(vifs), max(vifs[,3]), NA)
    sum_df$R2[ii] <- summary(mod2)$adj.r.squared
    sum_df$tour[ii] <- tour
}



L <- nrow(top_player_names)
player_list <- vector(mode = "list", length = L)
slams <- c("Australian Open", "French Open", "US Open", "Wimbledon")
quant <-  .5
pred_df <- top_players %>% group_by(name) %>%
    filter(name %in% top_players$name) %>%
    summarize_if(is.numeric, quantile, na.rm = TRUE, probs = quant)



pred_list <- vector(mode = "list", length = L)
for(ll in 1:L){
    ii <- ll
    player_name <- top_player_names$name[ii]
    if(player_name == "Rafael Nadal"){
      print(ii)
    }
    data_train <- top_players %>% filter(name == player_name)
    if(nrow(data_train) < 40){
      print("Not enough observations to fit a model")
      next
    }
    mod <- mods[[ii]]
    df0 <- data[ii]
    df <- data[[ii]]
    tour <- toupper(df$Tour[1])
    df1 <-  df %>% summarize_if(is.numeric, quantile, na.rm = TRUE,
                                probs = .25)
    df1$quant <-  "Below avg."
    df1 <- rbind(df1, df1, df1, df1)
    df1$court <- slams
    df2 <-  df %>% summarize_if(is.numeric, quantile, na.rm = TRUE, probs = .5)
    df2$quant <-  "Avg."
    df2 <- rbind(df2, df2, df2, df2)
    df2$court <- slams
    df3 <-  df %>% summarize_if(is.numeric, quantile, na.rm = TRUE, probs = .75)
    df3$quant <-  "Above avg."
    df3 <- rbind(df3, df3, df3, df3)
    df3$court <- slams
    df <- rbind(df1, df2, df3)
   # df <- df %>%
   #   mutate_at(.vars = vars(contains("pct")), ~.x * 10)
    df$opponent_rank <- 10
    df$player_name <- player_name
    if(player_name == "Rafael Nadal"){
        print(kable(df[c(1, 5, 9), c("player_name",
                                     "quant",
                                     "pct_bp",
                               "pct_ace", "wue_ratio",
                               "pct_netpt")],
               format = "latex",
      row.names = FALSE, col.names = c("Player",
                                       "Performance Level",
                                       "% Break points won", "% Aces",
                                       "W/UE", "% Net points won"
                                       ),
             caption = "\\label{tab:quart-preds} Data used to predicted expected percent of points won for individual models using different quartiles of predictors.",
      booktabs = TRUE, digits = 2) %>% kable_styling(latex_options = "striped", "hold_position"))
    }
    pred <- predict.glm(mod, newdata = df, se.fit = TRUE)
    pred_df <- as.data.frame(pred)
    pred_df$name <- player_name
    pred_df$court <- df$court
    pred_df$tour <- tour
    pred_df$quant <- df$quant
    pred_list[[ii]] <- pred_df
### data frame summary
}


##library(extrafont)
##loadfonts()

##my_theme <-  theme_bw(base_size = 20) + # White background, black and white theme
 ##   theme(text = element_text(family="FreeSerif"),
 ##         strip.text = element_text(size = 12))
ggdf <- do.call('rbind', pred_list)
ggdf$pred <- boot::inv.logit(ggdf$fit)
ggdf$upr <- with(ggdf, boot::inv.logit(fit + 1.96 * se.fit))
ggdf$lwr <- with(ggdf, boot::inv.logit(fit- 1.96 * se.fit))

ggdf$my_new_facet <- paste(ggdf$tour, "-", ggdf$name)
ggplot(data = ggdf, aes(y = pred, x = quant, col = court, group = court)) +
    facet_wrap(~my_new_facet, scales = "free_y",
               ncol = 3) +
    scale_color_manual(values = tournament_colors, name = "Tournament") +
  geom_hline(yintercept = .5, linetype = "dashed") +
    geom_errorbar(aes(ymin = lwr,
                      ymax = upr), position = position_dodge(width = .5)) +
    geom_point(position = position_dodge(width = .5), size = .5) +
    coord_flip()  +
    my_theme  + theme(legend.position = "bottom") +
    labs(x = "Level of play", y = "Expected % of points won",
         title = "Individual model predictions",
         subtitle = "Opponent rank = 10; reference court = French Open")

if(save_graph){
ggsave("../plots/individual-models-results-mod-sub.pdf", width = 10, height = 14)
}
