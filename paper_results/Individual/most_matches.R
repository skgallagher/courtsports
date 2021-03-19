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
my_theme <-  theme_bw() + # White background, black and white theme
  theme(axis.text = element_text(size = 24),
        text = element_text(size = 28,
                            family="serif"),
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
summary(best_mod)$r.squared
summary(best_mod)$adj.r.squared
print(r2)
par(mfrow = c(2,2))
plot(out_list$final_model)
print(out_list$n_obs)
df <- broom::tidy(out_list$final_model)
df <- df %>%
  mutate(OR = exp(estimate),
         lower = exp(estimate - 1.96 * std.error),
         upper = exp(estimate + 1.96 * std.error))
kable(df %>%
        select(term, OR, lower, upper,
               p.value), format = "latex",
      row.names = FALSE, col.names = c("Coef.", "Odds Ratio",
                                       "Lower 95% CI",
                                       "Upper 95% CI",
                                       "p-value"),
             caption = "\\label{tab:fed-ind-coef} Modeling coefficients for Nadal's best fit individual model using the French Open as a reference court.",
      booktabs = TRUE, digits = 4) %>% kable_styling(latex_options = "striped", "hold_position")
car::vif(out_list$final_model)

## QUANTILES
out_list$data_sub %>%
  summarize_if(is.numeric, quantile, na.rm = TRUE, probs = .25)
out_list$data_sub %>%
  summarize_if(is.numeric, quantile, na.rm = TRUE, probs = .5)
out_list$data_sub %>%
  summarize_if(is.numeric, quantile, na.rm = TRUE, probs = .75)

## Intrepretations of coefficients
coefs <- out_list$final_model$coefficients
cov_f <- vcov(out_list$final_model)
abs(coefs["log(opponent_rank)"])
coefs["log(opponent_rank)"]
sqrt(cov_f["log(opponent_rank)", "log(opponent_rank)"])
## break points
.1 * coefs["pct_bp"]
.1 *sqrt(cov_f["pct_bp", "pct_bp"])
## interactions
cov_mat <- vcov(out_list$final_model)
## AO
aus_vars <- c("I(court)Australian Open", "I(court)Australian Open:wue_ratio")
cov_AO <- cov_mat[aus_vars, aus_vars]
coefs[aus_vars[1]]
coefs[aus_vars[2]]
## FO
aus_vars <- c("I(court)French Open", "I(court)French Open:wue_ratio")
cov_AO <- cov_mat[aus_vars, aus_vars]
coefs[aus_vars[1]]
coefs[aus_vars[2]]
## USO
aus_vars <- c("I(court)US Open", "I(court)US Open:wue_ratio")
cov_AO <- cov_mat[aus_vars, aus_vars]
coefs[aus_vars[1]]
coefs[aus_vars[2]]


## plot diags
my_theme <-  theme_bw(base_size = 20) + # White background, black and white theme
  theme(text = element_text(family="serif"))
g <- ggDiagnose(out_list$final_model, which = 1:6, return = TRUE)
ggs <- g$ggout
ggthm <- lapply(ggs, function(gg) gg + my_theme)
pdf("../plots/fed_diags.pdf", width = 16, height = 10)
do.call("grid.arrange", c(ggthm, ncol = 3))
dev.off()

## outliers
fed_df <- out_list$final_model$data
fed_df[c(31,40,94),]
sort(fed_df$wue, de = FALSE)
ind <- which(fed_df$pct_bp == 0)
fed_df[ind,]
fed_df[which(fed_df$wue >= 5), ]

mod2 <- lm(out_list$final_model$formula, data = fed_df[-c(38,66,48),])
summary(mod2)


## interaction effects


delta_sd <- function(b, m, sigma){
    sbb <- sigma[1,1]
    smm <- sigma[2,2]
    sbm <- sigma[1,2]
    sqrt(sbb / m^2 - sbm * (1 + b) / m^3 + b * smm / m^4)
}

## Aus open
cov_mat <- vcov(out_list$final_model)
aus_vars <- c("I(court)Australian Open", "I(court)Australian Open:wue_ratio")
cov_AO <- cov_mat[aus_vars, aus_vars]
coefs[aus_vars[1]]
coefs[aus_vars[2]]
m <- df$estimate[10]
sd_AO <- sqrt(sum(cov_mat[c(2,10), c(2,10)]))
h <- c(1 / m, b / m^2)
d_AO <- (h %*% cov_AO) %*% h
df$estimate[2] / df$estimate[10]
2 * d_AO
## French
sd_FO <- sqrt(sum(cov_mat[c(3,11), c(3,11)]))
cov_FO <- cov_mat[c(3,11), c(3, 11)]
df$estimate[3] + df$estimate[11] + 2 * c(-1, 0, 1) * sd_FO
df$estimate[3] / df$estimate[11]
b <- df$estimate[3]
m <- df$estimate[11]
sd_AO <- sqrt(sum(cov_mat[c(3,11), c(3,11)]))
h <- c(1 / m, b / m^2)
d_FO <- (h %*% cov_FO) %*% h
df$estimate[3] / df$estimate[11]
d_FO

## US
cov_UO <- cov_mat[c(4,12), c(2, 12)]
sd_UO <- sqrt(sum(cov_mat[c(4,12), c(4,12)]))
df$estimate[4] + df$estimate[12] +  2 * c(-1, 0, 1) * sd_UO
df$estimate[4] / df$estimate[12]
b <- df$estimate[4]
m <- df$estimate[12]
sd_UO <- sqrt(sum(cov_mat[c(4,12), c(4,12)]))
h <- c(1 / m, b / m^2)
d_UO <- (h %*% cov_UO) %*% h
df$estimate[4] / df$estimate[12]
2 * d_UO



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
                              max_year = 2020,
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


inds <- with(sum_df, which(n_obs / n_cov >= 2 & max_vif <= 10))
out <- sum_df[inds,]
out <- out[order(out$tour), c("name", "tour",
                              "n_cov", "n_obs",
                              "R2", "max_vif")]
out$tour <- ifelse(out$tour == "atp", "ATP",
                   ifelse(out$tour == "wta", "WTA", out$tour))

kable(out,
               format = "latex",
      row.names = FALSE, col.names = c("Player", "Tour", "\\# Coef.",
                                       "\\# Obs.",
                                       "Adj. $R^2$",
                                       "Max VIF"),
             caption = "\\label{tab:ind-sum}Summary of best-fit individual models.",
      booktabs = TRUE, digits = 2, escape = FALSE, linesep = "") %>%
  kable_styling(latex_options = "striped", "hold_position")


player_df <- do.call('rbind', player_list[inds])

##########################################
## A
#########################################
player_df$var_pretty <- gsub("I\\(court\\)", "", player_df$var)
player_df$slam <- gsub(":.*", "", player_df$var_pretty)
sub_player_df <- player_df
sub_player_df$slam <- ifelse(sub_player_df$slam %in% c("Australian Open",
                                                       "US Open",
                                                       "Wimbledon"),
                             sub_player_df$slam, NA)
sub_player_df <- na.omit(sub_player_df)
player <- sub_player_df %>% group_by(name, tour, slam) %>%
    summarize(npos = sum(pos == 1), nneg = sum(pos == -1))

effects <- player %>% group_by(slam, tour) %>%
    summarize(npos = sum(npos), nneg = sum(nneg))


#############################################
## B
###############################

L <- nrow(top_player_names)
player_list <- vector(mode = "list", length = L)
slams <- c("Australian Open", "French Open", "US Open", "Wimbledon")
quant <-  .5
pred_df <- top_players %>% group_by(name) %>%
    filter(name %in% top_players$name) %>%
    summarize_if(is.numeric, quantile, na.rm = TRUE, probs = quant)

## rank difference
gs_partial_players_t %>%
    filter(name %in% top_players$name)  %>%
    group_by(name) %>%
    summarize( diff = (max(rank, na.rm = TRUE) - min(rank, na.rm = TRUE))) %>%
    filter(diff >= 50)

pred_list <- vector(mode = "list", length = L)
for(ll in 1:L){
    ii <- ll
    player_name <- top_player_names$name[ii]
    if(player_name == "Roger Federer"){
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
    df1 <-  df %>% summarize_if(is.numeric, quantile, na.rm = TRUE, probs = .25)
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
    df$opponent_rank <- 10
    df$player_name <- player_name
    if(player_name == "Roger Federer"){
        print(kable(df[c(1, 5, 9), c("player_name", "ave_serve_speed",
                               "pct_ace", "wue_ratio",
                               "pct_netpt", "quant")],
               format = "latex",
      row.names = FALSE, col.names = c("Player", "Ave. serve speed.", "% Aces",
                                       "W/UE", "% Net points won",
                                       "Quartile"),
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


library(extrafont)
loadfonts()

my_theme <-  theme_bw(base_size = 20) + # White background, black and white theme
    theme(text = element_text(family="FreeSerif"),
          strip.text = element_text(size = 12))
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

## FED QUARTILE PRED
fed <- ggdf


## 0
###################3
## Make above graph prettier
################3
## A
## make df with player | surface |# pos interaction |# neg interaction columns
## look at correlation between the two (# pos and neg)
## do by different surface
############################
## B
## Expected percent of points won at each slam for IQR of stats
#############################3
## C
## Make df with player_name | coef 1 | .. coeff P
## look at correlation matrix


## TODO
## diags for federer DONE
## and interpretation of model DONE
## table of # of observations, # of covariates, R^2, and VIF DONE
## rewrite interps of cool graphs DONE
## add how this work will transform tennis forever DONE
## Explain about different styles of play and emphasize court differences in
## discussion/intro DONE
## variable discussion and statistics ugh DONE
## R package maintenace// vignette mostly to get EDA, hiearchical model, and individual models
