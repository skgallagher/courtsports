## Script to get Federer individual model
## SKG July 20, 2019




devtools::load_all("~/courtsports")
library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(gridExtra)
library(knitr)
library(broom)
library(kableExtra)
library(ggDiagnose)


data(gs_partial_players)

########################3
## THeme
my_theme <-  theme_bw() + # White background, black and white theme
  theme(axis.text = element_text(size = 12),
        text = element_text(size = 14,
        family="serif"),
        plot.title = element_text( size=16))

tournament_colors <- c("#0297DB", "#b06835", "#0C2340", "#54008b")
league_colors <- c("#0C2340", "#902BA3")
win_colors <- c("#336699", "#339966")

# with yellow for us
tournament_colors <- c("#0297DB", "#b06835", "#ffe500", "#54008b")
########################################

player_name <- "Roger Federer"
data <- gs_partial_players
ref <- "Wimbledon"
start_lower <- TRUE
min_year <- 2013
max_year <- 2017
seed <- 2020
set.seed(seed)
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
df <- tidy(out_list$final_model)
kable(df[,-4], format = "latex",
      row.names = FALSE, col.names = c("Coef.", "Est.",
                                       "Std. Err.", "p-value"),
             caption = "\\label{tab:fed-ind-coef} Modeling coefficients for Federer's best fit individual model using Wimbledon as a reference court.",
      booktabs = TRUE, digits = 4) %>% kable_styling(latex_options = "striped", "hold_position")
car::vif(out_list$final_model)

## plot diags
g <- ggDiagnose(out_list$final_model, which = 1:6, return = TRUE) 
ggs <- g$ggout
ggthm <- lapply(ggs, function(gg) gg + my_theme)
pdf("fed_diags.pdf", width = 16, height = 10)
do.call("grid.arrange", c(ggthm, ncol = 3))
dev.off()

## outliers
fed_df <- out_list$final_model$data
fed_df[c(38,66,48),]
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
cov_AO <- cov_mat[c(2,10), c(2, 10)]
df$estimate[2] + df$estimate[10] + 2 * c(-1, 0, 1) * sd_AO
b <- df$estimate[2]
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
top_players <- gs_partial_players %>% group_by(name, Tour) %>% tally() %>%
    filter(n >= 32 & Tour == "wta" | n >=40 & Tour == "atp")
print(top_players, n = 45)


## Build a list of data frames
devtools::load_all("~/courtsports")
L <- nrow(top_players)
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
    player_name <- top_players$name[ii]
    print(player_name)
    tour <- top_players$Tour[ii]
    out_list <- courtsports::model_individual(
                              player_name = player_name,
                              data = gs_partial_players,
                              ref = ref,
                              start_lower = start_lower,
                              test_prop = test_prop,
                              min_year = min_year,
                              max_year = max_year,
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
      booktabs = TRUE, digits = 2, escape = FALSE) %>% kable_styling(latex_options = "striped", "hold_position")


player_df <- do.call('rbind', player_list[inds])

## Simple player effects graph
gdf <- player_df %>% group_by(tour, var, pos, pos_bonf) %>%
    summarize(times = abs(sum(pos, na.rm = TRUE)))
gdf$var <- as.character(gdf$var)
gdf <- gdf %>% filter(grepl(":", var))
gdf$Tour <-  fct_recode(gdf$tour, WTA = "wta", ATP = "atp")

gdf$var_pretty <- gdf$var
gdf$var_pretty <- gsub("I\\(court\\)", "", gdf$var_pretty)
gdf$slam <- gsub(":.*", "", gdf$var_pretty)
gdf$interaction <- gsub(".*:", "", gdf$var_pretty)
gdf$interaction <- factor(gdf$interaction, labels = c("% Break pts. won",
                                                      "% Net pts. won",
                                                      "W/UE",
                                                      "Ave. serve speed",
                                                      "% Aces"))

library(ggplot2) 
ggplot(data = na.omit(gdf[, -which(colnames(gdf) == "pos_bonf")]),
                      aes(x = interaction,
                       y = times,
                       group = pos,
                       fill = factor(pos))) +
    geom_bar(stat = "identity",
             position = position_dodge(preserve = "single")) +
    facet_wrap(slam~Tour, ncol = 2) +
    coord_flip() +
    scale_fill_manual(values = c("red", "blue"),
                      guide = guide_legend(reverse = TRUE),
                      name = "Effect sign",
                      labels = rev(c("Pos. (+)", "Neg. (-)"))) +
    labs(title = "Number of times selected and sign of coefficient",
         subtitle = paste0("In final individual model for top players; reference court: ",
                          ref),
         y = "Times selected",
         x = "Court and interaction") +
    my_theme

ggsave("individual-models-coef-signs-sub.pdf", width = 8, height = 10)


## C
gdf2 <- player_df %>% group_by(tour, var, pos) %>%
    summarize(times = abs(sum(pos)))
gdf2$var <- as.character(gdf2$var)
cnms <- unique(gdf2$var)[-1]


## hmm
df <- player_df[, c("tour", "name", "var", "val")]
df$var <- gsub("I\\(court\\)", "", df$var)
df$var <- gsub("\\:", " : ", df$var)
df$var <- gsub("\\_", " ", df$var)
df$var <- gsub("pct", "%", df$var)
df$var <- gsub("netpt", "Net points won", df$var)
df$var <- gsub("bp", "Break points won", df$var)
df$var <- gsub("wue", "W/UE", df$var)
df$var <- gsub("US Open", "USO", df$var)
df$var <- gsub("Australian Open", "AO", df$var)
df$var <- gsub("Wimbledon", "WIM", df$var)
df$var <- gsub("ave", "Ave.", df$var)
df$var <- gsub("opponent", "Opponent", df$var)
df_wide <- spread(df, key = var, val = val)
df_wide <-  df_wide %>% replace(., is.na(.), 0)
## Tomorrow
#
cor <- df_wide %>% select(-c("tour", "name", "(Intercept)")) %>% cor()
ggcorrplot(cor, hc.order = FALSE, type = "lower",
           outline.col = "black") + my_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0)) +
    labs(x = "", y = "",
         title = "Correlation matrix for selected coefficients",
         subtitle = "For top players individual models")

ggsave("individual-models-cor-mat-sub.pdf", width = 8, height = 8)

## Men's
df_wide_m <- df_wide %>% filter(tour == "atp")
cor <- df_wide_m %>% select(-c("tour", "name", "(Intercept)")) %>% cor()
ggcorrplot(cor, hc.order = FALSE, type = "lower",
           outline.col = "black") + my_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0)) +
    labs(x = "", y = "",
         title = "Correlation matrix for selected coefficients",
         subtitle = "For top ATP players individual models")
#
ggsave("individual-models-cor-mat-atp-sub.pdf", width = 8, height = 8)

## Women's
df_wide_w <- df_wide %>% filter(tour == "wta")
cor <- df_wide_w %>% select(-c("tour", "name", "(Intercept)")) %>% cor()
ggcorrplot(cor, hc.order = FALSE, type = "lower",
           outline.col = "black") + my_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0)) +
    labs(x = "", y = "",
         title = "Correlation matrix for selected coefficients",
         subtitle = "For top WTA players individual models")
#
ggsave("individual-models-cor-mat-wta-sub.pdf", width = 8, height = 8)


## AO
slam_abb <- c("AO", "WIM", "USO")
tours <- c("wta", "atp")
titles <- c("Australian Open", "Wimbledon", "US Open")
g_list <- vector(mode = "list", length = length(slam_abb) *  length(tours))

for(jj in 1:length(tours)){
    for(ii in 1:length(slam_abb)){
        slam_vars <- grep(slam_abb[ii], colnames(df_wide), value = TRUE)
        tour <- tours[jj]
        cor <- df_wide_w %>% select(slam_vars) %>% cor()
        g_list[[length(slam_abb) * (jj -1) + ii]] <- ggcorrplot(cor, hc.order = FALSE, type = "lower",
                                             outline.col = "black") + my_theme +
            theme(axis.text.x = element_text(angle = 90, vjust = 0)) +
            labs(x = "", y = "",
                 title = paste0(titles[ii], " coefficients corr."),
                 subtitle = paste0("For top ", toupper(tour), " players ind. models"))
                                        #
            }
    }

pdf("cor-slams-sub.pdf", width = 16, height = 10)
do.call("grid.arrange", c(g_list, ncol = 3))
dev.off()


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

L <- length(inds)
player_list <- vector(mode = "list", length = L)
slams <- c("Australian Open", "French Open", "US Open", "Wimbledon")
quant <-  .5
pred_df <- gs_partial_players %>% group_by(name) %>%
    filter(name %in% top_players$name) %>%
    summarize_if(is.numeric, quantile, na.rm = TRUE, probs = quant)

pred_list <- vector(mode = "list", length = L)
for(ll in 1:L){
    ii <- which(top_players$name == as.character(unique(player_df$name))[ll])
    player_name <- top_players$name[ii]
    mod <- mods[[ii]]
    df0 <- data[ii]
    df <- data[[ii]]
    tour <- toupper(df$Tour[1])
    df1 <-  df %>% summarize_if(is.numeric, quantile, na.rm = TRUE, probs = .25)
    df1$quant <-  "1st quartile predictors"
    df1 <- rbind(df1, df1, df1, df1)
    df1$court <- slams
    df2 <-  df %>% summarize_if(is.numeric, quantile, na.rm = TRUE, probs = .5)
    df2$quant <-  "2nd quartile predictors"
    df2 <- rbind(df2, df2, df2, df2)
    df2$court <- slams
    df3 <-  df %>% summarize_if(is.numeric, quantile, na.rm = TRUE, probs = .75)
    df3$quant <-  "3rd quartile predictors"
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
    pred <- predict.lm(mod, newdata = df, interval = "predict")
    pred_df <- as.data.frame(pred)
    pred_df$name <- player_name
    pred_df$court <- df$court
    pred_df$tour <- tour
    pred_df$quant <- df$quant
    pred_list[[ii]] <- pred_df
### data frame summary
}

ggdf <- do.call('rbind', pred_list)
ggdf$upr <- ifelse(ggdf$upr > 1, 1, ggdf$upr)
ggdf$lwr <- ifelse(ggdf$lwr < 0, 0, ggdf$lwr)
ggdf$fit <- ifelse(ggdf$fit > 1, 1, ggdf$fit)
ggdf$fit <- ifelse(ggdf$fit < 0, 0, ggdf$fit)
ggplot(data = ggdf, aes(y = fit, x = name, col = court, group = court)) + 
    facet_wrap(quant~tour, scales = "free_y", ncol =2) +
    scale_color_manual(values = tournament_colors, name = "Tournament") +
    geom_errorbar(aes(ymin = lwr,
                      ymax = upr), position = position_dodge(width = .5)) +
    geom_point(position = position_dodge(width = .5), size = .5) + 
    coord_flip()  +
    my_theme  + theme(legend.position = "bottom") + 
    labs(x = "", y = "Expected % of points won",
         title = "Individual model predictions",
         subtitle = "Opponent rank = 10; reference court = French Open")
ggsave("individual-models-results-mod-sub.pdf", width = 10, height = 14)
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
## rewrite interps of cool graphs
## add how this work will transform tennis forever
## Explain about different styles of play and emphasize court differences in
## discussion/intro
## variable discussion and statistics ugh
## R package maintenace// vignette mostly to get EDA, hiearchical model, and individual models
