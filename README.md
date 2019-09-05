# courtsports

An `R` package to analyze tennis data and a reproducible workspace.



# INSTRUCTIONS

## Download our package

```{r}
devtools::install_github("shannong19/courtsports")
```

## Results

The results of our paper may be reproduced by running the .R/.Rmd files in `paper_results`.  For user convenience, we have provided example code for each of the three sections.

### EDA and Visualization

```{r}
library(courtsports)
library(gridExtra)
library(ggplot2)
data(gs_players)

## Colors
tournament_colors <- c("#0297DB", "#b06835", "#0C2340", "#54008b")
league_colors <- c("#0C2340", "#902BA3")
win_colors <- c("#336699", "#339966")

# with yellow for us
tournament_colors <- c("#0297DB", "#b06835", "#ffe500", "#54008b")

p2 <- ggplot(gs_players) + geom_density(aes(pointswon, color = tournament)) + 
  labs(x = "Points") +
  scale_color_manual("Tournament", values=tournament_colors)

p3 <- ggplot(gs_players) + geom_density(aes(pointswon, color = league))+ 
  labs(x = "Points") +
  scale_color_manual("League", values=league_colors) 




p1 <- ggplot(gs_players) + geom_density(aes(pointswon, color = factor(did_win))) + 
  labs(x = "Points") +
  scale_color_manual("Winner?", values=win_colors) +
  facet_wrap(~league, ncol=1)

grid.arrange(p1, p2, ncol=2,
             top = grid::textGrob("Distribution of points per match",
                            gp=grid::gpar(fontsize=20, fontfamily="serif")),
              layout_matrix = rbind(c(NA, 1),
                                    c(2, 1),
                                    c(NA, 1)),
             heights=c(1, 3, 1))
```

### Hierarchical Modelling

```{r}
library(courtsports)
library(tidyverse)
library(broom)
library(lme4)

data(gs_players)
data(gs_partial_players)

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

gs_partial_players$name_int = as.integer(as.factor(gs_partial_players$name))
gs_partial_players$name_fac = as.factor(gs_partial_players$name)
gs_partial_players$ioc_fac = as.factor(gs_partial_players$ioc)
gs_partial_players$atp = gs_partial_players$Tour == "atp"
gs_partial_players$year_fac = as.factor(gs_partial_players$year)
gs_partial_players$late_round = gs_partial_players$round >= "R16"
gs_partial_players$seeded = gs_partial_players$rank <= 32
gs_partial_players$opponent_seeded = gs_partial_players$opponent_rank <= 32

## Fit models used in paper
set.seed(091418)
ind_logistic_noioc = lme4::glmer(did_win ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name_int), data = gs_players, family = "binomial", nAGQ =0)
n_aces_mod = lme4::lmer(n_aces ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name_fac), data = gs_partial_players)
n_winners_mod = lme4::lmer(n_winners ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name_fac), data = gs_partial_players)
n_net_mod = lme4::lmer(n_netpt_w ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name_fac), data = gs_partial_players)
n_ue_mod = lmer(n_ue ~ late_round + log(rank) + log(opponent_rank) + year_fac + atp + (0 + tournament |name_fac), data = gs_partial_players)
```

### Individual Modelling

```{r}
library(courtsports)
data(gs_partial_players)

player_name <- "Andy Murray"
ref <- "Wimbledon"
start_lower = TRUE
test_prop <- .50
out_list <- courtsports::model_individual(
                              player_name = player_name,
                              data = gs_partial_players,
                              ref = ref,
                              start_lower = start_lower,
                              test_prop = test_prop)
summary(out_list$final_model)
```
  
## Data Collection and Transformation

All data is freely available online from [Jeff Sackmann's data repositories](https://www.rstudio.com/).  In particular, we use [S. Kovalchik's `deuce` `R` package](https://github.com/skoval/deuce) to interface the repositories.  From `deuce`, we use the following data sets:

## Raw data

1. `data("atp_matches", package = "deuce")` - Basic results of men's ATP matches including winner, loser, opponent, country of origin, court, and more (See `help(atp_matches, package = "deuce")`) 
2. `data(wta_matches, package = "deuce")`  - Basic results of women's WTA matches including winner, loser, opponent, country of origin, court, and more (See `help(wta_matches, package = "deuce")`)
3. `data(gs_point_by_point, package = "deuce")` Point by point data of men and women's matches with results on number of winners, unforced errors, net points won, and more.  This is not complete (See `help(gs_point_by_point, package = "deuce")`)

## Data Transformation

We transform the raw data to use in our analysis.  These complete transformation scripts may be found in the folder `data-raw/`.  We then store the results of these transformations as `.rda` objects.  These 4 primary transformation objects are available in our package for use:

1. `data(gs)` - Combined men and women's grand slam matches from 2013-2017
2. `data(gs_players)` - Transformation of `gs` to look at a single player at a time, instead of pairs of opponents
3. `data(gs_partial)` - Aggregated point by point data for the grand slams.  It is crowd-sourced and partial.
4. `data(gs_partial_players)` - Transformation of `gs_partial` to look at a single player.

The **complete and full** data description and summaries of the data are available as .Rmd files [here](https://github.com/shannong19/courtsports/blob/master/paper_results/Data/data-description.Rmd).
  
## TROUBLESHOOTING

0.  Make sure `courtsports` is installed. (`devtools::install_github("shannong19/courtsports")`)

1. Use the most recent version and packages in `R`

2. Make sure you have the following `R` packages installed (regular CRAN installation unless otherwise specified)

 + `broom`
 + `deuce` (`install_github("skoval/deuce"`)
 + `dplyr`
 + `forcats`
 + `ggplot2`
 + `ggpubr`
 + `ggrepel`
 + `grid`
 + `gridExtra`
 + `kableExtra`
 + `knitr`
 + `lme4`
 + `MASS`
 + `rmarkdown` 
 + `stringr`
 + `tidyr`
 

 
3. If none of this works, please feel free to leave a comment.
