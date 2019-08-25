# courtsports

An `R` package to analyze tennis data and a reproducible workspace.

Authors: Shannon Gallagher, Amanda Luby, and Kayla Frisoli

Find our paper [here](https://github.com/shannong19/courtsports/blob/master/contest/surface-type/paper-tennis.pdf).


# INSTRUCTIONS

## Download our package

```{r}
devtools::install_github("shannong19/courtsports")
```

## Results

The results of our paper may be reproduced by running the scripts in `paper_results`.  For user convenience, we have provided example code for each of the three sections.

### EDA and Visualization

### Hierarchical Modelling


### Individual Modelling
  
## Data Collection and Transformation

All data is freely available online from [Jeff Sackmann's data repositories](https://www.rstudio.com/).  In particular, we use [S. Kovalchik's `deuce` `R` package](https://github.com/skoval/deuce) to interface the repositories.  From `deuce`, we use the following data sets:

## Raw data

1. `data(atp_matches)` - Basic results of men's ATP matches including winner, loser, opponent, country of origin, court, and more (See `help(atp_matches)`) 
2. `data(wta_matches)`  - Basic results of women's WTA matches including winner, loser, opponent, country of origin, court, and more (See `help(wta_matches)`)
3. `data(gs_point_by_point)` Point by point data of men and women's matches with results on number of winners, unforced errors, net points won, and more.  This is not complete (See `help(gs_point_by_point)`)

## Data Transformation

We transform the raw data to use in our analysis.  These complete transformation scripts may be found in the folder `data-raw/`.  We then store the results of these transformations as `.rda` objects.  These 4 primary transformation objects are available in our package for use:

1. `data(gs)` - Combined men and women's grand slam matches from 2013-2017
2. `data(gs_players)` - Transformation of `gs` to look at a single player at a time, instead of pairs of opponents
3. `data(gs_partial)` - Aggregated point by point data for the grand slams.  It is not complete.
4. `data(gs_partial_players)` - Transformation of `gs_partial` to look at a single player.

The **complete and full* data description and summaries of the data are available [here]().
  
## TROUBLESHOOTING

0.  Make sure `courtsports` is installed. (`devtools::install_github("shannong19/courtsports")`)

1. Use the most recent version and packages in `R`

2. Make sure you have the following `R` packages installed (regular CRAN installation unless otherwise specified)

 + `broom`
 + `cowplot`
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
