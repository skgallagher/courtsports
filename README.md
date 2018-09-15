# courtsports

An `R` package to analyze tennis data and a reproducible workspace.

Authors: Shannon Gallagher, Amanda Luby, and Kayla Frisoli


# INSTRUCTIONS

## Download our package

```{r}
devtools::install_github("http://github.com/shannong19/courtsports")
```

## Reproduce the report

To reproduce our paper for the sports analytics conference

1. Download the following file and save it as as `<my-file.Rmd>` (where you name the file):

[https://github.com/shannong19/courtsports/blob/master/contest/surface-type/paper-tennis.Rmd](https://github.com/shannong19/courtsports/blob/master/contest/surface-type/paper-tennis.Rmd) 

2. Open the file in `RStudio` and knit it OR

3. Open an `R` session
  a. Change the directory to where you downloaded the file
  b. Run the command `rmarkdown::render(<my-file.Rmd>)`
  c. Open and view `<my-file.pdf>` which should appear in the directory of the downloaded file
  
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
  
# Analysis and Results

We use packages from the [`tidyverse`](https://www.tidyverse.org/) to standarize our analysis process.  These include `dplyr`, `stringr`, `ggplot2`, `tidyr`, and more.  We hope that by using these, any user may follow our readable and succinct code.

All plots are reproducible directly from the `.Rmd` document.

## Hierarchical model

Our hierarchical model is computationally intensive for all possible implementations, it takes about ~1 hour to run.  As such, we have provided the code in the `.Rmd` file but do not evaluate it.  However, we have saved all intermediate steps as `.rda` objects accessible by the `data()` function so those interested can follow our analysis from beginning to end.
  
## TROUBLESHOOTING

1. Use the most recent version and packages in `R`

2. Make sure you have the following `R` packages installed (regular CRAN installation unless otherwise specified)
 + `deuce` (`r install_github("skoval/deuce"`)
 + `dplyr`
 + `forcats`
 + `ggplot2`
 + `ggrepel`
 + `grid`
 + `gridExtra`
 + `kableExtra`
 + `knitr`
 + `MASS`
 + `rmarkdown` 
 + `RStan` (See [mc-stan.org](http://mc-stan.org/users/interfaces/rstan) for detailed instructions.)
 + `stringr`
 
 
 

 
3. To compile the document as a .pdf file, the user must have [LaTeX installed and perhaps other packages](https://rmarkdown.rstudio.com/pdf_document_format)

4.  Regardless of the document knitting, the user may run the `R` chunks within the .Rmd file in conjunction with [RStudio](https://www.rstudio.com/) or other interface.
 
5. If none of this works, please feel free to leave a comment.
