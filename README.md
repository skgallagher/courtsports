# courtsports

An `R` package to analyze tennis data and a reproducible workspace.

#INSTRUCTIONS

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
  
## TROUBLESHOOTING

1. Use the most recent version and packages in `R`
2. Make sure you have the following `R` packages installed (regular CRAN installation unless otherwise specified)
 + `deuce` (`r install_github("skoval/deuce"`)
 + `dplyr`
 + `ggplot2`
 + `forcats`
 + `stringr`
 + `knitr`
 + `kableExtra`
 + `rmarkdown` 
 + `RStan` (See [mc-stan.org](http://mc-stan.org/users/interfaces/rstan) for detailed instructions.)
 
3. To compile the document as a .pdf file, the user must have [LaTeX installed and perhaps other packages](https://rmarkdown.rstudio.com/pdf_document_format)

4.  Regardless of the document knitting, the user may run the `R` chunks within the .Rmd file
 
