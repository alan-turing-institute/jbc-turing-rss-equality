# Time-varying association between deprivation, ethnicity and SARS-CoV-2 infections

This repository contains code to run the analysis described in *Time varying association between deprivation, ethnicity and SARS-CoV-2 infections in England: an ecological space-time study*.

## Data

Due to data confidenciality issues, the testing and vaccination data used in the paper cannot be shared.

Publicly available testing and vaccination data provided in this repository are for illustration purpose only and have been retrieved from https://coronavirus.data.gov.uk/details/download.

Note that whereas in the paper we model the test positivity rate (number of positive tests out of total number of tests), here we model the number of positive tests out of the total population. 

## Getting started

The analysis is written in R and uses the package [R-INLA](https://www.r-inla.org).
To [install](https://www.r-inla.org/download-install):

```{R}
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
```

The analysis code is contained in the [equality_model.R](equality_model.R) script. This loads the toy data retrieved from public data sources and runs the two main formulas used in the paper.
