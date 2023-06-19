# Time-varying association between deprivation, ethnicity and SARS-CoV-2 infections

This repository contains code to run analyses described in [Time varying association between deprivation, ethnicity and SARS-CoV-2 infections in England: A population-based ecological study](https://www.sciencedirect.com/science/article/pii/S2666776222000151).

## Overview

**Due to data confidenciality issues, the testing and vaccination data used in the paper cannot be shared.**

Publicly available testing and vaccination **data provided in this repository are for illustration purpose only** and have been retrieved from https://coronavirus.data.gov.uk/details/download.

Note that whereas in the article we model the test positivity rate (number of positive tests out of total number of tests), here we have to use the number of positive tests out of the total population.

The data in this repository therefore cannot be used to reproduce the original study results.
Nevertheless, the model structure and parameters used in the actual analysis are the same as described here.
We also include plotting code that shows how to format the analysis outputs.

## Getting started

Clone this repo:

```{bash}
git clone https://github.com/alan-turing-institute/jbc-turing-rss-equality.git
```

The analysis is in R and requires installing the [R-INLA](https://www.r-inla.org) package:

```{R}
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
```

To install other required packages (mainly for plotting):

```{R}
packages <- c(
  "tidyr",
  "plyr",
  "dplyr",
  "ggplot2",
  "patchwork",
  "pals",
  "sf"
)
install.packages(packages)
```

## Analysis

The analysis code with the two main formulas used in the paper is in the [equality_model.R](equality_model.R) script.
The analysis script loads two files from this repo:
- `toy_data.RDS` which contains a toy dataset retrieved from public data source
- `W.adj` which has the LTLA adjacency matrix necessary for including the spatial randon effect in the model

The [plot.R](plot.R) script contains code for producing an equivalent of the key figures in the paper using outputs of the two analyses here.

## Data

This repository comes with a toy dataset (`toy_data.RDS`).
The data is aggregated by LTLA, week and age class.

The dataset has the following columns:
- `lad20cd`: LTLA code (using 2020 codes)
- `LTLA_ID`: an integer ID for each LTLA (1-311)
- `week` & `date_ID`: an integer ID for each week (0-44 and 1-45 respectively)
- `date_LTLA_ID`: an integer ID for each combination of LTLA and date
- `date_month`: an integer ID for the month of each week (1-11; note that these do not correspond to calendar months)
- `age_class`: the age bracket/group
- `counts`: number of positive tests in that LTLA, age group and week
- `tot_pop`: total population of the given age group in the LTLA
- `vax_prop`: proportion of the population that is considered fully vaccinated in the LTLA, age group and week
- `IMD`: Index of Multiple Deprivation score (by LTLA)
- `Black_prop`: proportion of LTLA population that identifies as Black (according to the 2011 census)
- `South_Asian_prop`: proportion of LTLA population that identifies as South Asian (according to the 2011 census)
- `Other_BAME_prop`: proportion of LTLA population that identifies as non-White (but not Black or South Asian, according to the 2011 census)
- `BAME`: proportion of LTLA population that identifies as Black, South Asian or Other BAME (according to 2011 census)
- `rural_urban`: the rural/urban classification of the LTLA

All columns that end in `_stand` are standardized versions of that column (i.e., mean=0 and sd=1).

There are also multiple `date_month_X` columns. These are all identical but R-INLA requires that we have a new column in the formula each time we want to re-use the variable.

To create the toy dataset we used the following sources:
- [Coronavirus data](https://coronavirus.data.gov.uk/details/download)
- [LTLA rural/urban classification](https://geoportal.statistics.gov.uk/datasets/rural-urban-classification-2011-of-local-authority-districts-in-england/about)
- [Index of Multiple Deprivation](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)
- [Census data](https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?opt=3&theme=&subgrp=)

We also include in this repository a `space_obj.RData` file which we use for the spatial plots (it links LTLA_IDs with geographical data).
