# Time-varying association between deprivation, ethnicity and SARS-CoV-2 infections

This repository contains code to run analysis described in [Time varying association between deprivation, ethnicity and SARS-CoV-2 infections in England: an ecological space-time study](https://www.medrxiv.org/content/10.1101/2021.11.09.21266054v1).

## Overview

**Due to data confidenciality issues, the testing and vaccination data used in the paper cannot be shared.**

Publicly available testing and vaccination **data provided in this repository are for illustration purpose only** and have been retrieved from https://coronavirus.data.gov.uk/details/download. 

Note that whereas in the article we model the test positivity rate (number of positive tests out of total number of tests), here we have to use the number of positive tests out of the total population. 

The data used here therefore cannot be used to reproduce the original study results.
Nevertheless, the model structure and parameters used in the actual analysis are the same as described here.

## Getting started

Clone this repo:

```{bash}
git clone https://github.com/alan-turing-institute/jbc-turing-rss-equality.git
```

Install the analysis package [R-INLA](https://www.r-inla.org):

```{R}
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
```

The analysis code is in the [equality_model.R](equality_model.R) script. 
It loads the toy dataset retrieved from public data sources and runs the two main formulas used in the paper. 
Note that to run the analysis, we also require the `W.adj` file contained in this repo. 
It has the LTLA adjacency matrix, which is necessary for including the spatial randon effect in the model.

## Plots

We also include a [plot.R](plot.R) script with code used for producing an equivalent of the key figures in the paper.
To produce the plots, further packages are required.
To install those, run:

```{R}
packages <- c(
  "tidyr",
  "dplyr",
  "ggplot2",
  "patchwork","
  "pals",
  "sf""
)
install.packages(packages)
```

As stated above, these plots do not reproduce results in the paper.

## Notes on files in this repository

This repository comes with a toy dataset (`toy_data.RDS`) with the following columns:
- `lad20cd`: LTLA code (using 2020 codes)
- `LTLA_ID`: an integer ID for each LTLA (1-311)
- `week` & `date_ID`: an integer ID for each week (0-44 and 1-45 respectively)
- `date_LTLA_ID`: an integer ID for each combination of LTLA and date
- `date_month`: an integer ID for the month of year of each week (1-11)
- `age_class`: the age bracket/group
- `counts`: number of positive tests in that LTLA, age group and week
- `tot_pop`: total population of the given LTLA in the given age group
- `vax_prop`: proportion of the population that is considered fully vaccinated in that LTLA, age group and week
- `IMD`: Index of Multiple Deprivation score (by LTLA)
- `Black_prop`: proportion of LTLA population that identifies as Black (according to the 2011 census)
- `South_Asian_prop`: proportion of LTLA population that identifies as South Asian (according to the 2011 census)
- `Other_BAME_prop`: proportion of LTLA population that identifies as non-White (but not Black or South Asian, according to the 2011 census)
- `rural_urban`: the rural/urban classification of the LTLA

All columns that end in `_stand` are standardized versions of that column (i.e., mean=0 and sd=1).

We also include a `space_obj.RData` file which we use for spatial plotting (it allows us to link LTLA_IDs with geographical data).

To create the toy dataset we used the following sources:
- [Coronavirus data](https://coronavirus.data.gov.uk/details/download)
- [LTLA rural/urban classification](https://geoportal.statistics.gov.uk/datasets/rural-urban-classification-2011-of-local-authority-districts-in-england/about)
- [Index of Multiple Deprivation](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)
- [Census data](https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?opt=3&theme=&subgrp=)