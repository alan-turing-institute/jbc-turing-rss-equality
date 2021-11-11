# Time-varying association between deprivation, ethnicity and SARS-CoV-2 infections

This repository contains code to run analysis described in *Time varying association between deprivation, ethnicity and SARS-CoV-2 infections in England: an ecological space-time study*.

## Data overview

**Due to data confidenciality issues, the testing and vaccination data used in the paper cannot be shared.**

Publicly available testing and vaccination **data provided in this repository are for illustration purpose only** and have been retrieved from https://coronavirus.data.gov.uk/details/download. 
They have been combined with publicly available datasets on LTLA level covariates:
- [LTLA rural/urban classification](https://geoportal.statistics.gov.uk/datasets/rural-urban-classification-2011-of-local-authority-districts-in-england/about)
- [Index of Multiple Deprivation](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)
- [Census data](https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?opt=3&theme=&subgrp=)

Note that whereas in the article we model the test positivity rate (number of positive tests out of total number of tests), here we use the number of positive tests out of the total population. 

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

## Notes

Overview of columns in `toy_data.RDS`:
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
