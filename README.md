# covidGrandeRegion

## Intro

This package makes it easy to download daily or weekly COVID-19 cases data for the various regions from the [Greater Region](https://en.wikipedia.org/wiki/Greater_Luxembourg_(modern_region)).

For each of the 4 countries composing the Greater Region, there's a function that downloads the data in a tidy format:

```
get_lu_data() # downloads daily data for the Grand-Duchy of Luxembourg
get_de_data() # downloads daily data for Rhineland-Palatinate

get_lu_data(daily = FALSE) # downloads weekly data for the Grand-Duchy of Luxembourg
```

Features to easily visualize this data are planned.

## Installation

```
remotes::install_github("b-rodrigues/covidGrandeRegion")
```
