#' Population of the region of the Greater Region
#'
#' A dataset with the total population of the different regions (provinces for Wallonia,
#' Lankreise and Stadtkreise for the Rhineland-Palatinate, départements for France and
#' the country of Luxembourg) composing the Greater Region.
#'
#' @format A data frame with 46 rows and 6 variables:
#' \describe{
#'   \item{country}{Luxembourg, France, Belgium or Germany}
#'   \item{region}{First administrative level. Lorraine for France, Wallonie for Belgium, Rheinland-Pfalz and Saarland for Germany and Luxembourg for Luxembourg}
#'   \item{sub_region}{Second administrative level: departments for France, provinces for Belgium, kreise for Germany and Luxembourg for Luxembourg}
#'   \item{year}{The year the population was counted; (1st of January) 2020 is the only value}
#'   \item{population}{Population of the sub_region}
#'   \item{source}{Where these values can be downloaded}
#'   ...
#' }
"population"

#' Covid cases and deaths for the Greater Region in 2020
#'
#' A dataset with the daily cases for the lowest available administrative regions of the Greater Region.
#' Deaths are missing for France however. The `population` dataset can be used to obtain cases or
#' deaths per 100'000 inhabitants.
#'
#' @format A data frame with 11277 rows and 6 variables:
#' \describe{
#'   \item{day}{The day of the measurement, in the YYYY-MM-DD format}
#'   \item{country}{Luxembourg, France, Belgium or Germany}
#'   \item{region}{First administrative level. Lorraine for France, Wallonie for Belgium, Rheinland-Pfalz for Germany and Luxembourg for Luxembourg}
#'   \item{sub_region}{Second administrative level: departments for France, provinces for Belgium, kreise for Germany and Luxembourg for Luxembourg}
#'   \item{cases}{Positive cases}
#'   \item{deaths}{Deaths}
#'   ...
#' }
"covid_data"


#' Map of the Greater Region
#'
#' Map of the Greater Region, loads a SpatialPolygonsDataFrame object.
#'
"grande_region_map"


