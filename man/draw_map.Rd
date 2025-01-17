% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_map.R
\name{draw_map}
\alias{draw_map}
\title{Draw an interactive map of the COVID-19 cases in the Greater Region}
\usage{
draw_map(dataset = NULL, daily = TRUE, normalize = TRUE)
}
\arguments{
\item{dataset}{The dataset to use. Must be one of the datasets provide by either \code{get_de_data()},
\code{get_fr_data()}, \code{get_be_data()} or \code{ get_lu_ðata()}, or any combination of these. Can also be
the output from \code{get_greater_region_data()}. If missing, data will be downloaded automatically
(using \code{get_greater_region_data()}).}

\item{daily}{If TRUE, get daily cases, if FALSE, weekly cases.}

\item{normalize}{If TRUE, get cases per 100k inhabitants, if FALSE, get absolute cases.}
}
\value{
A data frame the latest positive cases data.
}
\description{
Draw an interactive map of the COVID-19 cases in the Greater Region
}
\examples{
\dontrun{
draw_map(fr_data, daily = FALSE) # draws a map of the French regions using weekly cases
draw_map(bind_rows(fr_data, lu_data), daily = FALSE) # draws a map of the French regions and Luxembourg using weekly cases
}
}
