#' Draw an interactive map of the COVID-19 cases in the Greater Region
#' @param dataset The dataset to use. Must be one of the datasets provide by either `get_de_data()`,
#' `get_fr_data()`, `get_be_data()` or ` get_lu_ðata()`, or any combination of these. Can also be
#' the output from `get_greater_region_data()`. If missing, data will be downloaded automatically
#' (using `get_greater_region_data()`).
#' @param daily If TRUE, get daily cases, if FALSE, weekly cases.
#' @param normalize If TRUE, get cases per 100k inhabitants, if FALSE, get absolute cases.
#' @return A data frame the latest positive cases data.
#' @import dplyr echarts4r
#' @importFrom data.table fread
#' @importFrom lubridate ymd isoweek year 
#' @importFrom stringr str_sub
#' @importFrom rlang quo `!!`
#' @export
#' @examples
#' \dontrun{
#' draw_map(fr_data, daily = FALSE) # draws a map of the French regions using weekly cases
#' draw_map(bind_rows(fr_data, lu_data), daily = FALSE) # draws a map of the French regions and Luxembourg using weekly cases
#' }
draw_map <- function(dataset = NULL,
                        daily = TRUE,
                        normalize = TRUE){

  if(daily){
    group_var <- quo(day)
  } else {
    group_var <- quo(week)
  }
  
  if(is.null(dataset)){
    dataset <- get_greater_region_data(daily = daily)
  } else {
    if(daily){
      stopifnot("day" %in% colnames(dataset))
    } else {
      stopifnot("week" %in% colnames(dataset))
    }
  }

  if(normalize){
    data("population")
    dataset <- dataset %>%
      right_join(population) %>%
      mutate(cases = cases/population*100000)
  } 

  max_cases <- dataset %>%
    summarise(max_cases = max(cases)) %>%
    pull(max_cases)

  if(daily){
    daily_string <- "Daily"
  } else {
    daily_string <- "Weekly"
  }

  if(normalize){
    pop_strings <- "Cases per 100k inhabitants"
  } else {
    pop_strings <- "Raw cases"
  }

  data("grande_region_map")

  dataset %>%  
    rename(NAME_2 = sub_region) %>%  
    group_by(!!group_var) %>%  
    e_charts(NAME_2, timeline = TRUE) %>%
    e_title(paste0(daily_string, " COVID-19 cases in the Greater Region"), pop_strings) %>%  
    e_map_register("Grande Region", grande_region_map) %>%
    e_map(cases, map = "Grande Region", nameProperty = "NAME_2") %>%
    e_visual_map(min = 0, max = max_cases) %>%
    e_timeline_opts(playInterval = 500) 
}
