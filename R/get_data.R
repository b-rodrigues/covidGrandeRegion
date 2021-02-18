#' Download data for the German border regions of Luxembourg
#' @param url The url to the data. By default, points to the latest known url.
#' @param daily If TRUE, get daily cases, if FALSE, weekly cases.
#' @return A data frame with the latest positive cases and deaths.
#' @import dplyr
#' @importFrom data.table fread
#' @importFrom lubridate ymd isoweek year 
#' @importFrom stringr str_sub
#' @export
#' @examples
#' \dontrun{
#' get_de_data()
#' }
get_de_data <- function(url = "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv",
                        daily = TRUE){

  dataset <- fread(url) %>%
    mutate(country = "Deutschland") %>%  
    filter(Bundesland %in% c("Saarland", "Rheinland-Pfalz")) %>%  
    rename(day = Refdatum) %>%
    mutate(day = str_sub(day, 1, 10)) %>%  
    mutate(day = ymd(day)) %>%  
    filter(day >= ymd("2020-02-24")) %>%
    group_by(day, country, region = Bundesland, sub_region = Landkreis) %>%  
    summarise(cases = sum(AnzahlFall),
              deaths = sum(AnzahlTodesfall)) %>%
    ungroup() %>%
    mutate(sub_region = ifelse(sub_region == "SK Kaiserslautern", "SK Kaiserslautern (Kreisfreie Stadt)", sub_region)) %>%  
    mutate(sub_region = ifelse(sub_region == "LK Stadtverband Saarbrücken", "LK Regionalverband Saarbrücken", sub_region)) %>%  
    mutate(sub_region = str_remove(sub_region, "^..."))

  if(daily){
    return(dataset)
  } else {
    dataset <- dataset %>%  
      mutate(week = isoweek(day)) %>%
      mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%  
      mutate(week = paste0(year(day), "-W", week, "-1")) %>%
      mutate(week = ifelse(week == "2021-W53-1", "2021-W01-1", week)) %>%  
      mutate(week = ISOweek2date(week)) %>%  
      group_by(week, country, region, sub_region) %>%
      summarise(cases = sum(cases),
                deaths = sum(deaths)) %>%
      ungroup()
      }

    return(dataset)
}

#' Download data for the Belgian border regions of Luxembourg
#' @param url_cases The url to the cases data. By default, points to the latest known url.
#' @param url_deaths The url to the deaths data. By default, points to the latest known url.
#' @param daily If TRUE, get daily cases, if FALSE, weekly cases.
#' @return A data frame with the latest positive cases and deaths.
#' @import dplyr
#' @importFrom data.table fread
#' @importFrom lubridate ymd
#' @export
#' @examples
#' \dontrun{
#' get_be_data()
#' }
get_be_data <- function(url_cases = "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv",
                        url_deaths = "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv",
                        daily = TRUE){

  data_cases <- fread(url_cases)
  data_deaths <- fread(url_deaths)
    

  dataset <- full_join(data_cases,
                       data_deaths, by = c("DATE", "REGION", "AGEGROUP", "SEX")) %>%
    mutate(CASES = ifelse(is.na(CASES), 0, CASES)) %>%
    mutate(DEATHS = ifelse(is.na(DEATHS), 0, DEATHS))

  dataset <- dataset %>%  
    mutate(country = "Belgique") %>%  
    filter(REGION == "Wallonia") %>%
    mutate(REGION = "Wallonie") %>%  
    filter(DATE >= ymd("2020-02-24")) %>%
    rename(day = DATE) %>%
    group_by(day, country, region = REGION, sub_region = PROVINCE) %>%
    summarise(cases = sum(CASES),
              deaths = sum(DEATHS)) %>%
    ungroup() %>%
    mutate(sub_region = ifelse(sub_region == "Luxembourg", "Province de Luxembourg", sub_region)) %>%  
    mutate(sub_region = ifelse(sub_region == "BrabantWallon", "Brabant Wallon", sub_region))

  if(daily){
    return(dataset)
  } else {
    dataset <- dataset %>%  
      mutate(week = isoweek(day)) %>%
      mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%  
      mutate(week = paste0(year(day), "-W", week, "-1")) %>%
      mutate(week = ifelse(week == "2021-W53-1", "2021-W01-1", week)) %>%  
      mutate(week = ISOweek2date(week)) %>%  
      group_by(week, country, region, sub_region) %>%
      summarise(cases = sum(cases),
                deaths = sum(deaths)) %>%
      ungroup()
  }

  return(dataset)
}

#' Download data for the French border regions of Luxembourg
#' @param url_alt The url to the data from the first wave (Spring 2020), but an alternative, more complete source. This
#' data is based on newspapers articles and daily reports from the French ARS. For the month of March, this data set
#' reflects the reality better. By default, points to the latest known url (source: \url{https://github.com/opencovid19-fr/data}).
#' @param url_old The url to the data from the first wave (Spring 2020). By default, points to the latest known url.
#' @param url_new The url to the data from after the first wave (Spring 2020). By default, points to the latest known url.
#' @param daily If TRUE, get daily cases, if FALSE, weekly cases.
#' @return A data frame with the latest positive cases. Deaths are unavailable, so that column is empty.
#' @import dplyr
#' @importFrom data.table fread
#' @importFrom lubridate ymd
#' @importFrom stringr str_replace_all
#' @export
#' @examples
#' \dontrun{
#' get_fr_data()
#' }
get_fr_data <- function(url_alt = "https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv",
                        url_old = "https://www.data.gouv.fr/fr/datasets/r/b4ea7b4b-b7d1-4885-a099-71852291ff20",
                        url_new = "https://www.data.gouv.fr/en/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675",
                        daily = TRUE){


  fr_alt <- fread(url_alt) %>%
    filter(maille_code %in% c("DEP-54", "DEP-55", "DEP-57", "DEP-88")) %>%
    mutate(country = "France",
           region = "Lorraine") %>%
    mutate(day = ymd(date)) %>%  
    select(day, country, region, sub_region = maille_nom, cases = cas_confirmes) %>%
    filter(!is.na(cases)) 

  fr_old <- fread(url_old) %>%
    filter(dep %in% c(54, 55, 57, 88)) %>%
    mutate(country = "France",
           region = "Lorraine") %>%
    mutate(sub_region = case_when(dep == 54 ~ "Meurthe-et-Moselle",
                                 dep == 55 ~ "Meuse",
                                 dep == 57 ~ "Moselle",
                                 dep == 88 ~ "Vosges")) %>%
    mutate(day = ymd(jour)) %>%  
    select(day, country, region, sub_region, cases = nb_pos)

  fr_new <- fread(url_new) %>%
    filter(dep %in% c(54, 55, 57, 88)) %>%
    mutate(country = "France",
           region = "Lorraine") %>%
    mutate(sub_region = case_when(dep == 54 ~ "Meurthe-et-Moselle",
                                 dep == 55 ~ "Meuse",
                                 dep == 57 ~ "Moselle",
                                 dep == 88 ~ "Vosges")) %>%
    mutate(day = ymd(jour)) %>%
    select(day, country, region, sub_region, cases = P)

  dataset <- bind_rows(fr_alt, fr_old, fr_new) %>%
    group_by(day, country, region, sub_region) %>%
    summarise(cases = sum(cases),
              deaths = NA) %>%
    ungroup()

  if(daily){
    return(dataset)
  } else {
    dataset <- dataset %>%
      mutate(week = isoweek(day)) %>%
      mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%
      mutate(week = paste0(year(day), "-W", week, "-1")) %>%
      mutate(week = ifelse(week == "2021-W53-1", "2021-W01-1", week)) %>%
      mutate(week = ISOweek2date(week)) %>%
      group_by(week, country, region, sub_region) %>%
      summarise(cases = sum(cases),
                deaths = NA) %>%
      ungroup()
      }
  return(dataset)
}


#' Download data for Luxembourg. 
#' @param url The url to the data. By default, points to the latest known url.
#' @param daily If TRUE, get daily cases and deaths, if FALSE, weekly cases.
#' @return A data frame with the latest positive cases and deaths.
#' @import dplyr
#' @importFrom data.table fread
#' @importFrom lubridate dmy
#' @importFrom stringr str_remove
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_fr_data()
#' }
get_lu_data <- function(url = "https://data.public.lu/en/datasets/r/767f8091-0591-4b04-9a6f-a9d60cd57159",
                        daily = TRUE){

  suppressWarnings({
    dataset <- fread(url) %>%
      clean_names() %>%
      mutate(day = dmy(date)) %>%
      filter(day >= ymd("2020-02-24")) %>% 
      mutate(across(where(is.character), as.numeric)) %>%
      rename_all(~str_remove(., "x1_")) %>%
      mutate(country = "Luxembourg",
             region = "Luxembourg",
             sub_region = "Luxembourg") %>%  
      mutate(l_morts = lag(nb_morts)) %>%
      mutate(l_morts = ifelse(is.na(l_morts), 0, l_morts)) %>%
      mutate(morts = nb_morts - l_morts) %>%  
      group_by(day, country, region, sub_region) 
      summarise(cases = sum(nb_de_positifs, na.rm = TRUE),
                deaths = sum(morts, na.rm = TRUE)) %>%
      ungroup()

    if(daily){
      return(dataset)
    } else {
      dataset <- dataset %>%
        mutate(week = isoweek(day)) %>%
        mutate(week = ifelse(nchar(week) == 1, paste0(0, week), week)) %>%  
        mutate(week = paste0(year(day), "-W", week, "-1")) %>%
        mutate(week = ifelse(week == "2021-W53-1", "2021-W01-1", week)) %>%  
        mutate(week = ISOweek2date(week)) %>%  
        group_by(week, country, region, sub_region) %>%
        summarise(cases = sum(cases),
                  deaths = sum(nb_morts, na.rm = TRUE)) %>%
        ungroup() 
    }
    return(dataset)
  })

}


#' Download data for the entirety of the Greater Region
#' @param daily If TRUE, get daily cases, if FALSE, weekly cases.
#' @return A data frame the latest positive cases data for the whole Greater Region
#' @import dplyr
#' @importFrom data.table fread
#' @importFrom lubridate dmy
#' @importFrom stringr str_remove
#' @importFrom janitor clean_names
#' @export
#' @examples
#' \dontrun{
#' get_fr_data()
#' }
get_greater_region_data <- function(daily = TRUE){

  lu <- get_lu_data(daily = daily)
  fr <- get_fr_data(daily = daily)
  be <- get_be_data(daily = daily)
  de <- get_de_data(daily = daily)

  bind_rows(lu, fr, be, de)

}
