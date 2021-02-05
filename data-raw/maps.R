library(echarts4r)
library(sp)
library(raster)
library(geojsonio)

france_dep <- getData("GADM", country = "FRANCE", level = 2)
lorraine <- france_dep[`%in%`(france_dep$NAME_2, c("Meurthe-et-Moselle", "Meuse", "Moselle", "Vosges")),]

ger_kreise <- getData("GADM", country = "GERMANY", level = 2)
ger_kreise <- ger_kreise[`%in%`(ger_kreise$NAME_1, c("Rheinland-Pfalz", "Saarland")),]
ger_kreise$NAME_2[ger_kreise$NAME_2 == "Eifelkreis Bitburg-Prüm"]  <- "Bitburg-Prüm"
ger_kreise$NAME_2[ger_kreise$NAME_2 == "St. Wendel"]  <- "Sankt Wendel"
#ger_kreise$NAME_2[ger_kreise$NAME_2 == "Regionalverband Saarbrücken"]  <- "Stadtverband Saarbrücken"
ger_kreise$NAME_2[ger_kreise$NAME_2 == "Altenkirchen (Westerwald)"]  <- "Altenkirchen"
ger_kreise$NAME_2[ger_kreise$NAME_2 == "Neustadt an der Weinstraße"]  <- "Neustadt a.d.Weinstraße"
ger_kreise$NAME_2[ger_kreise$NAME_2 == "Landau in der Pfalz"]  <- "Landau i.d.Pfalz"
ger_kreise$NAME_2[ger_kreise$NAME_2 == "Ludwigshafen am Rhein"]  <- "Ludwigshafen"
ger_kreise$NAME_2[ger_kreise$NAME_2 == "Frankenthal (Pfalz)"]  <- "Frankenthal"

be_province <- getData("GADM", country = "BELGIUM", level = 2)

be_lu <- be_province[be_province$NAME_1 == "Wallonie", ]
be_lu$NAME_2[be_lu$NAME_2 == "Luxembourg"]  <- "Province de Luxembourg"

lu_map <- getData("GADM", country = "LUXEMBOURG", level = 2)
lu_map_0 <- getData("GADM", country = "LUXEMBOURG", level = 0)

lu_map_0$GID_1 <- NA
lu_map_0$NAME_1 <- NA
lu_map_0$NL_NAME_1 <- NA
lu_map_0$GID_2 <- NA
lu_map_0$NAME_2 <- "Luxembourg"
lu_map_0$VARNAME_2 <- NA
lu_map_0$NL_NAME_2 <- NA
lu_map_0$TYPE_2 <- NA
lu_map_0$ENGTYPE_2 <- NA
lu_map_0$CC_2 <- NA
lu_map_0$HASC_2 <- NA

grande_region <- rbind(rbind(rbind(lorraine, ger_kreise), be_lu), lu_map_0)

grande_region %>% as.data.frame 


greater_region_covid <- get_greater_region_data(daily = FALSE) 



grande_region_json <- geojsonio::geojson_list(grande_region)

#Busethis::use_data(grande_region_json)

greater_region_covid %>%
  left_join(population) %>%
  mutate(cases = cases/population*100000) %>% 
  rename(NAME_2 = sub_region) %>%  
  group_by(week) %>%  
  e_charts(NAME_2, timeline = TRUE) %>%
  e_title("Weekly COVID-19 cases in the Greater Region", "Cases per 100k inhabitants") %>%  
  e_map_register("Grande Region", grande_region_json) %>%
  e_map(cases, map = "Grande Region", nameProperty = "NAME_2") %>%
  e_visual_map(min = 0, max = 1680) %>%
  e_timeline_opts(playInterval = 500) 

population <- data.table::fread("data-raw/population.csv")

usethis::use_data(population, overwrite = TRUE)

data("population")

lu <- get_lu_data()

lu %>% left_join(population) 

de <- get_de_data()

de %>% left_join(population) 

de %>% count(sub_region)  %>% as.data.frame 

population %>%
  filter(country == "Deutschland") %>%
  count(sub_region)


fr <- get_fr_data()

fr %>% left_join(population) %>% filter(is.na(population)) 

be <- get_be_data()

be %>% left_join(population) %>% filter(is.na(population)) 

