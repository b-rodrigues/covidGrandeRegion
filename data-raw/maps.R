
france_dep <- getData("GADM", country = "FRANCE", level = 2)
moselle <- france_dep[france_dep$NAME_2 == "Moselle",]

ger_kreise <- getData("GADM", country = "GERMANY", level = 2)

rheinland_pfalz <- ger_kreise[`%in%`(ger_kreise$NAME_2, c("Eifelkreis Bitburg-Prüm", "Trier-Saarburg", "Trier", "Merzig-Wadern")),]

be_province <- getData("GADM", country = "BELGIUM", level = 2)

be_lu <- be_province[be_province$NAME_2 == "Luxembourg", ]

lu <- getData("GADM", country = "LUXEMBOURG", level = 2)

grande_region <- rbind(rbind(rbind(moselle, rheinland_pfalz), be_lu), lu)

grande_region %>% as.data.frame 

fake_data <- tidyr::tribble(~NAME_0, ~var,
                            "Luxembourg", 1,
                            "Germany", 0,
                            "France", 3,
                            "Belgium", 7)

grande_region_json <- geojsonio::geojson_list(grande_region)

library(echarts4r)
fake_data %>%  
e_charts(NAME_0) %>%
  e_map_register("Grande Region", grande_region_json) %>%
  e_map(var, map = "Grande Region", nameProperty = "NAME_0") %>%
  e_visual_map(var)
