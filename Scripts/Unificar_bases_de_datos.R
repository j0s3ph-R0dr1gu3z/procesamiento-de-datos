#Unir bases de datos
#Joseph Rodriguez
#05/08/2021

library(tidyr)
library(dplyr)
crop_data <- tibble (read.csv("Datos/Maize_Cordova_Crop_2.csv"))
soil_data <- tibble (read.csv("Datos/Soil_Data.csv"))


crop_data ; soil_data

#unir dos listas
alldata <- crop_data %>% left_join(soil_data, by = "ID")

summary(alldata)

soil_data_low_OM <- soil_data %>% filter(Organic_Matter_Content =="BAJA")

soil_data_low_OM

crop_data %>% left_join(soil_data_low_OM, by=c("ID"="ID"))

inner_crop <- crop_data %>% inner_join(soil_data_low_OM, by="ID")
inner_crop

rigth_crop <- crop_data %>% right_join(soil_data_low_OM, by="ID")

crop_data %>% semi_join(soil_data_low_OM, by = "ID")

#band members y band instruments
band_members
band_instruments
band_instruments2

band_members %>% full_join(band_instruments2, by=c("name" = "artist"))
