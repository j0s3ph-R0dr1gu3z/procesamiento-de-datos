#Gramatica dplyr
#Joseph rodriguez
#Agosto 2021
install.packages(nycflights13)
library(dplyr)
library(nycflights13)
library(ggplot2)


sqrt(mean(flights$distance))

flights$distance %>% mean() %>% sqrt()

#leer una base de datos

"Datos/eventos_de_platano.csv" %>% read.csv2(stringsAsFactors = 1) %>%summary()

datos_platano <- "Datos/eventos_de_platano.csv" %>% read.csv(stringsAsFactors = T)

datos_platano %>% head()

datos_platano %>% ggplot(aes(x=NO_ARBOLES, y=AREA_UM)) + geom_point() + 
  theme_bw()

ggsave("Resultados/plot_area_arboles.png")

library(tidyr)
library(dplyr)
library(ggplot2)

#Leer datos cultico utiloizando csv
crop_data <-read.csv("Datos/Maize_Cordova_Crop_2.csv", stringsAsFactors = T)
summary(crop_data)

crop_data
crop_data <- tibble(crop_data)

crop_data

is(crop_data)

#version transpuesta (se parece a str en dataframe)
glimpse(crop_data)

#filtrar en base a una condicion (filter)
crop_data[crop_data$Yield>8,]

?filter

filter(crop_data, Yield>8)

crop_data%>%filter(Yield >8) %>% summary()

#debe ir con doble =
crop_data %>% filter(Yield >8 & Sowing_Method=="Mecanizado")

#usando coma sale exactamente lo mismo
crop_data %>% filter(Yield >8 , Sowing_Method=="Mecanizado")

#Ordenar base de datos (arrange)
crop_data%>% arrange(Yield)
#descendiente
crop_data %>% arrange(desc(Yield))
#descendiente priorizando otro valor
crop_data %>% arrange(Sowing_Method, desc(Yield))

#Seleccionar algunas de las columnas de tibble
crop_data %>% select(Cultivar,Yield)

head(crop_data)

crop_data %>% select(Cultivar:Yield)

#ordenar las columnas en funcion a un nombre y con everyting acomoda el resto
crop_data %>% select(Sowing_Method, Former_Crop, everything())

#cambiar el nombre de una columna
crop_data %>% select(Sowing_Method, Cultivo_Anterior = Former_Crop, everything())
#otra funcion, primero enl nuevo nombre y luego el anterior
crop_data %>% rename(Rendimiento = Yield, Fecha_siembra = Planting_Date)

#cear una nueva columna dentro de la base de datos
crop_data <- crop_data %>% mutate(Dataset = "Maiz_cordoba")
crop_data
crop_data <- crop_data %>% mutate(YieldAvg = mean(Yield))

is(crop_data$Planting_Date)

#cambiar fechas a su formato
?as.Date
is(as.Date(crop_data$Planting_Date,"%m/%d/%Y"))

crop_data <- crop_data %>% mutate(Planting_Date =as.Date(Planting_Date,"%m/%d/%Y"),
                                  Harvest_Date = as.Date(Harvest_Date,"%m/%d/%Y"))
summary(crop_data)

#calculandeo los dias de ciclo del cultivo
crop_data <- crop_data %>% mutate(Length_Cycle = as.numeric(Harvest_Date-Planting_Date))
crop_data

#agrupar por categorias en la base de datos
#metodo antiguo
tapply(crop_data$Yield,crop_data$Sowing_Method, mean)
#dplyr

crop_data <- crop_data %>% group_by(Sowing_Method, na.rm=T)
crop_data %>% summarise(promedioY=mean(Yield, na.rm=T), maxY=max(Yield, na.rm=T),
                        sdY=sd(Yield, na.rm=T), promedioLC=mean(Length_Cycle, na.rm=T))
#n para contar datos
agregado<-
crop_data %>% group_by(Sowing_Method, Cultivar) %>% summarise(
  Avg_Yield=mean(Yield), Avg_LC = mean(Length_Cycle), n=n()
)

agregado
agregado %>% arrange(desc(n))


#separar
?separate
#ejemplo
#df <- data.frame(x = c("x", "x y", "x y z", NA))
#df %>% separate(x, c("a", "b"))

crop_data2 <-crop_data %>% mutate(Fecha=as.character(Harvest_Date))%>%
separate(Fecha, c("Anio","Mes","Dia"), sep="-")

crop_data2
summary(crop_data2)


# 1. utilizar mutate para convertir el año en numerico
crop_data2 <- crop_data2 %>% mutate(Anio = as.numeric(Anio))
summary(crop_data2)

# 2. filtrar os registros desde 2015 en adelante
fcrop_data2 <- crop_data2 %>% filter (Anio >= 2015)
summary(fcrop_data2)

# 3. hacer un agregado por Former crop

fcrop_data2 <- fcrop_data2 %>% group_by(Former_Crop)%>% 
  summarise(promedioY=mean(Yield, na.rm=T),
            medianaY=median(Yield, na.rm=T))

fcrop_data2
# 4. Ordenar el conjunto de datos resultante agregado de manera ascendente por
#promedio de rendimiento

fcrop_data2<-fcrop_data2 %>% arrange(promedioY)

write.csv(fcrop_data2,"Resultados/agregado.csv")
