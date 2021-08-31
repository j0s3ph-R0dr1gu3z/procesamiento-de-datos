#estructurar bases de datos
#Joseph rodriguez 
#3-8-2021
library(ggplot2)
library(dplyr)
library(tidyr)

data_long <- read.csv('Datos/cierre_agricola_mun_long.csv')

data_long <- tibble(data_long)

data_long

#al colocar scales dentro del facet_wrap agrupa los graficos y 
#corrige las escalas
data_long %>%> ggplot(aes(x=Nommodalidad, y=value))+geom_boxplot()+
  facet_wrap(~Variable, scales='free')

#existen datos mezclados, se ampliara la base de datos
#extraeremos los nombres de la columna Variable y sus valores serán de la 
#columna value

datos_censo <- data_long %>% pivot_wider(id_cols= c("Nommunicipio", "Nommodalidad"),
                                         names_from="Variable", 
                                         values_from="value")
?pivot_wider

datos_censo

summary(datos_censo)

#ahora podemos calcular rendimiento
datos_censo %>% mutate(Produccion = Area*Rendimiento)

datos_censo %>% pivot_wider (id_cols="Nommunicipio",
                             names_from = "Nommodalidad",
                             values_from = "Rendimiento")

#hacer el ejercicio a la inversa
datos_censo %>% pivot_longer(cols = c('Area', 'Rendimiento'),
                             names_to = 'Variable',
                             values_to ="Valores")


#reemplazar errores en la base de datos
#referncias en https://stringr.tidyverse.org/
library(stringr)
str_replace(data_long$Municipio, "Añº", "Anio")
#para reemplazar varios a la vez
mun <- str_replace_all(data_long$Municipio, c("error01"="e1","error02"="e2", "error03=e03" ))

#cambiar minusculas a mayusculas
str_to_upper(mun)




