library(tidyverse)
library(lubridate)
library(ggrepel)
library(leaflet)
library(sf)
library(htmltools)

##set dates in spanish
Sys.setlocale("LC_TIME", "Spanish_Spain.1252")

raw_dir = "1.raw"
clean_dir = "2.clean"
date_since = "2020-01-09" ##fecha para empezar el analisis

##read data ----------------------------------------------------------------------------------------

afluencia = read_csv(file.path(raw_dir,"afluencia-diaria-del-metro-cdmx.csv" )) %>%
  mutate(Linea = str_replace(Linea, "Línea", "Linea")) ##datos de afluencia


estaciones =read_csv(file.path(raw_dir,"estaciones-del-metro.csv" )) %>% ##gps de estaciones (datos abiertos)
  rename(Estacion = Name) %>%
  select(Estacion, lat, lon) %>%
  group_by(Estacion) %>%
  slice(1) %>%
  ungroup() 


alcaldias = read_rds(file.path(raw_dir,"estacion_alcadias.rds"))  ##alcaldias (pagina web) 




shape = st_as_sf(read_rds(file.path(raw_dir,"municipios.rds")))%>% ##solamente de CDMX y EDOMEX
  rename(Alcaldia = Nombre) %>%
  filter(ENTIDAD_RES_ID %in% c("MX09", "MX15")) %>%
  mutate(Alcaldia = as.character(Alcaldia))


##Limpiar nombres de alcaldia en shapefile ----------------------------------------------------------------------

shape$Alcaldia[shape$Alcaldia=="Cuauht moc"] <- "Cuauhtémoc"
shape$Alcaldia[shape$Alcaldia=="0lvaro Obregzn"] <- "Álvaro Obregón"
shape$Alcaldia[shape$Alcaldia=="Ecatepec de Morelos"] <- "Ecatepec"
shape$Alcaldia[shape$Alcaldia=="Coyoacln"] <- "Coyoacán"
shape$Alcaldia[shape$Alcaldia=="Benito Julrez"] <- "Benito Juárez"
shape$Alcaldia[shape$Alcaldia=="Naucalpan de JuIrez"] <- "Naucalpan de Juárez"
shape$Alcaldia[shape$Alcaldia=="La Paz"] <- "Los Reyes La Paz"
shape$Alcaldia[shape$Alcaldia=="Tl1huac"] <- "Tláhuac"


##Shapefile con alcaldias de interes (todas las de cdmx y las del edomex donde hay metro)
shape2 = shape[which(shape$Alcaldia %in% alcaldias$Alcaldia | shape$ENTIDAD_RES_ID=="MX09"), ]
	

#testar diferencias entre tabla de alcaldias y shapefile
setdiff(unique(alcaldias$Alcaldia), unique(shape$Alcaldia))

write_rds(shape2, "2.clean/shape_alcaldias.rds")


##unir bases de datos para crear tabla unica --------------------------------------------------------


##check differences entre alcaldias (base de www.metro.com) y gps de estaciones (datos abiertos)
setdiff(unique(alcaldias$Estacion), unique(estaciones$Estacion))



##join  estaciones (datos abiertos) con alcaldias y logos (www.metro)
estaciones_alcaldias = nest_join(alcaldias, estaciones, by="Estacion") %>%
  unnest(estaciones)



##merge alcaldias con datos abiertos
estaciones_alcaldias$Estacion[estaciones_alcaldias$Estacion=="Miguel A. de Quevedo"] <-  "Miguel A. de Q."
afluencia$Estacion[afluencia$Estacion=="Tecnológico"] <-  "Ecatepec" ##estacion tecnologico cambio el nombre a Ecatepec

setdiff(unique(afluencia$Estacion), unique(estaciones_alcaldias$Estacion))


##datos con estacion, gps, alcaldia y afluencia 
metro_df = left_join(afluencia, estaciones_alcaldias, by="Estacion")



##explorar datos -----------------------------------------------------------------------------------------
max(metro_df$Fecha) ##la ultima fecha es del 31 de marzo 2020
min(metro_df$Fecha) ##la ultima fecha es del 31 de marzo 2020

table(metro_df$Linea) 
table(metro_df$Estacion)
sum(metro_df$Afluencia)
unique(metro_df$Linea)



##BASE DE DATOS COMPLETA: (Afluencia + colores + logos gps)  colores: obtenidos de www.metro.com) y dia de la semana -----------------------------------------------------


lineas =c("Linea 1",  "Linea 2","Linea 3" , "Linea 4",  "Linea 5",
           "Linea 6",  "Linea 7",  "Linea 8",  "Linea 9",
           "Linea A",  "Linea B", "Linea 12" )

##colores de cada linea
colores=c("#F56394", ##1
          "#007BC1",##2
          "#BFAD1E", ##3
          "#7BC7BA", ## 4
          "#FFDB25", ## 5,
          "#E82428", ##6, 
          "#F17C2F", ##7
          "#00A263", ##8
          "#571C00", ##9
          "#8E248E",##A
          "#BEBFC3", ##B
           "#B69C51" ##12
          
          
          )


lineas_colores = as.data.frame(cbind(lineas, colores)) %>%
  rename(Linea = lineas) %>%
  mutate(Linea = factor(Linea, ##convertir lineas en factor para mejor visualizacion
                        levels = lineas)
  )





##traer colores y crear dia de la semana

metro_df1 = metro_df %>%
  ##join with colores
full_join(lineas_colores, by="Linea") %>%
  
  ##crete dia y semana
mutate(Linea = factor(Linea, ##convertir lineas en factor para mejor visualizacion
                        levels = lineas),
         dia = wday(Fecha, label = T, abbr = F, ##Dias de la semana
                           week_start = getOption("lubridate.week.start", 1)),
         semana = week(Fecha))  




saveRDS(metro_df1, "2.clean/metro.rds")



## Promedio por semana y dia desde 2010 ------------------------------------------------------------------------

promedio = metro_df1 %>%
  filter(Fecha >= "2020-01-03" & Fecha <= "2020-02-06") %>%
  group_by(Alcaldia, Linea, Estacion, dia) %>%
  summarise(Promedio = mean(Afluencia, na.rm = T)) %>%
  ungroup() 



saveRDS(promedio, "2.clean/promedio.rds")



##Guardar datos 





##tablas para analisis -----------------------------------------------------------------------------------------------

##TODO EL SIETEMA METRO: tabla con afluencia diaria desde fecha de interes
diario = raw2 %>%
    group_by(Fecha) %>%
  summarise(viajes = sum(Afluencia)/1000000) ##in millions


ggplot(data= diario, ##el numero de vijes no cambia antes de covid
       aes(x=Fecha,
           y = viajes))+
geom_bar(stat = "identity")



##POR DIA DE LA SEMANA DE TODO EL SISTEMA METRO:tabla con movimiento por dia de la semana
semanal = raw2 %>%
  mutate(dia = wday(Fecha, label = T, abbr = F,
                    week_start = getOption("lubridate.week.start", 1))) %>%
  group_by(dia) %>%
  summarise(viajes = sum(Afluencia)/1000000)


ggplot(data= semanal, ##viernes son los dias mas usados
       aes(x=dia,
           y = viajes))+
  geom_bar(stat = "identity")


##COMPARACION CON INICIO DE CUARENTENA POR ESTACION tabla con afluencia en relacion a cuarentena por

covid = raw2 %>%
  group_by(Linea, Fecha) %>%
  summarise(Afluencia =sum(Afluencia),
            colores = first(colores)) %>%
  mutate(Inicio = first(Afluencia),
         Cambio = round((Afluencia/Inicio)*100, digits = 0))



 ##TODO : pensar si es mejor hacerlo por estacion o todas juntas (dependiendo que tan juntas estan)
##por linea
ggplot(data= covid, ##viernes son los dias mas usados
       aes(x=Fecha,
           y = Cambio,
           color = Linea
          ))+
  geom_line() +
   ylim(0,max(covid$Cambio)+10) +
  facet_wrap(~Linea, ncol = 4) +
  scale_x_date(date_breaks = "3 week",
               date_labels = "%b %d") +
  scale_color_manual(values=colores) +
  labs(title = "Cambio en afluencia desde inicio de cuarentena")



##POR ESTACION CON MAYOR AFLUENCIA:

populares = raw2 %>% 
  group_by(Estacion) %>%
  summarise(max = sum(Afluencia)) %>%
  ungroup() %>%
  arrange(desc(max)) %>%
  filter(row_number()<6) %>%
  left_join(raw2, by="Estacion")%>%
  group_by(Estacion, Fecha) %>%
  summarise(Afluencia =sum(Afluencia),
            colores = first(colores)) %>%
  mutate(Inicio = first(Afluencia),
         Cambio = round((Afluencia/Inicio)*100, digits = 0))


ggplot(data= populares, ##viernes son los dias mas usados
       aes(x=Fecha,
           y = Cambio,
           color = Estacion
       ))+
  geom_line() +
  ylim(0,max(populares$Cambio)+10) 
  

##TODO: LEER mapa de estaciones y crear un dashboard con estacion y grafica de afluencia,
##cada punto tiene el color de la reduccion en afluencia (pesar como normalizar)

##por alcaldia 

por_alcaldia = metro_df1 %>%
  group_by(Alcaldia, Fecha) %>%
  summarise(Afluencia =sum(Afluencia)) %>%
  mutate(Inicio = first(Afluencia),
         Cambio = round((Afluencia/Inicio)*100, digits = 0))



ggplot(data= por_alcaldia, ##viernes son los dias mas usados
       aes(x=Fecha,
           y = Cambio,
           color = Alcaldia
       ))+
  geom_line() +
  ylim(0,max(por_alcaldia$Cambio)+10) +
  facet_wrap(~Alcaldia, ncol = 4) +
  scale_x_date(date_breaks = "3 week",
               date_labels = "%b %d") +
  labs(title = "Cambio en afluencia desde inicio de cuarentena")


ggplot(data= por_alcaldia, ##viernes son los dias mas usados
       aes(x=Fecha,
           y = Cambio,
           color = Alcaldia
       ))+
  geom_line() +
  ylim(0,max(por_alcaldia$Cambio)+10) +
    labs(title = "Cambio en afluencia en cada alcaldia desde inicio de cuarentena")
