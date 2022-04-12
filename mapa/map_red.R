
library(tidyverse)
library(leaflet)

clean_dir = "2.clean"
file = "metro.rds"


##read data ------------------------------------------------------------------------------------------

metro = read_rds(file.path(clean_dir, file))
shape = read_rds(file.path(clean_dir, "shape_alcaldias.rds"))


##Mapa
#revisar consistencia en mapa

data_map = metro %>%
  group_by(Estacion) %>%
  mutate(Afluencia = sum(Afluencia, na.rm = T)) %>%
  slice(1) %>%
  ungroup() 


sum(data_map$Afluencia) ==sum(metro$Afluencia, na.rm = T)

labels <- paste0(
  "<center><strong> <font size=4>",data_map$Estacion,"</font></strong></center><br/>",
  '<img src = "', data_map$photo,'" style="display: block;
  margin-left: auto;
  margin-right: auto;
  width: 50%;"> <br>',
  '<strong> <font size=3> Alcaldia: </strong>',data_map$Alcaldia,"</font> <br/>",
  '<strong> <font size=3> Afluencia: </strong>',prettyNum(data_map$Afluencia,big.mark = ","),"</font> <br/>"
  
  
  
)%>% 
  lapply(htmltools::HTML)

leaflet(data = data_map) %>%
  addProviderTiles(providers$ CartoDB.Positron) %>%
  
  addPolygons(data=shape,
              fill = NA,
              color = "gray",
              weight = 1
  ) %>%
  addCircleMarkers(lng = ~lon,
                   lat = ~lat,
                   color = ~colores,
                   weight = 1.5,
                   stroke = T,
                   fillOpacity = .7,
                   popup =  labels,
                   radius = ~.2*(Afluencia/10000000),
                   opacity = 1)


