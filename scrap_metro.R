library(rvest)
library(stringr)
library(tidyverse)
library(httr)



raw_dir = "1.raw"


url = paste0("https://www.metro.cdmx.gob.mx/la-red/", "linea-2")

print(url)

r = GET(url) ##get html
c = content(r, type="text", encoding = "UTF-8") ## transform to text

photo= unlist(str_extract_all(c,'(?<=app/media/red)(.*)(?=.png)')) 

photo = paste0("https://www.metro.cdmx.gob.mx/storage/app/media/red", photo,".png")


 
photo

photo

##vector con todas las lineas
lineas = sapply(c(c(1:9),"12-2"), function(x){
  x = print(paste("linea",x,sep = "-"))
  return(x)
  
})

lineas = c(lineas, "linea", "linea-b")



lineas
## function to wrap all lineas 

get_alcaldias = function(linea){
  
  url = paste0("https://www.metro.cdmx.gob.mx/la-red/", linea)
 
  print(url)
 
  r = GET(url) ##get html
  c = content(r, type="text", encoding = "UTF-8") ## transform to text
  
  
  ##extract estaciones 
  if(linea %in% c("linea-3", "linea-4",
                  "linea-8", "linea-9"
                  )){
    
    Estaciones = unlist(str_extract_all(c,'(?<=font-weight: 700;">)(.*)(?=</span></a>)')) %>%
      str_remove_all("[a-z]|&|;")
  } 
  
  else if(linea %in% c("linea-5")) {
    
    Estaciones = unlist(str_extract_all(c,'(?<=font-weight: 700;">)(.*)(?=</span>)|(?<=font-weight: 700;">)(.*)(?=</span></a>)')) %>%
      str_remove_all("[a-z]|&|;")
    
    
  } else if (linea %in% c("linea-6")) {
    
    
    Estaciones = unlist(str_extract_all(c,'(?<=font-weight: 700;">)(.*)(?=</span>)|(?<=font-weight: 700;">)(.*)(?=</a>)')) %>%
      str_remove_all("[a-z]|&|;") %>%
      str_remove('< =\"://.../-/-6/\">') %>%
      str_remove('</>')
    
  } else if (linea %in% c("linea-7")){
    
    Estaciones = unlist(str_extract_all(c,'(?<=font-weight: 700;">)(.*)(?=</span>)|(?<=strong>)(.*)(?=</strong></a>)')) %>%
      str_remove('<img src=\"https://metro.cdmx.gob.mx/storage/app/media/red/linea7/barranca_del_muerto.png\" class=\"fr-fic fr-dii\" style=\"max-width: 90%; width: 49px; height: 49px;\"></span><br><span style=\"font-weight: 700;\">')
    
    print(linea)
  } else if (linea %in% c("linea-12-2")){
    
    Estaciones = unlist(str_extract_all(c,'(?<=font-weight: 700;">)(.*)(?=</span></a>)|(?<=strong>)(.*)(?=</strong></a>)')) %>%
      str_remove_all("[a-z]|&|;")
  } else if (linea %in% c("linea", "linea-b")){
    
    Estaciones = unlist(str_extract_all(c,'(?<=font-weight: 700;">)(.*)(?=</span>)|(?<=font-weight: 700;">)(.*)(?=</a>)|(?<=strong>)(.*)(?=</strong></a)')) 
    
    
  }
  
    
 else {
    
    Estaciones = unlist(str_extract_all(c,"(?<=strong>)(.*)(?=</strong></a>)|(?<=strong>)(.*)(?=</strong><br>)")) %>%
      str_remove_all("&nbsp;")
  }
  
  Alcaldias = unlist(str_extract_all(c,"(?<=Alcaldía)(.*)(?=\\.)|(?<=Municipio de)(.*)(?=\\.)|(?<=Estado de México, )(.*)(?=\\.)")) %>%
    str_remove_all("&nbsp;") %>%
    str_trim()
  
  if(linea %in% c("linea-1")) {
      photo = unlist(str_extract_all(c,'(?<=lared)(.*)(?=\"></a>)')) 
      photo = paste0("https://www.metro.cdmx.gob.mx/storage/app/media/lared", photo)
  }
  
  else if(linea %in% c("linea-2")) {
    print("Hola linea 2!")
    photo= unlist(str_extract_all(c,'(?<=app/media/red)(.*)(?=.png)')) 
    photo = paste0("https://www.metro.cdmx.gob.mx/storage/app/media/red", photo,".png")
  }
  else if (linea %in% c("linea-3", "linea-4",
                        "linea-5", "linea-6",
                        "linea-7", "linea-8",
                        "linea-9", "linea-12-2",
                        "linea", "linea-b")) {
    
    photo= unlist(str_extract_all(c,'(?<=app/media/red)(.*)(?=\" class=)')) 
    photo = paste0("https://metro.cdmx.gob.mx/storage/app/media/red", photo) %>%
      str_extract('.*(?<=png)')
    
  }
  else {
    
    photo = NA
  }
  
  print(length(Estaciones))
  print(length(Alcaldias))
  print(length(photo))
  
  table = as.data.frame(cbind(Estaciones, Alcaldias, photo)) %>%
    mutate(Linea = linea)
  
  
  return(table)
  
}


  
alcaldias_list = lapply(lineas, get_alcaldias)
names(alcaldias_list) <- lineas

alcaldias = do.call(rbind, alcaldias_list) 

alcaldias2 =alcaldias %>%
  rename(Estacion = Estaciones,
         Alcaldia = Alcaldias)%>%
  select(Estacion, Alcaldia, Linea, photo ) %>%
  mutate(Estacion = str_to_title(Estacion),
         Estacion = str_replace(Estacion, " De ", " de "),
         Estacion = str_replace(Estacion, " Los ", " los "),
         Estacion = str_replace(Estacion, " La ", " la "),
         Estacion = str_replace(Estacion, " Del ", " del ")) %>%
  group_by(Estacion) %>% ##mantener una observacion por estacion
  slice(1) %>%
  select(Estacion, Alcaldia, photo) %>%
  ungroup() %>%
  mutate(Alcaldia = str_remove(Alcaldia,"s "),
         Alcaldia = str_remove(Alcaldia, '<span style="text-align: left;">'))


##limpiar alcaldias
alcaldias2$Alcaldia[alcaldias2$Alcaldia=="Naucalpan de Juárez, Estado de México"] <- "Naucalpan de Juárez"
alcaldias2$Alcaldia[alcaldias2$Alcaldia=="Municipio de Ecatepec"] <- "Ecatepec"
alcaldias2$Alcaldia[alcaldias2$Alcaldia=="Mpio. LoReyes la Paz"] <- "Los Reyes La Paz"
alcaldias2$Alcaldia[alcaldias2$Alcaldia=="Venustiano Carranza e Iztacalco"] <- "Iztacalco"
alcaldias2$Alcaldia[alcaldias2$Alcaldia=="Gustavo A. Madero y Cuauhtémoc"] <- "Gustavo A. Madero"
alcaldias2$Alcaldia[alcaldias2$Alcaldia=="Álvaro Obregón y Coyoacán"] <- "Álvaro Obregón"
alcaldias2$Alcaldia[alcaldias2$Alcaldia=="Benito Juárez e Iztacalco"] <- "Iztacalco"
alcaldias2$Alcaldia[alcaldias2$Alcaldia=="Benito Juárez y Álvaro Obregón"] <- "Álvaro Obregón"
alcaldias2$Alcaldia[alcaldias2$Alcaldia=="Gustavo A. Madero y Venustiano Carranza"] <- "Gustavo A. Madero"
alcaldias2$Alcaldia[alcaldias2$Alcaldia=="Cuahtémoc"] <- "Cuauhtémoc"



##limpiar estacion para hacerlo consistente con datos abiertos
alcaldias2$Estacion[alcaldias2$Estacion=="Hospital 20 de Noviembre"] <-  "20 de Noviembre"
alcaldias2$Estacion[alcaldias2$Estacion=="Gómez Farias"] <-  "Gómez Farías"
alcaldias2$Estacion[alcaldias2$Estacion=="Boulevard Puerto Aéreo"] <-  "Blvd. Puerto Aéreo"
alcaldias2$Estacion[alcaldias2$Estacion=="Villa de Cortes"] <-  "Villa de Cortés"
alcaldias2$Estacion[alcaldias2$Estacion=="Miguel Ángel de Quevedo"] <-  "Miguel A. de Quevedo"
alcaldias2$Estacion[alcaldias2$Estacion=="Tezozómoc"] <-  "Tezozomoc"
alcaldias2$Estacion[alcaldias2$Estacion=="Instituto del Petróleo"] <-  "Inst. del Petróleo"
alcaldias2$Estacion[alcaldias2$Estacion=="Deportivo 18 de Marzo"] <-  "Deptvo. 18 de Marzo"
alcaldias2$Estacion[alcaldias2$Estacion=="Garibaldi/ Lagunilla"] <-  "Garibaldi"
alcaldias2$Estacion[alcaldias2$Estacion=="Uam-I"] <-  "U A M  I"
alcaldias2$Estacion[alcaldias2$Estacion=="Olimpica"] <-  "Olímpica"
alcaldias2$Estacion[alcaldias2$Estacion=="La Villa/ Basílica"] <-  "La Villa-Basilica"
alcaldias2$Estacion[alcaldias2$Estacion=="Etiopía/ Plaza de la Transparencia"] <-  "Etiopía"
alcaldias2$Estacion[alcaldias2$Estacion=="Viveros/ Derechos Humanos"] <-  "Viveros"
alcaldias2$Estacion[alcaldias2$Estacion=="Uam Azcapotzalco"] <-  "Azcapotzalco"
alcaldias2$Estacion[alcaldias2$Estacion=="Ferrería/ Arena Ciudad de México"] <-  "Ferrería"
alcaldias2$Estacion[alcaldias2$Estacion=="San Pedro de los Pinos"] <-  "San Pedro los Pinos"
alcaldias2$Estacion[alcaldias2$Estacion=="San Juan de Letrán"] <-  "San Juan Letrán"



 
##guardar base limpia
write_rds(alcaldias2,file.path(raw_dir, "estacion_alcadias.rds"))

          
          