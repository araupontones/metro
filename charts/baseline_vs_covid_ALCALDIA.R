library(tidyverse)
library(plotly)


afluencia = read_rds("2.clean/metro.rds")
#promedio = read_rds("2.clean/promedio.rds")
start_covid = "2020-03-10" ##fecha desde comienzo de pandemia
anos = c(2015,2016,2017, 2018, 2019)
meses = c(1:3)


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


##crear datos ------------------------------------------------------------------
covid = afluencia %>%
  filter(Fecha >= start_covid) %>%
  group_by(Alcaldia, dia, Fecha) %>%
  summarise(Afluencia =mean(Afluencia),
            colores = first(colores)) %>%
  ungroup()

names(afluencia)
promedio = afluencia %>%
  filter(Año %in% anos, Mes %in% meses) %>%
  group_by(Alcaldia,dia) %>%
  summarise(Promedio = mean(Afluencia, na.rm = T)) %>%
  ungroup() 


diferencia = left_join(covid, promedio) %>%
  mutate(Cambio = round((Afluencia/Promedio)*100, digits = 0))

diferencia$Cambio <- log(diferencia$Cambio, base = 10)


##crear graficas ----------------------------------------------------------------------
tema = theme(panel.background = element_blank(),
             panel.grid.major.y = element_line(colour = "#EBECF0"),
             axis.text.x = element_text(angle = 90, hjust = 1))


ch = ggplot(data= diferencia, ##viernes son los dias mas usados
       aes(x=Fecha,
           y = Cambio
       ))+
  
  ##linea
  geom_line(color = "#4B8AF4",
            size = 1) +
  #area entre line y baseline
  geom_ribbon(aes(ymin= 100,
                   ymax= Cambio),
               fill = "#D9E6FC") +
  scale_y_continuous(breaks = c(50,100, 150),
                labels = c("-50%", "Baseline", "+50%")) +
 
  
  facet_wrap(~Alcaldia, ncol = 3) +
  labs(title = "Cambio en afluencia diaria del Metro por Alcaldía desde desde 2 de marzo") +
  tema +
  scale_x_date(date_breaks = "1 week",
                date_labels = "%d %b") +
  xlab("") +
  ylab("")
  
  

ggplotly(ch)

log(100)
log(100, base = 10)
##linea 

tema2 = theme(panel.background = element_blank(),
             panel.grid.major.y = element_line(colour = "#EBECF0"),
             panel.grid.major.x = element_line(colour = "#EBECF0"),
             legend.position = "none")


labels = diferencia %>%
  group_by(Alcaldia) %>%
  arrange(Fecha) %>%
  slice(n())

library(ggrepel)
library(lubridate)
base = 10

p = ggplot(data= diferencia, ##viernes son los dias mas usados
            aes(x=Fecha,
                y = Cambio,
                color = Alcaldia)
            )+
  
  geom_line(size = .3) +
  tema2
  
  
 CH = p+
  geom_point(data = labels,
             aes(x=Fecha,
                 y = Cambio)
             ) +
  geom_text_repel(data = labels,
                  aes(x=Fecha,
                      y = Cambio,
                      label = Alcaldia),
                  nudge_x = 4,
                  direction = "y",
                  hjust =0,
                  segment.size = .2,
                  segment.color = "grey50"
                  ) +
  
   xlim(min(diferencia$Fecha), (max(diferencia$Fecha)+6))+

  scale_y_continuous(breaks = c(log(120, base = 10),
                                log(100, base =10),
                                log(80, base = 10),
                                log(60, base = 10),
                                log(30, base = 10),
                                log(20, base = 10)),
                     labels = c("120", "Baseline", "-20%", "-40%", "-70%", "-80%"))
 
 
 CH
  #                    )+
  # scale_x_date(breaks = "3 weeks") +
  # #                    labels = c("-80%","-40%", "Baseline", "+40%",  "+80%")) +
  tema2


CH
ggplotly(CH)
