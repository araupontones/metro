library(tidyverse)
library(plotly)


afluencia = read_rds("2.clean/metro.rds")
promedio = read_rds("2.clean/promedio.rds")
start_covid = "2020-03-02" ##fecha desde comienzo de pandemia

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


covid = afluencia %>%
  filter(Fecha >= start_covid) %>%
  group_by(Linea,dia, Fecha) %>%
  summarise(Afluencia =sum(Afluencia),
            colores = first(colores)) %>%
  ungroup()

promedio2 = promedio %>%
  group_by(Linea, dia) %>%
  summarise(Promedio = sum(Promedio))


t = left_join(covid, promedio2) %>%
  mutate(Cambio = round((Afluencia/Promedio)*100, digits = 0))

tema = theme(panel.background = element_blank(),
             panel.grid.major.y = element_line(colour = "#EBECF0"),
             legend.position = "none",
             axis.text.x = element_text(angle = 90, hjust = 1))

ch = ggplot(data= t, ##viernes son los dias mas usados
       aes(x=Fecha,
           y = Cambio,
           color = factor(Linea)
       ))+
  geom_line() +
  geom_ribbon(aes(ymin= 100,
                  ymax= Cambio,
                  fill = Linea
                  ),
              
              ) +
  
  scale_y_continuous(breaks = c(20,60,100,180),
                     labels = c("-80%","-40%", "Baseline", "+20%")) +
  
  facet_wrap(~Linea, ncol = 3) +
 
  scale_color_manual(values=colores) +
  scale_fill_manual(values = alpha(colores,.7))+
  labs(title = "Cambio en afluencia desde 2 de marzo")+
  tema +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%d %b") +
  xlab("") +
  ylab("")
 

ggplotly(ch)
