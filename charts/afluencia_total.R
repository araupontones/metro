library(tidyverse)
library(plotly)

afluencia = read_rds("2.clean/metro.rds")

names(afluencia)

date_since = "2020-01-05" ##fecha para empezar el analisis

data_total = afluencia %>%
  filter(Fecha >=date_since) %>%
  group_by(Fecha) %>%
  summarise(Afluencia = sum(Afluencia)/1000000) 
  


ch = ggplot(data = data_total,
       aes(
       x= Fecha,
       y = Afluencia)) +
  geom_line() +
  ylim(0,8)

ggplotly(ch)

