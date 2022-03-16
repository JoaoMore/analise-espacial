library(tidyverse)
library(gghighlight)
theme_set(theme_minimal())
color <- 'dodgerblue3'
update_geom_defaults("line",   list(color = color))
update_geom_defaults("boxplot",   list(fill = color))
update_geom_defaults("col",   list(fill = color))
update_geom_defaults("bar",   list(fill = color, color = 'gray80'))

altura <- 4.5
comprimento <- 2.5
escala <- 2

# Infectados por 100k -----------------------------------------------------

infec <- read_csv("out/data/casos100k.csv", 
                  locale = locale(decimal_mark = ","))

## Histograma ---- 
infec %>% 
  ggplot(aes(casos100k)) +
  geom_histogram() +
  labs(title = 'Histograma de casos por 100 mil habitantes')

ggsave(width = altura, height = comprimento, scale = escala, 
       filename = 'histograma_100k.png', path = 'out/plots/')

## boxplot ----
infec %>% 
  ggplot(aes(x = factor(ano), y = casos100k)) +
  geom_boxplot() +
  labs(x = NULL, y = 'Casos por 100 mil habitantes', 
       title = 'Boxplot de casos por 100 mil habitantes, por ano')

ggsave(width = altura, height = comprimento, scale = escala, 
       filename = 'boxplot_100k.png', path = 'out/plots/')

## Evolução do numero de casos ----

infec %>% 
  ggplot(aes(ano, casos100k, col = municipio)) +
  geom_line() +
  gghighlight(mean(casos100k) > 100) +
  labs(title = 'Evolução do número de casos por 100 mil habitantes', 
       subtitle = 'Municipios com média acima de 100', x = NULL, y = 'Casos/100 mil habitantes')

ggsave(width = altura, height = comprimento, scale = escala, 
       filename = 'evolução_100k.png', path = 'out/plots/')
