library(tidyverse)
library(gghighlight)
theme_set(theme_minimal())
color <- 'dodgerblue3'
update_geom_defaults("boxplot",   list(fill = color))
update_geom_defaults("col",   list(fill = color))
update_geom_defaults("bar",   list(fill = color, color = 'gray80'))

# Infectados por 100k -----------------------------------------------------

infec <- read_csv("out/data/casos100k.csv", 
                  locale = locale(decimal_mark = ","))

## Histograma ---- 
infec %>% 
  ggplot(aes(casos100k)) +
  geom_histogram() +
  labs(title = 'Histograma de casos por 100 mil habitantes')

ggsave(filename = 'histograma_100k.png', width = 4.5, height = 2.5, scale = 2, path = 'out/plots/')

## boxplot ----
infec %>% 
  ggplot(aes(x = factor(ano), y = casos100k)) +
  geom_boxplot() +
  labs(x = NULL, y = 'Casos por 100 mil habitantes', 
       title = 'Boxplot de casos por 100 mil habitantes, por ano')

ggsave(filename = 'boxplot_100k.png', width = 4.5, height = 2.5, scale = 2, path = 'out/plots/')
