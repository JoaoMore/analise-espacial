library(tidyverse)
library(gghighlight)
library(clipr)
theme_set(theme_minimal())
color <- 'dodgerblue3'
update_geom_defaults("point",   list(color = color))
update_geom_defaults("line",   list(color = color))
update_geom_defaults("boxplot",   list(fill = color))
update_geom_defaults("col",   list(fill = color))
update_geom_defaults("bar",   list(fill = color))

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
       subtitle = 'Municipios com média acima de 100 estão em destaque', 
       x = NULL, y = 'Casos/100 mil habitantes')

ggsave(width = altura, height = comprimento, scale = escala, 
       filename = 'evolução_100k.png', path = 'out/plots/')


# Unidades de saude nos municipios ----------------------------------------

unidades_mun <- read_csv('out/data/unidades_por_municipio.csv')

unidades_mun %>% 
  mutate(cat = cut(n, breaks = c(0,50,100,150, Inf), labels = c('0-50', '51-100', '101-150','150+'))) %>% 
  group_by(cat) %>% 
  count() %>%
  write_clip()
  ggplot() +
  geom_col(aes(cat, n)) +
  geom_text(aes(x = cat, y = n, label = n), nudge_y = 2, fontface = 'bold') +
  labs(title = 'Categorização dos municípios por quantidade de unidades de saúde', 
       x = 'Unidades de saúde', y = 'N° de municípios')

ggsave(width = altura, height = comprimento, scale = escala, 
       filename = 'unidades_municipio_cat.png', path = 'out/plots/')

municipios_prioritarios %>% 
  left_join(., unidades_mun) %>% 
  mutate(ubs = round(n*1e5/pop, 0)) %>%
  arrange(-ubs) %>% 
  filter( ubs < 100) %>% 
  select(uf, municipio, pop, n, ubs) %>% 
  ggplot() +
  geom_point(aes(pop, n)) +
  labs(title = 'Número de unidades de saúde x população',
       x = 'População', y = 'Unidades de Saúde')

ggsave(width = altura, height = comprimento, scale = escala, 
       filename = 'pop_x_unidades.png', path = 'out/plots/')

municipios_prioritarios %>% 
  left_join(., unidades_mun) %>% 
  mutate(ubs = round(n*1e5/pop, 0)) %>%
  arrange(-ubs) %>% 
  filter( ubs < 100) %>% 
  select(uf, municipio, pop, n, ubs) %>% 
  ggplot() +
  geom_point(aes(pop, ubs)) +
  labs(title = 'Número de unidades de saúde/100 mil hab. x população',
       x = 'População', y = 'Unidades de Saúde/100 mil hab.')

ggsave(width = altura, height = comprimento, scale = escala, 
       filename = 'pop_x_ubs.png', path = 'out/plots/')


# Cobertura assistencial --------------------------------------------------

cobertura <- read_csv('out/data/cobertura.csv') %>% 
  mutate(cobertura_ab = str_remove(`Cobertura AB`, '%') %>% str_replace(',', '.') %>% as.numeric())

cobertura %>% 
  ggplot(aes(Competência, cobertura_ab, group = Município, color = Município)) +
  geom_line() +
  gghighlight(max(cobertura_ab, na.rm = T) < 40, max_highlight = 10) +
  labs(title = 'Cobertura da atenção básica', 
       subtitle = 'Municípios destacados nunca ultrapassaram 40% de cobertura',
       x = NULL, y = 'Cobertura (%)')

ggsave(width = altura, height = comprimento, scale = escala, 
       filename = 'cobertura.png', path = 'out/plots/')