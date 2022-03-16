library(tidyverse)
library(gghighlight)
theme_set(theme_minimal())
update_geom_defaults("boxplot",   list(fill = "blue"))


# Infectados por 100k -----------------------------------------------------

infec <- read_csv("out/data/casos100k.csv", 
                  locale = locale(decimal_mark = ","))

infec %>% 
  ggplot(aes(x = factor(ano), y = casos100k)) +
  geom_boxplot() 
