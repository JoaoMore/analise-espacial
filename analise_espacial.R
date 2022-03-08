library(tidyverse)
library(data.table)
library(gitignore)

gi_write_gitignore(gi_fetch_templates("R"))


pmaq1 <- fread("data/PMAQ_1.csv", 
               select = c('IBGE','CONTROLE_UBS','I_13_1', 'I_7_8_4',
                          'I_14_46','I_14_46_1','I_14_47','I_14_47_1',
                          'I_16_1','I_16_2','I_16_3','I_16_4'))

pma

