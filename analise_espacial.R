library(tidyverse)
library(data.table)
#library(gitignore)

#gi_write_gitignore(gi_fetch_templates("R"))


pmaq1 <- fread("data/PMAQ_1.csv", 
               select = c('IBGE','UF','CONTROLE_UBS','I_13_1', 'I_7_8_4',
                          'I_14_46','I_14_46_1','I_14_47','I_14_47_1',
                          'I_16_1','I_16_2','I_16_3','I_16_4'))

pmaq2 <- fread("data/PMAQ_2.csv", 
               select = c('CNES','IBGE','CIDADE','UF','I_13_2','I_8_6_6',
                          'I_20_3','I_20_3_1','I_20_4','I_20_4_1',
                          'I_15_1','I_15_2','I_15_3','I_15_4'))


pmaq3 <- fread('data/PMAQ_3.csv',
               select = c('ESTADO','IBGE','MUNICIPIO','CNES_FINAL','I_9_2',
                          'I_15_1','I_15_9_3','I_15_9_4',
                          'I_11_1','I_11_2','I_11_3','I_11_4','I_11_5'))