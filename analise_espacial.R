library(tidyverse)
library(readxl)
library(data.table)
#library(basedosdados)
#library(dotenv)
#load_dot_env(file = '.env')
#set_billing_id(Sys.getenv("billing_project_id"))
#library(gitignore)
#gi_write_gitignore(gi_fetch_templates("R"))

# Municípios prioritários -------------------------------------------------

municipios_prioritarios <- read_excel("data/municipios_prioritarios.xlsx", 
                                      col_types = c("skip", "text", "numeric", 
                                                    "text", "text", "text", "numeric"),
                                      col_names = c('uf','cod_uf','cod_mun','cod_ibge','municipio','pop'), 
                                      skip = 1)

# População por município -------------------------------------------------

#query <- bdplyr("br_ibge_populacao.municipio")
#pop <- bd_collect(query)
load('data/populacao.RData')
#save(pop, file = 'data/populacao.RData')

# Dados PMAQ --------------------------------------------------------------

pmaq1 <- fread("data/pmaq/PMAQ_1.csv", 
               select = c('IBGE','UF','CONTROLE_UBS','I_13_1', 'I_7_8_4',
                          'I_14_46','I_14_46_1','I_14_47','I_14_47_1',
                          'I_16_1','I_16_2','I_16_3','I_16_4')) %>% 
  tibble()

pmaq2 <- fread("data/pmaq/PMAQ_2.csv", 
               select = c('CNES','IBGE','CIDADE','UF','I_13_2','I_8_6_6',
                          'I_20_3','I_20_3_1','I_20_4','I_20_4_1',
                          'I_15_1','I_15_2','I_15_3','I_15_4')) %>% 
  tibble()


pmaq3 <- fread('data/pmaq/PMAQ_3.csv',
               select = c('ESTADO','IBGE','MUNICIPIO','CNES_FINAL','I.9.2',
                          'I.15.1','I.15.9.3','I.15.9.4',
                          'I.11.1','I.11.2','I.11.3','I.11.4','I.11.5')) %>% 
  tibble()

# Notificações de sífilis -------------------------------------------------

sifilis <- read_csv('data/casos_municipio.csv', na = '-', 
                    col_names = c('municipio', paste('y', 2010:2021, sep = '')), 
                    skip = 1, locale = locale(encoding = "latin2"))


sifilis <- sifilis %>% 
  separate(municipio, into = c('codigo', 'municipio'), sep = " ", extra = 'merge') %>% 
  pivot_longer(cols = y2010:y2021, names_to = 'ano', names_prefix = 'y', values_to = 'casos')

#retirar o dígito verificador do codigo dos municípios do IBGE com 7 algarismos