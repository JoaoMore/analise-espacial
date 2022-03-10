library(tidyverse)
library(readxl)
library(data.table)
#library(basedosdados)
#library(dotenv)
#load_dot_env(file = '.env')
#set_billing_id(Sys.getenv("billing_project_id"))
#library(gitignore)
#gi_write_gitignore(gi_fetch_templates("R"))
theme_set(theme_minimal())

# Municípios prioritários -------------------------------------------------

municipios_prioritarios <- read_excel("data/municipios_prioritarios.xlsx", 
                                      col_types = c("skip", "text", "numeric", 
                                                    "text", "text", "text", "numeric"),
                                      col_names = c('uf','cod_uf','cod_mun','cod_ibge','municipio','pop'), 
                                      skip = 1)

municipios_prioritarios <-  municipios_prioritarios %>% 
  mutate(cod_ibge = str_sub(cod_ibge, end = 6))

# Cobertura da atenção básica ---------------------------------------------

cobertura <- list()
i <- 1

for (name in list.files('data/cobertura/')) {
  
  cobertura[[i]] <- read_excel(paste('data/cobertura/', name, sep = ''), 
                               skip = 7) %>% 
    .[1:(which(.$Competência == "Fonte: e-Gestor Atenção Básica") - 1), ]
  
  i <- i+1
  
}

cobertura <- bind_rows(cobertura) %>% 
  mutate(Competência = str_sub(Competência, 5),
         IBGE = as.character(IBGE)) %>% 
  filter(IBGE %in% municipios_prioritarios$cod_ibge)

#save(cobertura, file = 'data/cobertura/cobertura.RData')
load('data/cobertura/cobertura.RData')

# Dados CNES --------------------------------------------------------------

load('data/cnes.RData')

# População por município -------------------------------------------------

#query <- bdplyr("br_ibge_populacao.municipio")
#pop <- bd_collect(query)
load('data/populacao.RData')
#save(pop, file = 'data/populacao.RData')
pop <- pop %>% 
  mutate(id_municipio = str_sub(id_municipio, end = 6),
         ano = as.character(ano)) %>% 
  filter(id_municipio %in% municipios_prioritarios$cod_ibge)

# Dados PMAQ --------------------------------------------------------------

pmaq1 <- fread("data/pmaq/PMAQ_1.csv", 
               select = c('IBGE','UF','CONTROLE_UBS','I_13_1', 'I_7_8_4',
                          'I_14_46','I_14_46_1','I_14_47','I_14_47_1',
                          'I_16_1','I_16_2','I_16_3','I_16_4')) %>% 
  tibble()

pmaq2 <- fread("data/pmaq/PMAQ_2.csv", 
               select = c('Aplicação_AE', 'Motivo', 'CNES','IBGE','CIDADE','UF','I_13_2','I_8_6_6',
                          'I_20_3','I_20_3_1','I_20_4','I_20_4_1',
                          'I_15_1','I_15_2','I_15_3','I_15_4')) %>% 
  tibble()


pmaq3 <- fread('data/pmaq/PMAQ_3.csv',
               select = c('APLICADO_UBS','MOTIVO_NÃO_APLICAÇÃO','ESTADO',
                          'IBGE','MUNICIPIO','CNES_FINAL','I.9.2',
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


# Numero de infectados por 100k habitantes --------------------------------

sifilis %>% 
  filter(codigo %in% municipios_prioritarios$cod_ibge) %>% 
  left_join(., pop, by = c('codigo' = 'id_municipio', 'ano' = 'ano')) %>% 
  mutate(casos100k = casos*100000/populacao) %>% 
  write_csv(., file = 'out/data/casos100k.csv')

sifilis %>% 
  filter(codigo %in% municipios_prioritarios$cod_ibge) %>% 
  left_join(., pop, by = c('codigo' = 'id_municipio', 'ano' = 'ano')) %>% 
  mutate(casos100k = casos*100000/populacao) %>% 
  ggplot(aes(x = as.numeric(ano), y = casos100k, grp = codigo)) +
  geom_line(alpha = 0.3, color = 'dodgerblue') +
  labs(title = 'Casos por 100 mil habitantes nos municípios prioritários' ,
       x = NULL, y = 'Casos/100 mil hab.')


# Unidades de saúde no município ------------------------------------------

cnes %>% 
  filter(CO_MUNICIPIO_GESTOR %in% municipios_prioritarios$cod_ibge,
         CO_TIPO_ESTABELECIMENTO == 1) %>% 
  left_join(., municipios_prioritarios, by = c('CO_MUNICIPIO_GESTOR' = 'cod_ibge')) %>% 
  group_by(municipio) %>% 
  count() %>% 
  write_csv(., file = 'out/data/unidades_por_municipio.csv')


# Cobertura da atenção básica ---------------------------------------------

write_csv(cobertura, file = 'out/data/cobertura.csv')

# Quantidade de unidades de saúde que participaram do PMAQ ----------------


