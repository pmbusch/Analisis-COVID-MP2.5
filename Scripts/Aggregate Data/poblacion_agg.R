### Analisis-COVID-MP2.5
## Agrega poblacion a nivel comunal
## PBH Julio 2020

# Carga datos brutos --------
source("Scripts/Load Data/mapa_load.R", encoding = "UTF-8")

## Dividir en grupos etarios -----------
df_poblacion$edad %>% unique()
df_poblacion <- df_poblacion %>% 
  mutate(grupo_edad=case_when(
    edad %in% c("0 a 4","5 a 9","10 a 14") ~ "0-14",
    edad %in% c("15 a 19","20 a 24","25 a 29",
                "30 a 34","35 a 39","40 a 44") ~ "15-44",
    edad %in% c("45 a 49","50 a 54","55 a 59","60 a 64") ~ "45-64",
    T ~ "65+"))

df_edad <- df_poblacion %>% 
  group_by(codigo_comuna, grupo_edad) %>% 
  summarise(pob=sum(poblacion,na.rm=T)) %>% 
  mutate(porc_edad=pob/sum(pob),
         pob=NULL) %>% ungroup() %>% 
  filter(grupo_edad!="0-14") %>% spread(grupo_edad,porc_edad)

## Dividir por sexo
df_sexo <- df_poblacion %>% 
  group_by(codigo_comuna, sexo) %>% 
  summarise(pob=sum(poblacion,na.rm=T)) %>% 
  mutate(porc=pob/sum(pob),
         pob=NULL) %>% ungroup() %>% 
  filter(sexo!="hombre") %>% spread(sexo,porc)


## Poblacion por comuna ----------
df_poblacion <- df_poblacion %>% 
  group_by(codigo_comuna) %>% 
  summarise(poblacion=sum(poblacion,na.rm=T))

# Juntar todo -----
df_poblacion <- left_join(df_poblacion, df_edad) %>% 
  left_join(df_sexo) %>% 
  left_join(codigos_territoriales)

# Limpiar WS
rm(df_edad,df_sexo)


## Agregar codigos regionales y datos de superficie ----------
df_poblacion <- df_poblacion %>% 
  left_join(mapa_regiones %>% select(codigo_region, region)) %>% 
  select(-geometry) %>% 
  left_join(mapa_comuna %>% select(codigo_comuna, superficie, perimetro))
  
## EoF