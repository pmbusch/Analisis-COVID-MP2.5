### Analisis-COVID-MP2.5
## CASEN: Datos socioeconomicos
## PBH Julio 2020

library(casen)
library(spatstat) #weighted median

# Ejecutar solo para descargar datos
# casen::descargar_casen_github(anios=2017, carpeta = "Data/Data Modelo/Casen")
df_casen <- read_rds("Data/Data_Modelo/Casen/2017.rds")
df_casen %>% names()

## Para homologar codigos comunales, debo agregar un 0 a las regiones (ej: 01)
df_casen <- df_casen %>% 
  mutate(comuna=paste(if_else(str_length(comuna)==4,"0",""),
                      comuna,sep=""))

# Factor expansion
df_casen$expc %>% sum()
df_casen$expr %>% sum()


## INGRESO -------
# ytotcor: Ingreso total corregido
# yautcor: Ingreso autónomo corregido
df_ingreso <- df_casen %>% 
  group_by(comuna) %>% 
  summarise(ingresoTotal_media=weighted.mean(ytotcor,w = expc,na.rm=T),
            ingresoTotal_mediana=weighted.median(ytotcor, w = expc,na.rm=T),
            ingresoAutonomo_media=weighted.mean(yautcor,w = expc,na.rm=T),
            ingresoAutonomo_mediana=weighted.median(yautcor, w = expc,na.rm=T)) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)

## EDUCACION ------------
# e6a: Cuál fue el nivel educacional más alto alcanzado o el nivel educacional actual
df_codigoEducacion <- read_excel("Data/Data_Modelo/Casen/Codigos_CASEN.xlsx", sheet = "e6a")
df_educacion <- df_casen %>% 
  group_by(comuna,e6a) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoEducacion, by=c("e6a"="codigo")) %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)


## SALUD PREVISION -------
# s12: A qué sistema previsional de salud pertenece usted
df_codigoSalud <- read_excel("Data/Data_Modelo/Casen/Codigos_CASEN.xlsx", sheet = "s12")
df_prevision <- df_casen %>% 
  group_by(comuna,s12) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoSalud, by=c("s12"="codigo")) %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)


## OCUPADOS -----------
# o9a: o9a. ¿Cuál es su ocupación u oficio?
# o1: La semana pasada, ¿trabajó al menos una hora, sin considerar los quehaceres del hogar?
df_ocupacion <- df_casen %>% 
  filter(!is.na(o1)) %>% 
  group_by(comuna,o1) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab),
         o1=if_else(o1==1,"Si","No")) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)
# Dejo porcentaje ocupacion
df_ocupacion <- df_ocupacion %>% filter(o1=="Si") %>% rename(perc_ocupado=perc)

## EoF