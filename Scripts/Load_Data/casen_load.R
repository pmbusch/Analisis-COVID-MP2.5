### Analisis-COVID-MP2.5
## CASEN: Datos socioeconomicos
## PBH Julio 2020

library(casen)
library(spatstat) #weighted median

# Ejecutar solo para descargar datos
# casen::descargar_casen_github(anios=2015, carpeta = "Data/Data_Original/Casen")
df_casen <- read_rds("Data/Data_Original/Casen/2017.rds")
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
df_codigoEducacion <- read_excel("Data/Data_Original/Casen/Codigos_CASEN.xlsx", sheet = "e6a")
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
df_codigoSalud <- read_excel("Data/Data_Original/Casen/Codigos_CASEN.xlsx", sheet = "s12")
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
  mutate(perc=hab/sum(hab)*100,
         o1=if_else(o1==1,"Si","No")) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)
# Dejo porcentaje ocupacion
df_ocupacion <- df_ocupacion %>% filter(o1=="Si") %>% rename(perc_ocupado=perc)


## LEÑA 2015 -----------
# Obtenidos de la casen 2015
df_casen2015 <- read_rds("Data/Data_Original/Casen/2015.rds")
df_casen2015 %>% names()

# Pasar a codigos subdere_2017

# Dejo un la tupla validoHasta-Codigo_casen mas reciente
codigos_casen_recientes <- codigos_casen %>% 
  group_by(codigo_casen) %>% summarise(valido_hasta=max(valido_hasta, na.rm=T)) %>% 
  ungroup() %>% 
  left_join(codigos_casen, by=c("valido_hasta","codigo_casen")) %>% 
  rename(comuna=codigo_casen, codigo_comuna=codigo_subdere_2017)

df_casen2015 <- df_casen2015 %>% 
  left_join(codigos_casen_recientes, by=c("comuna"))

# Factor expansion
df_casen2015$expc %>% sum(na.rm=T)
df_casen2015$expr %>% sum(na.rm=T)

# Existen varias comunas sin factor de expansion comunal, por defecto lo reemplazo por 1
# df_casen2015 %>% filter(codigo_comuna=="01402") %>% select(codigo_comuna, expc) %>% view
df_casen2015 <- df_casen2015 %>% 
  mutate(expc=if_else(is.na(expc),1,expc))


## Datos leña
# v36a: Qué combustible o fuente de energía usa habitualmente para: Cocinar 
# v36b: Idem Calefacción
# v36c: Idem Sistema de Agua Caliente
# Opción 3: Leña o derivados (pellets, astillas o briquetas)
# Sum if si corresponde a la opcion de Leña
df_lena_casen <- df_casen2015 %>% 
  group_by(codigo_comuna) %>% 
  summarise(hab=sum(expc, na.rm=T),
            lena_cocina=sum(if_else(v36a==3,expc,0),na.rm=T),
            lena_calefaccion=sum(if_else(v36b==3,expc,0),na.rm=T),
            lena_agua=sum(if_else(v36c==3,expc,0),na.rm=T)) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("codigo_comuna"))

rm(codigos_casen_recientes, df_casen2015)

## EoF