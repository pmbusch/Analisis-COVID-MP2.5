### Analisis-COVID-MP2.5
## Agrego datos de mortalidad
## PBH Julio 2020

# Carga datos brutos --------
source("Scripts/Load_Data/tasaMortalidad_load.R", encoding = "UTF-8")


## Calculo agregado de muertes ---------
df_defunciones <- df_defunciones %>% 
  mutate(year=year(fecha))

# Defunciones por año a nivel nacional
df_defunciones %>% group_by(year) %>% summarise(muertes=sum(defunciones,na.rm=T))

# Defunciones año 2017 a nivel Comunal
df_defunciones <- df_defunciones %>% 
  filter(year==2017) %>% 
  group_by(codigo_comuna) %>% 
  summarise(defunciones=sum(defunciones,na.rm = T))

# Agrego poblacion de Chile Mapas
df_pob <- censo_2017_comunas %>% group_by(codigo_comuna) %>% 
  summarise(pob=sum(poblacion, na.rm=T))
df_pob$pob %>% sum()

df_tasaMortalidad <- df_defunciones %>% 
  left_join(df_pob) %>% 
  mutate(tasa_mortalidad_all=defunciones/pob*1e5) %>% 
  left_join(codigos_territoriales) %>% 
  select(codigo_comuna, defunciones, tasa_mortalidad_all)

rm(url, df_pob, df_defunciones)

## EoF