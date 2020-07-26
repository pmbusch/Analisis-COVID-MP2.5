### Analisis-COVID-MP2.5
## Datos Covid. Fuente: https://github.com/MinCiencia/Datos-COVID19
## Producto 32: Defunciones en Chile
## PBH Julio 2020

## Descarga de datos ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados
df_defunciones <- read_csv(paste(url,"producto32","Defunciones_std.csv", sep="/"))
names(df_defunciones) <- names(df_defunciones) %>% str_to_lower() %>% str_replace_all(" ","_")
df_defunciones <- df_defunciones %>% na.omit() # limpio NA

# Para homologar codigos comunales, debo agregar un 0 a las regiones (ej: 01)
df_defunciones <- df_defunciones %>% 
  mutate(codigo_comuna=paste(if_else(str_length(codigo_comuna)==4,"0",""),
                             codigo_comuna,sep=""))

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
  mutate(tasa_mortalidad=defunciones/pob*1e5) %>% 
  left_join(codigos_territoriales)


rm(url, df_pob, df_defunciones)
## EoF