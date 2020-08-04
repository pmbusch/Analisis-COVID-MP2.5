### Analisis-COVID-MP2.5
## Datos Covid. Fuente: https://github.com/MinCiencia/Datos-COVID19
## Producto 1: Casos totales por comuna incremental
## PBH Julio 2020


## Descarga de datos ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados
df_casos <- read_csv(paste(url,"producto1","Covid-19_std.csv", sep="/"))
names(df_casos) <- names(df_casos) %>% str_to_lower() %>% str_replace_all(" ","_")
df_casos <- df_casos %>% na.omit() # limpio NA

## Dia primer contagio por comuna ---------------
# Limitado por fecha de primer reporte 30-03-2020
df_contagioZero <- df_casos %>% 
  filter(casos_confirmados!=0) %>% #Filtro que haya casos
  group_by(codigo_comuna,comuna) %>% 
  summarise(dia_contagioZero=min(fecha, na.rm=T)) %>% ungroup()


## Casos totales a la fecha -----------
# Filtro fechas mas reciente
fecha_casos <- df_casos$fecha %>% max()
df_casos <- df_casos %>% filter(fecha==fecha_casos)

# total casos confirmados
df_casos$casos_confirmados %>% sum()
df_casos$poblacion %>% sum()

# Poblacion -----
## Utilizo poblacion de Chilemapas (censo 2017)
pob <- censo_2017_comunas %>% group_by(codigo_comuna) %>% 
  summarise(poblacion=sum(poblacion, na.rm=T)) %>% ungroup()

df_casos <- df_casos %>% select(-poblacion) %>% 
  left_join(pob)
df_casos$poblacion %>% sum()
rm(pob)



## Parametros --------
df_casos <- df_casos %>% 
  mutate(tasa_contagios=casos_confirmados/poblacion*1e5) %>% 
  left_join(df_contagioZero %>% select(-comuna)) %>% 
  mutate(dias_primerContagio=(fecha-dia_contagioZero) %>%
           as.numeric(units="days")) %>% 
  select(codigo_comuna, casos_confirmados, tasa_contagios, 
         dias_primerContagio)

rm(url, df_contagioZero)
## EoF