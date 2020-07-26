### Analisis-COVID-MP2.5
## Datos Covid. Fuente: https://github.com/MinCiencia/Datos-COVID19
## Producto 38: Casos fallecidos por comuna
## PBH Julio 2020


## Descarga de datos ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados
df_muertes <- read_csv(paste(url,"producto38","CasosFallecidosPorComuna_std.csv", sep="/"))
names(df_muertes) <- names(df_muertes) %>% str_to_lower() %>% str_replace_all(" ","_")
df_muertes <- df_muertes %>% na.omit() # limpio NA


## Muertes totales a la fecha -----------
# Filtro fechas mas reciente (dado que son muertes acumuladas)
fecha_muertes <- df_muertes$fecha %>% max()
df_muertes <- df_muertes %>% filter(fecha==fecha_muertes)

# total muertes
df_muertes$casos_fallecidos %>% sum()
df_muertes$poblacion %>% sum()

## Parametros --------
df_muertes <- df_muertes %>% 
  mutate(tasa_mortalidad=casos_fallecidos/poblacion*1e5)

rm(url)
## EoF