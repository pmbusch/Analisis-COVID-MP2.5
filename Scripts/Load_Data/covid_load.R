### Analisis-COVID-MP2.5
## Datos Covid. Fuente: https://github.com/MinCiencia/Datos-COVID19
## Este script crea unan base de datos de datos comunales generales y de COVID
## Script basado en codigo de DQV.
## PBH Julio 2020

## Descarga de datos ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados

## Producto 1: Casos totales por comuna incremental
df_casos <- read_csv(paste(url,"producto1","Covid-19_std.csv", sep="/"))
names(df_casos) <- names(df_casos) %>% str_to_lower() %>% str_replace_all(" ","_")
df_casos <- df_casos %>% na.omit() # limpio NA

## Producto 38: Casos fallecidos por comuna
df_muertes <- read_csv(paste(url,"producto38","CasosFallecidosPorComuna_std.csv", sep="/"))
names(df_muertes) <- names(df_muertes) %>% str_to_lower() %>% str_replace_all(" ","_")
df_muertes <- df_muertes %>% na.omit() # limpio NA

## Producto 7: Exámenes PCR por región
df_pcr <- read_csv(paste(url,"producto7","PCR_std.csv", sep="/"))
names(df_pcr) <- names(df_pcr) %>% str_to_lower() %>% str_replace_all(" ","_")
df_pcr <- df_pcr %>% na.omit() # limpio NA

## Producto 29: Cuarentenas Activas e Históricas
df_cuarentena <- read_csv(paste(url,"producto29","Cuarentenas-Totales.csv", sep="/"))
names(df_cuarentena) <- names(df_cuarentena) %>% str_to_lower() %>% str_replace_all(" ","_")
df_cuarentena <- df_cuarentena %>% na.omit() # limpio NA

## Producto 32: Defunciones en Chile
df_defunciones <- read_csv(paste(url,"producto32","Defunciones_std.csv", sep="/"))
names(df_defunciones) <- names(df_defunciones) %>% str_to_lower() %>% str_replace_all(" ","_")
df_defunciones <- df_defunciones %>% na.omit() # limpio NA

## Producto 36: Residencias Sanitarias
df_residencias <- read_csv(paste(url,"producto36","ResidenciasSanitarias_std.csv", sep="/"))
names(df_residencias) <- names(df_residencias) %>% str_to_lower() %>% str_replace_all(" ","_")
df_residencias <- df_residencias %>% na.omit() # limpio NA


## Manipulacion datos ----------





# junto datos de muertes y casos
df_covid <-full_join(df_casos, 
                     df_muertes %>% select(-Region,-`Codigo region`,-Comuna, -Poblacion), 
                     by=c("Codigo comuna","date"))



# limpia comunas desconocidas, totales
df_covid <- df_covid %>% filter(!(Comuna %>% str_detect("Desconocido")))
df_covid <- df_covid %>% filter(!(Comuna %>% str_detect("Total")))
df_covid <- df_covid %>% filter(!is.na(date))



# arregla nombres de base de datos
df_covid <-df_covid %>% 
  rename(c("region"="Region", "codigo_region"="Codigo region",
           "comuna"="Comuna","codigo_comuna"="Codigo comuna",
           "poblacion"="Poblacion","tasa"="Tasa",
           "cum_confirmados"="casos",
           "cum_fallecidos"="muertes"))


# Calcula por dia (resta del acumulado)
df_covid <- df_covid %>% 
  arrange(date) %>% 
  group_by(codigo_comuna) %>% 
  mutate(confirmados = cum_confirmados - lag(cum_confirmados, 
                                             default = first(cum_confirmados)),
         fallecidos = cum_fallecidos - lag(cum_fallecidos, 
                                           default = first(cum_fallecidos))) %>% 
  ungroup()


## Calculo Indicadores --------------
df_covid <- df_covid %>% 
  mutate(tasa_letalidad=cum_fallecidos/cum_confirmados,
         tasa_mortalidad=cum_fallecidos/poblacion*1e5,
         tasa_contagios=cum_confirmados/poblacion*1e5)


## GUARDAR -----------
cat('sep=; \n',file = "Data/Data_Modelo/Datos_Covid.csv")
write.table(df_covid,"Data/Data_Modelo/Datos_Covid.csv",
            sep=';',row.names = F, append = T)

# Limpio WS
rm(url, df_casos, df_muertes)

## EoF