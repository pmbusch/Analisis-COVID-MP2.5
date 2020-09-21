### Analisis-COVID-MP2.5
## Datos Covid. Fuente: https://github.com/MinCiencia/Datos-COVID19
## Producto 29: Cuarentenas Activas e Históricas
## PBH Julio 2020

## Descarga de datos ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados
df_cuarentena <- read_csv(paste(url,"producto29","Cuarentenas-Totales.csv", sep="/"))
names(df_cuarentena) <- names(df_cuarentena) %>% str_to_lower() %>% str_replace_all(" ","_")
# df_cuarentena <- df_cuarentena %>% na.omit() # limpio NA

# Para homologar codigos comunales, debo agregar un 0 a las regiones (ej: 01)
df_cuarentena <- df_cuarentena %>% 
  rename(codigo_comuna=código_cut_comuna) %>% 
  mutate(codigo_comuna=paste(if_else(str_length(codigo_comuna)==4,"0",""),
                             codigo_comuna,sep=""))


df_cuarentena %>% group_by(alcance) %>% summarise(count=n()) %>% arrange(desc(count))
# df_cuarentena <- df_cuarentena %>% arrange(alcance)

# Date format
df_cuarentena <- df_cuarentena %>% 
  mutate(date = fecha_de_inicio %>% strptime("%Y-%m-%d") %>% as_date())

## Fecha inicio cuarentena por comuna --------
df_cuarentena <- df_cuarentena %>% 
  group_by(codigo_comuna) %>% 
  summarise(fecha_cuarentena=min(date)) %>% ungroup() %>% 
  mutate(fecha_cuarentena=fecha_cuarentena %>% as_date())

rm(url)
## EoF