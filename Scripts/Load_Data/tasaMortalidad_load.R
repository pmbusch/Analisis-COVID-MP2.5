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

## EoF