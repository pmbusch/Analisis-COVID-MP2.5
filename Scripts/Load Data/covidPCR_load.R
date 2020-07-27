### Analisis-COVID-MP2.5
## Datos Covid. Fuente: https://github.com/MinCiencia/Datos-COVID19
## Producto 7: Exámenes PCR por región
## PBH Julio 2020

## Descarga de datos ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados
df_pcr <- read_csv(paste(url,"producto7","PCR_std.csv", sep="/"))
names(df_pcr) <- names(df_pcr) %>% str_to_lower() %>% str_replace_all(" ","_")
df_pcr <- df_pcr %>% na.omit() # limpio NA

## PCR totales a la fecha -----------
# Sumo los totales
df_pcr <- df_pcr %>% 
  group_by(region, codigo_region, poblacion) %>% 
  summarise(pcr_region=sum(numero, na.rm=T)) %>% ungroup() %>% 
  select(codigo_region, pcr_region)

# total pcr
df_pcr$pcr_region %>% sum()

rm(url)
## EoF