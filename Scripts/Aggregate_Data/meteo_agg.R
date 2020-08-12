### Analisis-COVID-MP2.5
## Agrego datos meteorologicos a nivel de comuna
## PBH Julio 2020

# Carga datos brutos --------
# source("Scripts/Load_Data/meteo_scrap.R", encoding = "UTF-8") # Baja los datos
df <-read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")
source("Scripts/00-Funciones.R", encoding = "UTF-8")

df$tipo %>% unique()
df$date %>% year() %>% unique()

# Numero estaciones: 398
df$estacion %>% unique() %>% length()

## Agregar a nivel comunal y promediar por años ----------------
# Promedio simple: Que va primero: estacion o años??
df_meteo <- df %>% 
  mutate(year=year(date)) %>% 
  filter(year %in% 2010:2019 &
           tipo %in% c("tmed", "hr","heating_degree_18","heating_degree_15")) %>% 
  group_by(codigo_comuna, tipo, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  mutate(tipo=paste(tipo, season, sep="_"),
         season=NULL) %>% 
  filter(!is.nan(valor))
 
# Expandir datos
df_meteo <- df_meteo %>% spread(tipo,valor)

# N comunsa con al menos 1 dato de meteorologia
df_meteo %>% nrow()

saveRDS(df_meteo, "Data/Data_Modelo/Datos_Meteorologia.rsd")

## EoF