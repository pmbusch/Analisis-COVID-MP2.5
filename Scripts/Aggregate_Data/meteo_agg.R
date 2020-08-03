### Analisis-COVID-MP2.5
## Agrego datos meteorologicos a nivel de comuna
## PBH Julio 2020

# Carga datos brutos --------
# source("Scripts/Load_Data/meteo_scrap.R", encoding = "UTF-8") # Baja los datos
df <-read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")
source("Scripts/00-Funciones.R", encoding = "UTF-8")

## Agregar a nivel comunal y promediar por años ----------------
# Promedio simple: Que va primero: estacion o años??
df <- df %>% 
  mutate(year=year(date)) %>% 
  filter(year %in% c(2016,2017,2018,2019) &
           tipo %in% c("tmed", "hr","heating_degree")) %>%
  # filter(season %in% c("winter","summer")) %>% # solo verano e invierno
  group_by(codigo_comuna, tipo, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  mutate(tipo=paste(tipo, season, sep="_"),
         season=NULL)
 
# Expandir datos
df <- df %>% spread(tipo,valor)

saveRDS(df, "Data/Data_Modelo/Datos_Meteorologia.rsd")

## EoF