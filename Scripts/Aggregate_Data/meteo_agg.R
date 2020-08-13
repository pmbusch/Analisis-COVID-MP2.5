### Analisis-COVID-MP2.5
## Agrego datos meteorologicos a nivel de comuna
## PBH Julio 2020

# Carga datos brutos --------
# source("Scripts/Load_Data/meteo_scrap.R", encoding = "UTF-8") # Baja los datos
df <-read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")
source("Scripts/00-Funciones.R", encoding = "UTF-8")
df_dist <- read_rds("Data/Data_Modelo/distanciaComunaEstacionMeteo.rsd")
df_dist <- df_dist %>% filter(rank==1)

df$tipo %>% unique()
df$date %>% year() %>% unique()

# Numero estaciones: 398
df$estacion %>% unique() %>% length()

## Agregar a nivel de estcion y promediar por años ----------------
df_meteo <- df %>% 
  mutate(year=year(date)) %>% 
  filter(year %in% 2010:2019 &
           tipo %in% c("tmed", "hr","heating_degree_18","heating_degree_15")) %>% 
  group_by(estacion, tipo,year, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  group_by(estacion, tipo, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()

## Expando los datos
df_meteo <- df_meteo %>% 
  mutate(tipo=paste(tipo, season, sep="_"),
         season=NULL) %>% 
  filter(!is.nan(valor))



## CRITERIO TEMPORAL: Asigno datos de monitor mas cercano a cada comuna
# Cruzo todos los datos
df_com <- df_dist %>% select(codigo_comuna, estacion, dist, rank) %>% 
  left_join(df_meteo, by=c("estacion")) %>% na.omit()

## Filtro por ranking mayor
(df_com$tipo %>% unique() %>% length())*343 # Largo arreglo, tipos x comunas
df_meteo <- df_com %>% 
  group_by(codigo_comuna, tipo) %>% 
  summarise(rank=min(rank),
            estacion=estacion[which.min(rank)],
            dist=dist[which.min(rank)],
            valor=valor[which.min(rank)]) %>% ungroup()

# Rango distancia
df_meteo$dist %>% range()

# Expandir datos
df_meteo <- df_meteo %>% 
  select(-rank,-estacion, -dist) %>% 
  spread(tipo,valor)

df_meteo %>% skim()

# N comunas con al menos 1 dato de meteorologia (todas dado la aproximacion utilizada)
df_meteo %>% n_distinct("estacion")

# Save data
saveRDS(df_meteo, "Data/Data_Modelo/Datos_Meteorologia.rsd")

## EoF