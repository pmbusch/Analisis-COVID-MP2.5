### Analisis-COVID-MP2.5
## Agrega datos de concentraciones
## PBH Julio 2020

# Carga datos brutos --------
# source("Scripts/Load_Data/sinca_scrap.R", encoding = "UTF-8") # Baja los datos
df <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")

## Agregar a nivel comunal -----------
## NOTAS:
# Debo mejorar la agregacion al incluir otros contaminantes y ver bien 
# promedio de estaciones con distinta disponibilidad de datos

df_conc <- df %>% 
  filter(year %in% 2010:2019 & pollutant=="mp2.5") %>% 
  group_by(codigo_comuna, pollutant, unidad, site,year) %>% 
  summarise(valor=mean(valor, na.rm=T),
            disponibilidad=n()/365) %>% ungroup() %>% 
  filter(disponibilidad>0.8) %>% 
  group_by(site,codigo_comuna, pollutant, unidad) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()

# Numero estaciones
df_conc$site %>% unique() %>% length()
  
df_conc <- df_conc %>%   
  group_by(codigo_comuna, pollutant, unidad) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()

# Numero comunas
df_conc %>% nrow()

# Resumir data
df_conc <- df_conc %>% 
  rename(mp25=valor) %>% 
  select(codigo_comuna, mp25)

# Guarda datos ----------
saveRDS(df_conc, "Data/Data_Modelo/Datos_Concentraciones.rsd")

rm(df)
## EoF