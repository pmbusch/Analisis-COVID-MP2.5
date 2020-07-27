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

source("Scripts/00-Funciones.R", encoding = "UTF-8")
df_conc <- df %>% 
  filter(year %in% c(2016,2017,2018,2019)) %>% 
  group_by(comuna, pollutant, unidad, site) %>% 
  summarise(valor=weighted.mean(valor, count, na.rm=T)) %>% 
  ungroup() %>% group_by(comuna, pollutant, unidad) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  filter(pollutant=="mp2.5") # solo MP2.5 por el momento

# Agregar codigos comunales
df_conc <- df_conc %>% 
  mutate(nombre_comuna=f_remover_acentos(comuna) %>% 
           str_replace_all("Aysen","Aisen") %>% 
           str_replace_all("Coyhaique","Coihaique")) %>% 
  left_join(codigos_territoriales,by=c("nombre_comuna")) %>% 
  rename(mp25=valor) %>% 
  select(codigo_comuna, mp25)

# Guarda datos ----------
saveRDS(df_conc, "Data/Data_Modelo/Datos_Concentraciones.rsd")

rm(df)
## EoF