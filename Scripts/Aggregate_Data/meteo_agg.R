### Analisis-COVID-MP2.5
## Agrego datos meteorologicos a nivel de comuna
## PBH Julio 2020

# Carga datos brutos --------
# source("Scripts/Load_Data/meteo_scrap.R", encoding = "UTF-8") # Baja los datos
df <-read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")

## Agregar a nivel comunal ----------------
## NOTAS:
# Agregar mas comunas

df <- df %>% 
  mutate(year=year(date)) %>% 
  filter(year %in% c(2016,2017,2018,2019) &
           tipo %in% c("tmed", "hr")) %>% 
  cutData(type="season", hemisphere = "southern") %>% 
  filter(season %in% c("winter (JJA)","summer (DJF)")) %>% # solo verano e invierno
  group_by(comuna, estacion, tipo, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% 
  ungroup() %>% group_by(comuna, tipo, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  mutate(season=season %>% str_remove_all(" \\(JJA\\)| \\(DJF\\)"),
         tipo=paste(tipo, season, sep="_"),
         season=NULL) %>% 
  spread(tipo,valor)

# Agregar codigos comunales
df <- df %>% 
  mutate(nombre_comuna=comuna %>% 
           str_replace_all("Viña del Mar","Vina del Mar")) %>%
  left_join(codigos_territoriales %>% select(codigo_comuna, nombre_comuna),
            by=c("nombre_comuna")) %>% 
  select(-comuna, -nombre_comuna)


saveRDS(df, "Data/Data_Modelo/Datos_Meteorologia.rsd")

## EoF