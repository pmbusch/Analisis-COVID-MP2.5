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
df_conc %>% n_distinct("site")
  
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


## Expansion concentracion FIXED RADIUS ------
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")
df <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")
df_conc <- df %>% 
  filter(year %in% 2016:2019 & pollutant=="mp2.5") %>% 
  group_by(codigo_comuna, pollutant, unidad, site,year) %>% 
  summarise(valor=mean(valor, na.rm=T),
            disponibilidad=n()/365) %>% ungroup() %>% 
  filter(disponibilidad>0.8) %>% 
  group_by(site,codigo_comuna, pollutant, unidad) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  filter(!is.na(valor))
df_conc %>% n_distinct("site")

## Cargar datos distancia
df_dist <- read_rds("Data/Data_Modelo/distanciacomunaEstacionsinca.rsd")
df_dist_zona <- read_rds("Data/Data_Modelo/distanciazonaEstacionsinca.rsd")


# Cruzo con estaciones dentro del rango del centroide de cada comuna
# Nota: centroide debe ser estimado mediante zonas censales
corte_km <- 20
df_dist <- df_dist %>% filter(dist<corte_km*1e3)

# Comunas y estaciones dentro de este rango
comunas_site <- df_dist %>% 
  mutate(tupla=paste(codigo_comuna,site,sep="-")) %>% 
  pull(tupla) %>% unique()

# Filtro en zonas censales: tuplas comunas-site disponibles
df_dist_zona <- df_dist_zona %>% 
  mutate(tupla=paste(codigo_comuna,site,sep="-")) %>% 
  filter(tupla %in% comunas_site) %>% 
  select(geocodigo, codigo_comuna, nombre_comuna, site, dist)

## Add data poblacion
censo_2017_zonas$poblacion %>% sum()
pob <- censo_2017_zonas %>% group_by(geocodigo) %>% 
  summarise(poblacion=sum(poblacion, na.rm=T)) %>% ungroup()
df_avg <- df_dist_zona %>% left_join(pob, by=c("geocodigo")) %>% na.omit()
rm(pob)

# Add conc data
## 21 de Mayo y La Florida nombres repetidos!!!!
df_avg <- df_avg %>% left_join(df_conc %>% select(-codigo_comuna), 
                               by=c("site")) %>% na.omit()

## Promedio ponderado por inverso de la distancia a nivel de zona censal
## Agregado a nivel de comuna ponderado por la poblacion
library(RColorBrewer)

df_mp <- df_avg %>% 
  group_by(geocodigo, codigo_comuna, poblacion) %>% 
  summarise(valor=weighted.mean(valor, 1/(dist))) %>% ungroup()

# View map on zonas
m2 <- left_join(mapa_zonas, df_mp, by=c("geocodigo")) %>% st_as_sf()
m3 <- mapview(m2, zcol="valor",col.regions=brewer.pal(9, "YlOrRd"))
mapshot(m3, "Figuras/ConcentracionMP25_zonas.html")
rm(m2,m3)

df_mp <- df_mp %>% 
  group_by(codigo_comuna) %>% 
  summarise(valor=weighted.mean(valor, poblacion)) %>% ungroup() %>% 
  right_join(mapa_comuna) %>% left_join(codigos_territoriales) %>% 
  rename(mp25=valor)
df_mp %>% filter(!is.na(mp25)) %>% nrow() # N comunas con datos:125

m1 <- mapview(df_mp %>% st_as_sf(), 
      label=paste(df_mp$nombre_comuna,": ",
                  round(df_mp$mp25,2),"[ug/m3]",sep=""),
      layer.name="Concentracion MP2.5 2016-2019",
      zcol="mp25",
      na.color="white",
      col.regions=brewer.pal(9, "YlOrRd"))
m1
mapshot(m1, "Figuras/ConcentracionMP25.html")
rm(m1)

# Guarda datos ----------
df_conc <- df_mp %>% 
  select(codigo_comuna, mp25) %>% filter(!is.na(mp25))
saveRDS(df_conc, "Data/Data_Modelo/Datos_Concentraciones.rsd")

## EoF