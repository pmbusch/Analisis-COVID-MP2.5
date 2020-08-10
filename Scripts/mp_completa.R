### Analisis-COVID-MP2.5
## Analisis para completar datos de MP2.5
## Calcula la distancia entre centroides de comunas y estaciones de monitoreo
## Calcula promedio ponderado inverso a la distancia cuadratica
# Se calcula la distancia con coordeandas geograficas (latlong)
## PBH Julio 2020
# Nota: elegir un nombre mas creativo

library(sf)
source("Scripts/00-Funciones.R", encoding = "UTF-8")
file_name <- "Figuras/Completa_MP/%s.png"


## Centroide comunas --------
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
comunas <- mapa_comuna %>% 
  left_join(codigos_territoriales) %>% 
  mutate(centroide=st_centroid(geometry),
         cent_lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         cent_lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))

## Mapa RM: Poligonos + centroide
comunas %>% 
  filter(mapa_rm==1) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  geom_sf_label(aes(geometry=geometry, label=nombre_comuna))+
  geom_point(aes(cent_lon, cent_lat), col="green")+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 13)

f_savePlot(last_plot(), sprintf(file_name,"Centroides"))


## Estaciones calidad Aire -------------
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")

df_conc <- df_conc %>% filter(year %in% 2016:2019 & pollutant=="mp2.5")

estaciones <- df_conc %>% 
  group_by(codigo_comuna,site, longitud, latitud) %>% 
  summarise(avg=mean(valor, na.rm=T)) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)

## Mapa con estaciones
estaciones <- estaciones %>% 
  left_join(mapa_comuna %>% select(codigo_comuna, mapa_rm))

# Convertir a sf
estaciones <- st_as_sf(estaciones, 
                          coords = c("longitud","latitud"),
                          remove = F, 
                          crs="+proj=longlat +ellps=GRS80 +no_defs")
                          # crs=9155)

comunas %>% 
  filter(mapa_rm==1) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  # geom_sf_label(aes(geometry=geometry, label=nombre_comuna))+
  geom_point(aes(cent_lon, cent_lat), col="green")+
  geom_sf_label(aes(geometry=geometry, label=nombre_comuna),col="red",
                data=estaciones %>% filter(mapa_rm==1))+
  geom_sf(aes(geometry=geometry), col="red", size=3,shape=1,
          data=estaciones %>% filter(mapa_rm==1))+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 13)
f_savePlot(last_plot(), sprintf(file_name,"Estaciones"))


## Distancia entre Estaciones-Centroide ---------
library(nngeo)
st_crs(comunas$centroide)
st_crs(estaciones$geometry)

# Distancia geografica entre latlong, en metros
dist_matrix <- st_distance(comunas$centroide, estaciones$geometry)
dist_matrix
dist_matrix %>% dim() # Comunas vs estaciones

# Paso los datos de la matriz a mis dataframe
df_matrix <- dist_matrix %>% as_tibble()
# Aplano
df_matrix <- df_matrix %>% rowid_to_column() %>% gather(estacion_index,dist,-rowid)
# Convierto feature en indices para hacer los join
df_matrix <- df_matrix %>% 
  mutate(dist=as.numeric(dist),
         estacion_index=str_remove_all(estacion_index,"V") %>% as.numeric()) %>% 
  rename(comuna_index=rowid)

# Join to comuna
df <- comunas %>% 
  dplyr::select(codigo_comuna, nombre_comuna, centroide) %>% 
  rowid_to_column() %>%
  left_join(df_matrix, by=c("rowid"="comuna_index"))
df %>% names()

#Join to estacion
df <- df %>% left_join(estaciones %>% rowid_to_column() %>% 
              dplyr::select(rowid, site, avg),
            by=c("estacion_index"="rowid"))

rm(df_matrix)


# Distancia minima de cada comuna ----------
df_dist <- df %>% 
  group_by(nombre_comuna, codigo_comuna, centroide) %>% 
  summarise(minDist=min(dist),
            minSite=site[which.min(dist)])

# ECDF distancia
df_dist$minDist %>% range()
theme_set(theme_bw())
df_dist %>% 
  mutate(distancia=minDist/1e3) %>% 
  ggplot(aes(distancia,y=..y..*nrow(df_dist)))+
  stat_ecdf()+
  scale_x_continuous(breaks=50*0:7)+
  coord_cartesian(xlim = c(0,200))+ # coord_cartesian does not filter the data
  labs(x="Distancia [km]", y= "N° Comunas")+
  theme_bw(18)
  
f_savePlot(last_plot(), sprintf(file_name,"ECDF_DistanciaEstacion"))

# Graficar lineas distancia---------

# Junto datos de comuna con estaciones. Solo para RM. 
# Saco Lo Barnechea e incluyo Puente Alto
df_aux <- df_dist %>% 
  left_join(mapa_comuna %>% select(codigo_comuna,mapa_rm)) %>% 
  filter(mapa_rm==1)

estaciones_aux <- estaciones %>% 
  filter(mapa_rm==1) %>% 
  select(site,longitud, latitud) %>% rename(ubi=geometry)

df_aux <- df_aux %>% ungroup() %>% 
  left_join(estaciones_aux, by=c("minSite"="site")) %>% 
  left_join(comunas %>% select(codigo_comuna,cent_lon, cent_lat))
 
## DF con los puntos de las lineas, para dp con geom_path graficar
# Junto codigo_comuna (grupo) con las coordenadas de centroide y estacion
puntos_comunas <- df_aux %>% na.omit() %>% 
  select(codigo_comuna, cent_lon, cent_lat) %>% rename(longitud=cent_lon,
                                                       latitud=cent_lat)
puntos_estaciones <- df_aux %>% na.omit() %>% select(codigo_comuna, longitud, latitud)

lineas <- rbind(puntos_comunas, puntos_estaciones)
rm(df_aux, estaciones_aux, puntos_comunas, puntos_estaciones)

# Mapa Estacion mas cercana de cama comuna RM
estaciones_mapa <- estaciones %>% rename(ubi=geometry) %>% 
  filter(mapa_rm==1) %>%  
  dplyr::select(-codigo_provincia,-codigo_comuna)
# Nota: Hay una estacion llamada La Florida en Talca que genera repeticion
df_dist %>% 
  left_join(mapa_comuna) %>% 
  filter(mapa_rm==1) %>% 
  left_join(estaciones_mapa,by=c("minSite"="site")) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  # geom_sf_label(aes(geometry=centroide, label=codigo_comuna),col="green")+
  geom_sf(aes(geometry=centroide), col="green")+
  # geom_sf_label(aes(geometry=ubi,label=minSite),col="red")+
  geom_sf(aes(geometry=ubi),col="red")+
  geom_path(data=lineas, aes(x=longitud, y=latitud, group=codigo_comuna),col="red")+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 13)

f_savePlot(last_plot(), sprintf(file_name,"EstacionCercana"))


## Distancia N primeros ---------------
# Ordena por distancia para cada comuna, e incorpora un ranking
df_dist <- df %>% arrange(codigo_comuna, dist) %>% 
  group_by(nombre_comuna, codigo_comuna, centroide) %>% 
  mutate(rank=rank(dist))

# Filta N primeros
df_dist <- df_dist %>% filter(rank<7)

# ECDF distancia
df_dist %>% 
  mutate(distancia=dist/1e3) %>% 
  ggplot(aes(distancia,y=..y..*unique(df_dist$codigo_comuna) %>% length()))+
  stat_ecdf(aes(col=factor(rank), group=rank),size=1)+
  # facet_wrap(~rank)+
  scale_x_continuous(breaks=10*0:5)+
  coord_cartesian(xlim=c(0,50))+ # coord_cartesian does not filter the data
  scale_color_viridis_d(name="N° Estaciones \n cercanas")+
  labs(x="Distancia [km]", y= "N° Comunas")+
  theme_bw(18)

f_savePlot(last_plot(), sprintf(file_name,"ECDF_DistanciaEstacion_N"))

## Exporta matriz distancia-comuna
df_dist <- df %>% arrange(codigo_comuna, dist) %>% 
  group_by(nombre_comuna, codigo_comuna, centroide) %>% 
  mutate(rank=rank(dist)) %>% 
  left_join(codigos_territoriales)

f_saveCsv(df_dist, "Data/Data_Modelo/distanciaComunaEstacion.csv")
rm(df_dist)

## Promedio ponderado por inverso de la distancia--------
corte_km <- 20
# corte_km <- Inf
df %>% names()
df_avg <- df %>% filter(dist<corte_km*1e3)
df_avg <- df_avg %>% 
  group_by(codigo_comuna, nombre_comuna) %>% 
  summarise(avg=weighted.mean(avg, 1/(dist))) %>% ungroup() %>% 
  right_join(mapa_comuna)

## Mapas
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")

# Chile
df_avg %>% 
  fig_mapa(avg, limites = c(0,50), lwd=0.01,
           titulo="Promedio 2016-2019 \n MP2.5 [ug/m3]", 
           fileName = sprintf(file_name,"MapaChile"))

## Chile Facet
fig_mapaChile_facet(df_avg, avg, limites=c(0,50),
                    titulo = "Promedio 2016-2019 \n MP2.5 [ug/m3]",
                    fileName = sprintf(file_name,"MapaChileFacet"))

# Santiago
df_avg %>% 
  filter(mapa_rm==1) %>% 
  fig_mapa(avg, limites = c(0,50), titulo="Promedio 2016-2019 \n MP2.5 [ug/m3]",
           fileName = sprintf(file_name,"MapaSantiago"))


# Poblacion en comunas con datos MP2.5
total_pob <- df_poblacion$poblacion %>% sum()
pob_mp25 <- df_avg %>% left_join(df_poblacion) %>% pull(poblacion) %>% sum(na.rm=T)
cat(round(pob_mp25/total_pob*100,1),
    "% de poblacion en comunas con monitoreo de MP2.5")

## Guardo Datos de MP2.5 expandidos a nivel comunal
df_avg %>% rename(mp25=avg) %>% select(codigo_comuna,mp25) %>% 
  saveRDS("Data/Data_Modelo/Datos_Concentraciones_20km.rsd")

## Exportacion Mapa interactivo --------------
library(mapview)
library(leaflet)

comunas_mapa <- st_as_sf(comunas, 
                         coords = c("cent_lon","cent_lat"),
                         remove = F, 
                         crs="+proj=longlat +ellps=GRS80 +no_defs")

m <- mapview(comunas_mapa, label=comunas_mapa$nombre_comuna, col.regions="blue",
        layer.name = c("Centroides Comuna"))+
  mapview(estaciones, label=estaciones$site, col.regions="red",
        layer.name = c("Estaciones Monitoreo"))


mapa_comuna_view <- mapa_comuna %>% 
                              left_join(codigos_territoriales) %>% 
                              select(codigo_comuna, nombre_comuna,
                                     nombre_provincia, nombre_region)
m_comunas <- mapview(mapa_comuna_view,
          layer.name = c("Comunas"),label=mapa_comuna_view$nombre_comuna,
          alpha.regions=0.1, col.regions="green")
m_comunas+m


# Save file as html
mapshot(m+m_comunas, "Figuras/Completa_MP/EstacionesMonitoreo.html")

rm(m, m_comunas, comunas_mapa,mapa_comuna_view)

## IDEM METEOROLOGIA ----------
# Cargar objeto Comunas
## Estaciones Meteorologia -------------
df_meteo <- read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")

df_meteo <- df_meteo %>% filter(year(date) %in% 2016:2019)

estaciones <- df_meteo %>% 
  group_by(codigo_comuna,estacion,nombre_estacion, longitud, latitud) %>% 
  summarise(count=n()) %>% ungroup() %>% 
  select(-count) %>% na.omit() %>% 
  left_join(codigos_territoriales)


## Mapa con estaciones
estaciones <- estaciones %>% 
  left_join(mapa_comuna %>% select(codigo_comuna, mapa_rm))

# Convertir a sf
estaciones <- st_as_sf(estaciones, 
                       coords = c("longitud","latitud"),
                       remove = F, 
                       crs="+proj=longlat +ellps=GRS80 +no_defs")
# crs=9155)


comunas %>% 
  filter(mapa_rm==1) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  # geom_sf_label(aes(geometry=geometry, label=nombre_comuna))+
  geom_point(aes(cent_lon, cent_lat), col="green")+
  geom_sf_label(aes(geometry=geometry, label=nombre_estacion),col="red",
                data=estaciones %>% filter(mapa_rm==1))+
  geom_sf(aes(geometry=geometry), col="red", size=3,shape=1,
          data=estaciones %>% filter(mapa_rm==1))+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 13)

f_savePlot(last_plot(), sprintf(file_name,"EstacionesMeteo"))


## Distancia entre Estaciones-Centroide ---------
library(nngeo)
st_crs(comunas$centroide)
st_crs(estaciones$geometry)

# Distancia geografica entre latlong, en metros
dist_matrix <- st_distance(comunas$centroide, estaciones$geometry)
dist_matrix
dist_matrix %>% dim() # Comunas vs estaciones

# Paso los datos de la matriz a mis dataframe
df_matrix <- dist_matrix %>% as_tibble()
# Aplano
df_matrix <- df_matrix %>% rowid_to_column() %>% gather(estacion_index,dist,-rowid)
# Convierto feature en indices para hacer los join
df_matrix <- df_matrix %>% 
  mutate(dist=as.numeric(dist),
         estacion_index=str_remove_all(estacion_index,"V") %>% as.numeric()) %>% 
  rename(comuna_index=rowid)

# Join to comuna
df_meteo <- comunas %>% 
  dplyr::select(codigo_comuna, nombre_comuna, centroide) %>% 
  rowid_to_column() %>%
  left_join(df_matrix, by=c("rowid"="comuna_index"))
df_meteo %>% names()

#Join to estacion
df_meteo <- df_meteo %>% left_join(estaciones %>% rowid_to_column() %>% 
                         dplyr::select(rowid, estacion, nombre_estacion),
                       by=c("estacion_index"="rowid"))

rm(df_matrix)

# Distancia minima de cada comuna ----------
df_dist <- df_meteo %>% 
  group_by(nombre_comuna, codigo_comuna, centroide) %>% 
  summarise(minDist=min(dist),
            minSite=estacion[which.min(dist)])

# ECDF distancia
df_dist$minDist %>% range()
theme_set(theme_bw())
df_dist %>% 
  mutate(distancia=minDist/1e3) %>% 
  ggplot(aes(distancia,y=..y..*nrow(df_dist)))+
  stat_ecdf()+
  scale_x_continuous(breaks=10*0:75)+
  coord_cartesian(xlim = c(0,50))+ # coord_cartesian does not filter the data
  labs(x="Distancia [km]", y= "N° Comunas")+
  theme_bw(18)

f_savePlot(last_plot(), sprintf(file_name,"ECDF_DistanciaEstacionMeteo"))

# Graficar lineas distancia---------

# Junto datos de comuna con estaciones. Solo para RM. 
# Saco Lo Barnechea e incluyo Puente Alto
df_aux <- df_dist %>% 
  left_join(mapa_comuna %>% select(codigo_comuna,mapa_rm)) %>% 
  filter(mapa_rm==1)

estaciones_aux <- estaciones %>% 
  filter(mapa_rm==1) %>% 
  select(estacion,longitud, latitud) %>% rename(ubi=geometry)

df_aux <- df_aux %>% ungroup() %>% 
  left_join(estaciones_aux, by=c("minSite"="estacion")) %>% 
  left_join(comunas %>% select(codigo_comuna,cent_lon, cent_lat))

## DF con los puntos de las lineas, para dp con geom_path graficar
# Junto codigo_comuna (grupo) con las coordenadas de centroide y estacion
puntos_comunas <- df_aux %>% na.omit() %>% 
  select(codigo_comuna, cent_lon, cent_lat) %>% rename(longitud=cent_lon,
                                                       latitud=cent_lat)
puntos_estaciones <- df_aux %>% na.omit() %>% select(codigo_comuna, longitud, latitud)

lineas <- rbind(puntos_comunas, puntos_estaciones)
rm(df_aux, estaciones_aux, puntos_comunas, puntos_estaciones)

# Mapa Estacion mas cercana de cama comuna RM
estaciones_mapa <- estaciones %>% rename(ubi=geometry) %>% 
  filter(mapa_rm==1) %>%  
  dplyr::select(-codigo_provincia,-codigo_comuna)
# Nota: Hay una estacion llamada La Florida en Talca que genera repeticion
df_dist %>% 
  left_join(mapa_comuna) %>% 
  filter(mapa_rm==1) %>% 
  left_join(estaciones_mapa,by=c("minSite"="estacion")) %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  # geom_sf_label(aes(geometry=centroide, label=codigo_comuna),col="green")+
  geom_sf(aes(geometry=centroide), col="green")+
  # geom_sf_label(aes(geometry=ubi,label=minSite),col="red")+
  geom_sf(aes(geometry=ubi),col="red")+
  geom_path(data=lineas, aes(x=longitud, y=latitud, group=codigo_comuna),col="red")+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 13)

f_savePlot(last_plot(), sprintf(file_name,"EstacionCercanaMeteo"))


## Distancia N primeros ---------------
# Ordena por distancia para cada comuna, e incorpora un ranking
df_dist <- df_meteo %>% arrange(codigo_comuna, dist) %>% 
  group_by(nombre_comuna, codigo_comuna, centroide) %>% 
  mutate(rank=rank(dist, ties.method = "first"))

# Filta N primeros
df_dist <- df_dist %>% filter(rank<7)

# ECDF distancia
df_dist %>% 
  mutate(distancia=dist/1e3) %>% 
  ggplot(aes(distancia,y=..y..*unique(df_dist$codigo_comuna) %>% length()))+
  stat_ecdf(aes(col=factor(rank), group=rank),size=1)+
  scale_x_continuous(breaks=10*0:5)+
  coord_cartesian(xlim=c(0,50))+ # coord_cartesian does not filter the data
  scale_color_viridis_d(name="N° Estaciones \n cercanas")+
  labs(x="Distancia [km]", y= "N° Comunas")+
  theme_bw(18)

f_savePlot(last_plot(), sprintf(file_name,"ECDF_DistanciaEstacion_NMeteo"))


## Exportacion Mapa interactivo --------------
library(mapview)
library(leaflet)

comunas_mapa <- st_as_sf(comunas, 
                         coords = c("cent_lon","cent_lat"),
                         remove = F, 
                         crs="+proj=longlat +ellps=GRS80 +no_defs")

m <- mapview(comunas_mapa, label=comunas_mapa$nombre_comuna, col.regions="blue",
             layer.name = c("Centroides Comuna"))+
  mapview(estaciones, label=estaciones$estacion, col.regions="red",
          layer.name = c("Estaciones Meteorologia"))


mapa_comuna_view <- mapa_comuna %>% 
  left_join(codigos_territoriales) %>% 
  select(codigo_comuna, nombre_comuna,
         nombre_provincia, nombre_region)
m_comunas <- mapview(mapa_comuna_view,
                     layer.name = c("Comunas"),label=mapa_comuna_view$nombre_comuna,
                     alpha.regions=0.1, col.regions="green")
m_comunas+m


# Save file as html
mapshot(m+m_comunas, "Figuras/Completa_MP/EstacionesMeteorologia.html")

rm(m, m_comunas, comunas_mapa,mapa_comuna_view)


## Nearest stations -----
# Otro metodo para estimar la distancia menor
# Fuente:  https://ryanpeek.org/mapping-in-R-workshop/03_spatial_joins.html
# nearest_stations <- st_nn(comunas$centroide, estaciones$geometry, 
#                           returnDist = T, k = 1, progress = FALSE)
# 
# nearest_stations_df <- tibble(dist=nearest_stations$dist, 
#                               centrID=do.call(rbind, nearest_stations$nn)) %>%
#   dplyr::distinct(.keep_all = T)
# 
# estaciones <- estaciones %>% rowid_to_column()
# 
# ## Join by index
# df_nn <- comunas %>% cbind(nearest_stations_df) %>% 
#   left_join(codigos_territoriales) %>% 
#   left_join(estaciones, by=c("centrID"="rowid"))
# 
# df_nn %>% names()
# df_nn %>% dplyr::select(codigo_region.x, nombre_comuna.x, site, dist) %>% view()


## EoF