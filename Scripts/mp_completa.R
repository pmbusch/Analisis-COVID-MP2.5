### Analisis-COVID-MP2.5
## Analisis para completar datos de MP2.5
## Calcula la distancia entre centroides de comunas y estaciones de monitoreo
## Calcula promedio ponderado inverso a la distancia cuadratica
## PBH Julio 2020
# Nota: elegir un nombre mas creativo

library(sf)
library(raster)
# Se calcula la distancia con coordeandas geograficas (latlong)
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
comunas %>% filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  geom_sf_label(aes(geometry=geometry, label=nombre_comuna))+
  geom_point(aes(cent_lon, cent_lat), col="green")+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 13)

f_savePlot(last_plot(), sprintf(file_name,"Centroides"))

## SOLO RM
# comunas <- comunas %>% filter(codigo_provincia=="131" & codigo_comuna!="13115")


## Estaciones calidad Aire -------------
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")

df_conc <- df_conc %>% filter(year %in% 2016:2019 & pollutant=="mp2.5")

estaciones <- df_conc %>% 
  group_by(codigo_comuna,site, longitud, latitud) %>% 
  summarise(avg=mean(valor, na.rm=T)) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)

# SOLO RM
# estaciones <- estaciones %>% filter(codigo_provincia=="131" & codigo_comuna!="13115")

# Convertir a sf
estaciones <- st_as_sf(estaciones, 
                          coords = c("longitud","latitud"),
                          remove = F, 
                          crs="+proj=longlat +ellps=GRS80 +no_defs")
                          # crs=9155)

## Mapa con estaciones
comunas %>% filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  # geom_sf_label(aes(geometry=geometry, label=nombre_comuna))+
  geom_point(aes(cent_lon, cent_lat), col="green")+
  geom_sf_label(aes(geometry=geometry, label=nombre_comuna),col="red",
                data=estaciones %>% filter(codigo_provincia=="131" & codigo_comuna!="13115"))+
  geom_sf(aes(geometry=geometry), col="red", size=3,shape=1,
          data=estaciones %>% filter(codigo_provincia=="131" & codigo_comuna!="13115"))+
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


# Distancia minima de cada comuna
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
  scale_x_continuous(breaks=50*0:7, limits = c(0,350))+
  labs(x="Distancia [km]", y= "N° Comunas")
  
f_savePlot(last_plot(), sprintf(file_name,"ECDF_DistanciaEstacion"))

# Mapa Estacion mas cercana de cama comuna RM
df_dist %>% 
  left_join(mapa_comuna) %>% 
  left_join(estaciones %>% rename(ubi=geometry) %>% 
              dplyr::select(-codigo_provincia), 
            by=c("minSite"="site", "codigo_comuna")) %>% 
  filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  geom_sf_label(aes(geometry=centroide, label=minSite),col="green")+
  geom_sf(aes(geometry=centroide), col="green")+
  geom_sf_label(aes(geometry=ubi,label=minSite),col="red")+
  geom_sf(aes(geometry=ubi),col="red")+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 13)
f_savePlot(last_plot(), sprintf(file_name,"EstacionCercana"))


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