### Analisis-COVID-MP2.5
## Analisis para completar datos de MP2.5
## PBH Julio 2020
## Fuente:  https://ryanpeek.org/mapping-in-R-workshop/03_spatial_joins.html
# Nota: elegir un nombre mas creativo

library(sf)
library(raster)
# EPSG Chile: 9155
## Debo proyectar correctamente para calcular bien las distancias
projection(mapa_comuna$geometry)
mapa_comuna %>% names()
## Centroide comunas --------
df <- mapa_comuna %>% 
  left_join(codigos_territoriales) %>% 
  mutate(cent_lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), 
         cent_lat=map_dbl(geometry, ~st_centroid(.x)[[2]]),
                          centroide=st_centroid(geometry))

## Mapa RM: Poligonos + centroide
df %>% filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  geom_sf_label(aes(geometry=geometry, label=nombre_comuna))+
  geom_point(aes(cent_lon, cent_lat), col="green")+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)

# Prueba solo RM
df <- df %>% filter(codigo_provincia=="131" & codigo_comuna!="13115")


df_centroide <- df %>% 
  st_as_sf(coords=c("cent_lon","cent_lat"), crs=9155, remove=FALSE)  

# Mismo grafico pero ahora con geometry
df %>% filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  geom_sf_label(aes(geometry=geometry, label=nombre_comuna))+
  geom_sf(aes(geometry=geometry), col="green", 
             data=df_centroide %>% filter(codigo_provincia=="131" & codigo_comuna!="13115"))+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)


## Estaciones calidad Aire -------------
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")

df_conc <- df_conc %>% filter(year %in% 2016:2019 & pollutant=="mp2.5")

df_estaciones <- df_conc %>% 
  group_by(codigo_comuna,site, longitud, latitud) %>% 
  summarise(avg=mean(valor, na.rm=T)) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)

# SOLO RM
df_estaciones <- df_estaciones %>% 
  filter(codigo_provincia=="131" & codigo_comuna!="13115")

# Convertir a sf
df_estaciones <- st_as_sf(df_estaciones, 
                          coords = c("longitud","latitud"),
                          remove = F, 
                          crs="+proj=longlat +ellps=GRS80 +no_defs")
                          # crs=9155)

## Mapa Final
df %>% filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry), fill="white")+
  geom_sf_label(aes(geometry=geometry, label=nombre_comuna))+
  geom_sf(aes(geometry=geometry), col="green", 
          data=df_centroide %>% filter(codigo_provincia=="131" & codigo_comuna!="13115"))+
  # geom_sf_label(aes(geometry=geometry, label=nombre_comuna),col="red",
  #               data=df_estaciones %>% filter(codigo_provincia=="131" & codigo_comuna!="13115"))+
  geom_sf(aes(geometry=geometry), col="red", size=3,shape=1,
          data=df_estaciones %>% filter(codigo_provincia=="131" & codigo_comuna!="13115"))+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)


## Distancia entre Estaciones-Centroide ---------
library(nngeo)

st_crs(df_centroide$centroide)
st_crs(df_estaciones$geometry)

dist_matrix <- st_distance(df_centroide$centroide, df_estaciones$geometry)
dist_matrix
dist_matrix %>% dim()

df_matrix <- dist_matrix %>% as_tibble()

# Aplano
df_matrix <- df_matrix %>% rowid_to_column() %>% gather(estacion_index,dist,-rowid)
# Convierto
df_matrix <- df_matrix %>% 
  mutate(dist=as.numeric(dist),
         estacion_index=str_remove_all(estacion_index,"V") %>% as.numeric()) %>% 
  rename(comuna_index=rowid)

# Join to comuna
df_prueba <- df_centroide %>% 
  select(codigo_comuna, nombre_comuna) %>% 
  rowid_to_column() %>%
  left_join(df_matrix, by=c("rowid"="comuna_index"))
df_prueba %>% names()

#Join to estacion
df_prueba2 <- df_prueba %>% 
  left_join(df_estaciones %>% 
              select(rowid, site, avg),
            by=c("estacion_index"="rowid"))

# Distancia minima
df_distMinima <- df_prueba2 %>% ungroup() %>% 
  group_by(nombre_comuna) %>% 
  summarise(minDist=min(dist),
            minSite=site[which.min(dist)])

# ECDF distnacia
theme_set(theme_bw())
df_distMinima %>% 
  mutate(distancia=minDist/1e3) %>% 
  ggplot(aes(distancia,y=..y..*nrow(df_distMinima)))+
  stat_ecdf()+
  labs(x="Distancia [km]", y= "N° Comunas")
  

## Nearest stations
nearest_stations <- st_nn(df$centroide, df_estaciones$geometry, 
                          returnDist = T, k = 1, progress = FALSE)

nearest_stations_df <- tibble(dist=nearest_stations$dist, 
                              centrID=do.call(rbind, nearest_stations$nn)) %>%
  dplyr::distinct(.keep_all = T)

df_estaciones <- df_estaciones %>% rowid_to_column()

## Join by index
df1 <- df %>% cbind(nearest_stations_df) %>% 
  left_join(codigos_territoriales) %>% 
  left_join(df_estaciones, by=c("centrID"="rowid"))

df1 %>% names()

df1 %>% select(codigo_region.x, nombre_comuna.x, site, dist) %>% view()


## EoF