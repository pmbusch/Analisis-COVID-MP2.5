### Analisis-COVID-MP2.5
## Analisis para completar datos de MP2.5 y meteorologia
## Calcula la distancia entre centroides de comunas y estaciones de monitoreo
## Se calcula la distancia con coordeandas geograficas (latlong)
## Genera una matriz distancia monitor-comuna, que puede ser utilizada para 
## agregar datos (opcion SINCA o meteorologia)
## PBH Julio 2020


## ELEGIR: SINCA o METEOROLOGIA (para no duplicar codigo) -----
sinca <- T

if (sinca){
  df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")
  df_conc <- df_conc %>% filter(year %in% 2010:2019 & pollutant=="mp2.5")
  file_out <- "Data/Data_Modelo/distanciaComunaEstacionSINCA.rsd"
  est_file <- "sinca"
} else{
  df_conc <- read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")
  df_conc <- df_conc %>% filter(year(date) %in% 2010:2019) %>% 
    rename(site=estacion)
  file_out <- "Data/Data_Modelo/distanciaComunaEstacionMeteo.rsd"
  est_file <- "meteo"
  }
rm(sinca)


## Librerias ----
library(sf)
source("Scripts/00-Funciones.R", encoding = "UTF-8")
file_name <- "Figuras/Completa_MP/%s.png"
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))


## Centroide comunas --------
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
comunas <- mapa_comuna %>% as_tibble() %>% 
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


## Estaciones Monitoreo -------------
estaciones <- df_conc %>% 
  group_by(codigo_comuna,site, longitud, latitud) %>% 
  summarise(avg=mean(valor, na.rm=T)) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)
rm(df_conc)

## Mapa con estaciones
estaciones <- estaciones %>% 
  left_join(mapa_comuna %>% as_tibble() %>% 
              select(codigo_comuna, mapa_rm))

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
f_savePlot(last_plot(), sprintf(file_name,
                                paste("estaciones",est_file,sep="_")))


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


## Distancia N primeros ---------------
# Ordena por distancia para cada comuna, e incorpora un ranking
df_dist <- df %>% arrange(codigo_comuna, dist) %>% 
  group_by(nombre_comuna, codigo_comuna) %>% 
  mutate(rank=rank(dist,ties.method = "first")) %>% ungroup() %>% 
  left_join(comunas %>% select(codigo_comuna, centroide, mapa_rm)) %>% 
  left_join(codigos_territoriales)

# Exporta Matriz Distancia Comuna-Estacion
file_out
saveRDS(df_dist, file_out)

# ECDF distancia minima
df_dist %>% filter(rank==1) %>% pull(dist) %>% range()
df_dist %>% 
  filter(rank==1) %>% 
  mutate(distancia=dist/1e3) %>% 
  ggplot(aes(distancia,y=..y..*nrow(filter(df_dist,rank==1))))+
  stat_ecdf()+
  scale_x_continuous(breaks=50*0:7)+
  coord_cartesian(xlim = c(0,200))+ # coord_cartesian does not filter the data
  labs(x="Distancia [km]", y= "N° Comunas")
  
f_savePlot(last_plot(), sprintf(file_name,
                                paste("ECDF_DistanciaEstacion",
                                      est_file,sep = "_")))

# ECDF distancia n primeros
n <- 6
df_dist %>% 
  filter(rank<=n) %>% 
  mutate(distancia=dist/1e3) %>% 
  ggplot(aes(distancia,
             y=..y..*(df_dist %>% filter(rank<=n) %>% 
                        pull(codigo_comuna) %>% unique() %>% 
                        length())))+
  stat_ecdf(aes(col=factor(rank), group=rank),size=1)+
  scale_x_continuous(breaks=10*0:5)+
  coord_cartesian(xlim=c(0,50))+ # coord_cartesian does not filter the data
  scale_color_viridis_d(name="N° Estaciones \n cercanas")+
  labs(x="Distancia [km]", y= "N° Comunas")

f_savePlot(last_plot(), sprintf(file_name,
                                paste("ECDF_DistanciaEstacion_N",
                                      est_file,sep="_")))
rm(n)

# Graficar lineas distancia---------

# Junto datos de comuna con estaciones. Solo para RM. 
# Saco Lo Barnechea e incluyo Puente Alto
df_aux <- df_dist %>% 
  filter(rank==1) %>% 
  left_join(mapa_comuna %>% select(codigo_comuna,mapa_rm)) %>% 
  filter(mapa_rm==1)

estaciones_aux <- estaciones %>% 
  filter(mapa_rm==1) %>% 
  select(site,longitud, latitud) %>% rename(ubi=geometry)

df_aux <- df_aux %>% ungroup() %>% 
  left_join(estaciones_aux, by=c("site")) %>% 
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
  filter(mapa_rm==1 & rank==1) %>% 
  left_join(estaciones_mapa,by=c("site")) %>% 
  ggplot()+
  geom_sf(data=filter(mapa_comuna,mapa_rm==1),
          aes(geometry=geometry), fill="white")+
  geom_sf(aes(geometry=centroide), col="green")+
  geom_sf(aes(geometry=ubi),col="red")+
  geom_path(data=lineas, aes(x=longitud, y=latitud, group=codigo_comuna),col="red")+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 13)

f_savePlot(last_plot(), sprintf(file_name,
                                paste("EstacionCercana",
                                      est_file,sep="_")))

rm(lineas, estaciones_mapa)

# Clear all
rm(dist_matrix, est_file, file_out, df_dist, df, comunas, estaciones)

## EoF