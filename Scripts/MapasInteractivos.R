### Analisis-COVID-MP2.5
## Mapas interactivos de datos comunales y estaciones monitoreo
## PBH Agosto 2020


## Datos Comunales --------
# Nota: Debo cargar loadAllData antes

library(RColorBrewer)
df_modelo %>% names()
mapa <- left_join(mapa_comuna, df_modelo)

# Debo instalar la version en desarrollo para que funcione bien burst
# devtools::install_github("r-spatial/mapview@develop")


# Mapa Tasa Mortalidad
m <- mapview(mapa, label=mapa$nombre_comuna, 
             zcol=c("tasa_mortalidad"), 
             col.regions=brewer.pal(9, "YlOrRd"))
m


mapa2 <- mapa %>% select(geometry, nombre_comuna,
                         tasa_mortalidad, densidad_pob_censal,
                         perc_isapre, tasa_camas, perc_rural)

m2 <- mapview(mapa2, label=mapa2$nombre_comuna, 
             burst=T, hide=T,
             col.regions=brewer.pal(9, "YlOrRd"))
m2



# Alternativa con leaflet ----
library(leaflet)
library(htmltools)
library(scales)

pal <- colorBin("YlOrRd", bins = 9, domain=mapa$tasa_mortalidad)
labels <- sprintf(
  "<strong>%s</strong><br/> Tasa Mortalidad: %s [por 100mil hab]",
  mapa$nombre_comuna, comma(mapa$tasa_mortalidad,0.1)) %>% 
  lapply(HTML)

m1 <- leaflet(mapa) %>% 
  addTiles() %>% 
  addPolygons(
    group = "Tasa Mortalidad",
    # fill
    fillColor   = ~pal(tasa_mortalidad),
    fillOpacity = 0.7,
    # line
    dashArray   = "3",
    weight      = 0.1,
    color       = "white",
    opacity     = 1,
    # interaction
    highlight = highlightOptions(
      weight = 1,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(
    group = "Tasa Mortalidad",
    pal = pal, 
    values = ~tasa_mortalidad, opacity = 0.7, 
    title = HTML("Tasa Mortalidad [por 100mil hab]"),
    position = "bottomright") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)"),
    overlayGroups = c("Tasa Mortalidad")) %>% 
  hideGroup("Tasa Mortalidad")
m1



# Mapa Varios parametros
# OVERKILL: pero parametro Burst no me funciona ni en los ejemplos
# Funcion permite modificar todos los mapas de manera simultanea
# Genera un archivo muy grande 65MB, y demora 36 min en guardar!!!
f_mapview <- function(datos, columna){
  lab_data <- datos %>% pull(columna) %>% round(2)
  mapview(datos, 
          label=paste(datos$nombre_comuna,": ",lab_data,sep=""), 
          zcol=columna, 
          col.regions=brewer.pal(9, "YlOrRd"),
          hide=T)
}
f_mapview(mapa, "tasa_mortalidad")

mapa_filtro <- mapa %>% 
  select(region, nombre_provincia, nombre_provincia, 
         tasa_mortalidad, mp25, densidad_pob, densidad_pob_censal,
         `65+`,ingresoAutonomo_media, perc_menor_media, perc_isapre,
         tasa_contagios, tasa_camas, dias_primerContagio, dias_cuarentena)


m <- f_mapview(mapa_filtro,"tasa_mortalidad")+
  f_mapview(mapa_filtro,"mp25")+
  f_mapview(mapa_filtro,"tasa_contagios")+
  f_mapview(mapa_filtro,"densidad_pob")+
  f_mapview(mapa_filtro,"densidad_pob_censal")+
  f_mapview(mapa_filtro,"65+")+
  f_mapview(mapa_filtro,"ingresoAutonomo_media")+
  f_mapview(mapa_filtro,"perc_menor_media")+
  f_mapview(mapa_filtro,"perc_isapre")+
  f_mapview(mapa_filtro,"tasa_camas")+
  f_mapview(mapa_filtro,"dias_primerContagio")+
  f_mapview(mapa_filtro,"dias_cuarentena")

# Save file as html
mapshot(m, "Figuras/MapaParametrosComuna.html", selfcontained=F)
rm(mapa, m, f_mapview)


## Estaciones Monitoreo -------------
library(sf)
# Comunas
comunas <- mapa_comuna %>% as.data.frame() %>% 
  left_join(codigos_territoriales) %>% 
  mutate(centroide=st_centroid(geometry),
         cent_lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         cent_lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))

comunas_mapa <- st_as_sf(comunas, 
                         coords = c("cent_lon","cent_lat"),
                         remove = F, 
                         crs="+proj=longlat +ellps=GRS80 +no_defs")
rm(comunas)

# Estaciones Sinca
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")
df_conc <- df_conc %>% filter(year %in% 2010:2019 & pollutant=="mp2.5")
estaciones_sinca <- df_conc %>% 
  group_by(codigo_comuna,site, longitud, latitud,year) %>% 
  summarise(avg=mean(valor, na.rm=T),
            disponibilidad=n()/365) %>% ungroup() %>% 
  filter(disponibilidad>0.8) %>% 
  group_by(site, codigo_comuna, longitud, latitud) %>% 
  summarise(avg=mean(avg, na.rm=T)) %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)
rm(df_conc)
estaciones_sinca <- st_as_sf(estaciones_sinca, 
                       coords = c("longitud","latitud"),
                       remove = F, 
                       crs="+proj=longlat +ellps=GRS80 +no_defs")



# Estaciones Meteorologia
df_meteo <- read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")
df_meteo <- df_meteo %>% filter(year(date) %in% 2016:2019)
estaciones_meteo <- df_meteo %>% 
  group_by(codigo_comuna,estacion,nombre_estacion, longitud, latitud) %>% 
  summarise(count=n()) %>% ungroup() %>% 
  select(-count) %>% na.omit() %>% 
  left_join(codigos_territoriales)
rm(df_meteo)

estaciones_meteo <- st_as_sf(estaciones_meteo, 
                       coords = c("longitud","latitud"),
                       remove = F, 
                       crs="+proj=longlat +ellps=GRS80 +no_defs")

## Exportacion Mapa interactivo 
library(mapview)
library(leaflet)


m <- mapview(comunas_mapa, label=comunas_mapa$nombre_comuna, col.regions="blue",
             layer.name = c("Centroides Comuna"))+
  mapview(estaciones_sinca, label=estaciones_sinca$site, col.regions="red",
          layer.name = c("Estaciones Monitoreo"))+
  mapview(estaciones_meteo, label=estaciones_meteo$estacion, col.regions="orange",
          layer.name = c("Estaciones Meteorologia"))


mapa_comuna_view <- mapa_comuna %>% 
  left_join(codigos_territoriales)
m_comunas <- mapview(mapa_comuna_view,
                     layer.name = c("Comunas"),label=mapa_comuna_view$nombre_comuna,
                     alpha.regions=0.1, col.regions="green")
rm(mapa_comuna_view)

area_poblada <- st_read('Data/Data_Original/Areas_Pobladas/Areas_Pobladas.shp',
                        layer = "Areas_Pobladas")
area_poblada$Entidad %>% unique()
m_area_poblada <- mapview(area_poblada,label=area_poblada$comuna,
                          layer.name = c("Area Poblada bcn"),
                          alpha.regions=0.1, col.regions="yellow")
rm(area_poblada)


zonas <- mapa_zonas %>% st_as_sf() %>% left_join(codigos_territoriales)
zonas %>% class()
m_zonas <- mapview(zonas, label=zonas$nombre_comuna,
                   layer.name = c("Zonas Urbanas"),
                   alpha.regions=0.1, col.regions="orange")

## Unir zonas censales a nivel de comuna
library(rmapshaper)
zonas %>% names()
zonas_agg <- ms_dissolve(zonas, field = "codigo_comuna") %>%
  left_join(codigos_territoriales)
m_zonas_agg <- mapview(zonas_agg, label=zonas_agg$nombre_comuna,
                       layer.name = c("Zonas Urbanas Agregadas"),
                       alpha.regions=0.1, col.regions="purple")
rm(zonas, zonas_agg)

m_all <- m+m_comunas+m_area_poblada+m_zonas_agg

# Save file as html
mapshot(m_all, "Figuras/Completa_MP/EstacionesMonitoreo.html")

rm(m, m_comunas, comunas_mapa,m_zonas, m_zonas_agg,
   m_area_poblada,m_all, estaciones_sinca, estaciones_meteo)


## EoF