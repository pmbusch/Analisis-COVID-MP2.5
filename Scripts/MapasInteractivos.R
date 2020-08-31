### Analisis-COVID-MP2.5
## Mapas interactivos de datos comunales y estaciones monitoreo
## PBH Agosto 2020

## Datos Comunales --------
# Nota: Debo cargar loadAllData antes


# Debo instalar la version en desarrollo para que funcione bien burst
# devtools::install_github("r-spatial/mapview@develop")
library(RColorBrewer)

df_modelo %>% names()
mapa <- left_join(mapa_comuna, df_modelo)


library(leaflet)
library(htmltools)
library(scales)


## OVERKILL 2: Leaflet
# Funcion permite modificar todos los mapas de manera simultanea
f_leafleft <- function(map, datos,var, columna, unidad){
  pal <- colorBin("YlOrRd", bins = 9, domain=datos %>% pull(columna))
  labels <- sprintf(
    "<strong>%s</strong><br/> %s: %s [%s]",
    datos$nombre_comuna, columna,
    comma(datos %>% pull(columna),0.1), unidad) %>% 
    lapply(HTML)
  
  map %>% 
    addPolygons(
    group = columna,
    # fill
    fillColor   = ~pal({{var}}),
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
      group = columna,
      pal = pal, 
      values = ~{{var}}, opacity = 0.7, 
      title = HTML(paste(columna, " [",unidad,"]",sep="")),
      position = "bottomleft")
}


mapa_filtro <- mapa %>% 
  dplyr::select(region, nombre_provincia, nombre_comuna, poblacion,
                tasa_mortalidad_covid, mp25, mp25_winter,
                densidad_pob, densidad_pob_censal,
                densidad_pob_manzana_mediana,densidad_pob_manzana_p90,
                `15-44`,`65+`,
                perc_rural, perc_puebloOrig,
                ingresoAutonomo_media, perc_menor_media, perc_isapre,perc_fonasa_A,
                perc_fonasa_B,perc_fonasa_C,perc_fonasa_D,
                perc_lenaCocina, perc_lenaCalefaccion,
                tasa_contagios, tasa_camas, dias_primerContagio, dias_primerMuerte,
                dias_cuarentena, perc_letalidad,
                tmed_summer,tmed_winter,hr_summer,hr_winter,heating_degree_15_summer,
                heating_degree_15_winter) %>% 
  mutate(`0-14`=100-`15-44`-`65+`)

# PRUEBA FUNCION
# leaflet(mapa_filtro) %>% 
#   addTiles() %>% 
#   f_leafleft(mapa_filtro,mapa_filtro$tasa_mortalidad,
#              "tasa_mortalidad", unidad = "por 100mil hab") %>% 
#   addLayersControl(
#     baseGroups = c("OSM (default)"),
#     overlayGroups = c("tasa_mortalidad")) %>% 
#   hideGroup(c("tasa_mortalidad"))


m_leaf <- leaflet(mapa_filtro) %>% 
  addTiles() %>% 
  f_leafleft(mapa_filtro,mapa_filtro$tasa_mortalidad_covid,
             "tasa_mortalidad_covid", unidad = "por 100mil hab") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$mp25,
             "mp25", unidad = "ug/m3") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$mp25_winter,
             "mp25_winter", unidad = "ug/m3") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$poblacion,
             "poblacion", unidad = "hab") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$tasa_contagios,
             "tasa_contagios", unidad = "por 100mil hab") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$densidad_pob,
             "densidad_pob", unidad = "hab/km2") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$densidad_pob_censal,
             "densidad_pob_censal", unidad = "hab/km2") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$`0-14`,
             "0-14", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$`15-44`,
             "15-44", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$`65+`,
             "65+", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$perc_rural,
             "perc_rural", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$perc_puebloOrig,
             "perc_puebloOrig", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$ingresoAutonomo_media,
             "ingresoAutonomo_media", unidad = "CLP mes") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$perc_menor_media,
             "perc_menor_media", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$perc_isapre,
             "perc_isapre", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$perc_fonasa_A,
             "perc_fonasa_A", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$perc_fonasa_D,
             "perc_fonasa_D", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$perc_lenaCocina,
             "perc_lenaCocina", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$perc_lenaCalefaccion,
             "perc_lenaCalefaccion", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$tasa_camas,
             "tasa_camas", unidad = "por 100mil hab") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$dias_primerContagio,
             "dias_primerContagio", unidad = "dias") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$dias_primerMuerte,
             "dias_primerMuerte", unidad = "dias") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$dias_cuarentena,
             "dias_cuarentena", unidad = "dias") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$perc_letalidad,
             "perc_letalidad", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$tmed_summer,
             "tmed_summer", unidad = "°C") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$tmed_winter,
             "tmed_winter", unidad = "°C") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$hr_summer,
             "hr_summer", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$hr_winter,
             "hr_winter", unidad = "%") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$heating_degree_15_summer,
             "heating_degree_15_summer", unidad = "°C") %>% 
  f_leafleft(mapa_filtro,mapa_filtro$heating_degree_15_winter,
             "heating_degree_15_winter", unidad = "°C") %>% 
  addLayersControl(
    baseGroups = c("OpenStreetMap","Toner", "Toner by Stamen"),
    overlayGroups = c("tasa_mortalidad_covid","mp25","mp25_winter",
                      "poblacion","tasa_contagios",
                      "densidad_pob",
                      "densidad_pob_censal","0-14","15-44","65+","perc_rural",
                      "perc_puebloOrig","ingresoAutonomo_media",
                      "perc_menor_media","perc_isapre","perc_fonasa_A",
                      "perc_fonasa_D","perc_lenaCocina",
                      "perc_lenaCalefaccion","tasa_camas",
                      "dias_primerContagio","dias_primerMuerte","dias_cuarentena",
                      "perc_letalidad","tmed_summer","tmed_winter",
                      "hr_summer","hr_winter","heating_degree_15_summer",
                      "heating_degree_15_winter")) %>% 
  hideGroup(c("tasa_mortalidad_covid","mp25","mp25_winter",
              "poblacion","tasa_contagios",
              "densidad_pob","densidad_pob_censal","0-14","15-44","65+","perc_rural",
              "perc_puebloOrig","ingresoAutonomo_media",
              "perc_menor_media","perc_isapre","perc_fonasa_A",
              "perc_fonasa_D","perc_lenaCocina",
              "perc_lenaCalefaccion","tasa_camas",
              "dias_primerContagio","dias_primerMuerte","dias_cuarentena",
              "perc_letalidad","tmed_summer","tmed_winter",
              "hr_summer","hr_winter","heating_degree_15_summer",
              "heating_degree_15_winter"))
# m_leaf

mapshot(m_leaf, "Figuras/MapaParametrosComuna.html", selfcontained=F)
rm(mapa_filtro, mapa, m_leaf, f_leafleft)

library(leafsync)

m_leaf_dual <- sync(m_leaf,m_leaf, ncol=2)
mapshot(m_leaf_dual, "Figuras/MapaParametrosComunaDual.html", selfcontained=F)

rm(m_leaf,m_leaf_dual)

## Estaciones Monitoreo -------------
library(sf)
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
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
  summarise(avg=mean(avg, na.rm=T)) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)
rm(df_conc)
estaciones_sinca <- st_as_sf(estaciones_sinca, 
                       coords = c("longitud","latitud"),
                       remove = F, 
                       crs="+proj=longlat +ellps=GRS80 +no_defs")


# Estaciones Sinca Meteo
df_conc <- read_rds("Data/Data_Modelo/Datos_MeteorologiaSinca_raw.rsd")
df_conc <- df_conc %>% filter(year %in% 2010:2019)
estaciones_sinca_meteo <- df_conc %>% 
  group_by(codigo_comuna,site, longitud, latitud,year) %>% 
  summarise(count=n()) %>% ungroup() %>% 
  group_by(site, codigo_comuna, longitud, latitud) %>% 
  summarise(count=n()) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)
rm(df_conc)
estaciones_sinca_meteo <- st_as_sf(estaciones_sinca_meteo, 
                             coords = c("longitud","latitud"),
                             remove = F, 
                             crs="+proj=longlat +ellps=GRS80 +no_defs")


# Estaciones Meteorologia
df_meteo <- read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")
df_meteo <- df_meteo %>% filter(year(date) %in% 2010:2019)
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

m <- mapview(estaciones_sinca, label=estaciones_sinca$site, col.regions="red",
          layer.name = c("Estaciones Sinca"))+
  mapview(estaciones_sinca_meteo, label=estaciones_sinca_meteo$site, col.regions="brown",
          layer.name = c("Estaciones Sinca Meteorologia"))+
  mapview(estaciones_meteo, label=estaciones_meteo$estacion, col.regions="orange",
          layer.name = c("Estaciones Meteorologia"))
  # mapview(comunas_mapa, label=comunas_mapa$nombre_comuna, col.regions="blue",
  #         layer.name = c("Centroides Comuna"))


mapa_comuna_view <- mapa_comuna %>% 
  left_join(codigos_territoriales)
m_comunas <- mapview(mapa_comuna_view,
                     layer.name = c("Comunas"),label=mapa_comuna_view$nombre_comuna,
                     alpha.regions=0.1, col.regions="green")
rm(mapa_comuna_view)

# area_poblada <- st_read('Data/Data_Original/Areas_Pobladas/Areas_Pobladas.shp',
#                         layer = "Areas_Pobladas")
# area_poblada$Entidad %>% unique()
# m_area_poblada <- mapview(area_poblada,label=area_poblada$comuna,
#                           layer.name = c("Area Poblada bcn"),
#                           alpha.regions=0.1, col.regions="yellow")
# rm(area_poblada)

## Unir zonas censales a nivel de comuna
zonas <- rmapshaper::ms_dissolve(mapa_zonas %>% st_as_sf(), 
                                 field = "codigo_comuna") %>%
  left_join(codigos_territoriales)
zonas %>% class()

zonas_centroide <- zonas %>% as.data.frame() %>% 
  mutate(centroide=st_centroid(geometry),
         cent_lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         cent_lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_as_sf(coords = c("cent_lon","cent_lat"),
           remove = F, 
           crs="+proj=longlat +ellps=GRS80 +no_defs")

m_zonas <- mapview(zonas, label=zonas$nombre_comuna,
                   layer.name = c("Zonas Urbanas"),
                   alpha.regions=0.1, col.regions="purple")+
  mapview(zonas_centroide, label=zonas_centroide$nombre_comuna,
          layer.name = c("Centroide Zonas Urbanas"),
          col.regions="blue")
rm(zonas_centroide)


## Buffer estaciones 20 km
# https://stackoverflow.com/questions/60895518/why-is-st-buffer-function-not-creating-an-r-object-that-correctly-displays-in-ma
# library(rgeos)
# library(rgdal)
dist_buffer <- 20
# EPSG:5361 = SIRGAS-Chile 2002 / UTM zone 19S
buffer_sinca <- estaciones_sinca %>% st_transform(5361) %>% 
  st_buffer(dist_buffer*1e3) %>% 
  st_transform(4326)
buffer_Sincameteo <- estaciones_sinca_meteo %>% st_transform(5361) %>% 
  st_buffer(dist_buffer*1e3) %>% 
  st_transform(4326)
buffer_meteo <- estaciones_meteo %>% st_transform(5361) %>% 
  st_buffer(dist_buffer*1e3) %>% 
  st_transform(4326)

m_buffer <- mapview(buffer_sinca, label=buffer_sinca$site, col.regions="red",
        layer.name = c("Rango Estaciones Sinca"),alpha.regions=0.1 )+
  mapview(buffer_Sincameteo, label=buffer_Sincameteo$site, col.regions="brown",
          layer.name = c("Rango Estaciones Sinca Meteorologia"),alpha.regions=0.1 )+
  mapview(buffer_meteo, label=buffer_meteo$nombre_estacion, col.regions="orange",
          layer.name = c("Rango Estaciones Meteorologia"),alpha.regions=0.1 )

# Unir todo
m_all <- m+m_comunas+m_zonas+m_buffer

# Save file as html
mapshot(m_all, "Figuras/Distancia_Monitoreo/EstacionesMonitoreo.html",
        selfcontained=F)

rm(m, m_comunas, comunas_mapa,m_zonas, m_buffer,
   m_area_poblada,m_all, estaciones_sinca, estaciones_meteo)


## PRUEBAS ------------
# Mapa Tasa Mortalidad
m <- mapview(mapa, label=mapa$nombre_comuna, 
             zcol=c("tasa_mortalidad"), 
             col.regions=brewer.pal(9, "YlOrRd"))
m


mapa2 <- mapa %>% dplyr::select(geometry, nombre_comuna,
                                tasa_mortalidad, densidad_pob_censal,
                                perc_isapre, tasa_camas, perc_rural)

m2 <- mapview(mapa2, label=mapa2$nombre_comuna, 
              burst=T, hide=T,
              col.regions=brewer.pal(9, "YlOrRd"))
m2


m3 <- mapview(mapa, label=mapa2$nombre_comuna, 
              zcol=c("tasa_mortalidad","densidad_pob_censal",
                     "perc_isapre", "tasa_camas", "perc_rural"),
              hide=T,
              col.regions=brewer.pal(9, "YlOrRd"))
m3

# Alternativa con leaflet
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
  hideGroup(c("Tasa Mortalidad"))
m1

## Mapa todo old -----
# Desestimado pq se demora mucho en guardar, opcion leaflet funciona mejor

# Mapa Varios parametros
# OVERKILL: pero parametro Burst no me funciona ni en los ejemplos
# Funcion permite modificar todos los mapas de manera simultanea
# Genera un archivo muy grande 65MB, y demora 36 min en guardar!!!
f_mapview <- function(datos, columna){
  lab_data <- datos %>% pull(columna) %>% round(2)
  mapview(datos, 
          label=paste(datos$nombre_comuna,": ",lab_data,sep=""), 
          zcol=columna,
          layer.name=columna,
          col.regions=brewer.pal(9, "YlOrRd"),
          hide=T)
}

mapa_filtro <- mapa %>% 
  dplyr::select(region, nombre_provincia, nombre_comuna, poblacion,
                tasa_mortalidad, mp25, densidad_pob, densidad_pob_censal,`15-44`,`65+`,
                perc_rural, perc_puebloOrig,
                ingresoAutonomo_media, perc_menor_media, perc_isapre,perc_fonasa_A,
                perc_fonasa_B,perc_fonasa_C,perc_fonasa_D,
                perc_lenaCocina, perc_lenaCalefaccion,
                tasa_contagios, tasa_camas, dias_primerContagio, dias_cuarentena, perc_letalidad,
                tmed_summer,tmed_winter,hr_summer,hr_winter,heating_degree_15_summer,
                heating_degree_15_winter)

f_mapview(mapa_filtro, "tasa_mortalidad")

m <- f_mapview(mapa_filtro,"tasa_mortalidad")+
  f_mapview(mapa_filtro,"mp25")+
  f_mapview(mapa_filtro,"poblacion")+
  f_mapview(mapa_filtro,"tasa_contagios")+
  f_mapview(mapa_filtro,"densidad_pob")+
  f_mapview(mapa_filtro,"densidad_pob_censal")+
  f_mapview(mapa_filtro,"15-44")+
  f_mapview(mapa_filtro,"65+")+
  f_mapview(mapa_filtro,"perc_rural")+
  f_mapview(mapa_filtro,"perc_puebloOrig")+
  f_mapview(mapa_filtro,"ingresoAutonomo_media")+
  f_mapview(mapa_filtro,"perc_menor_media")+
  f_mapview(mapa_filtro,"perc_isapre")+
  f_mapview(mapa_filtro,"perc_fonasa_A")+
  f_mapview(mapa_filtro,"perc_fonasa_D")+
  f_mapview(mapa_filtro,"perc_lenaCocina")+
  f_mapview(mapa_filtro,"perc_lenaCalefaccion")+
  f_mapview(mapa_filtro,"tasa_camas")+
  f_mapview(mapa_filtro,"dias_primerContagio")+
  f_mapview(mapa_filtro,"dias_cuarentena")+
  f_mapview(mapa_filtro,"perc_letalidad")+
  f_mapview(mapa_filtro,"tmed_summer")+
  f_mapview(mapa_filtro,"tmed_winter")+
  f_mapview(mapa_filtro,"hr_summer")+
  f_mapview(mapa_filtro,"hr_winter")+
  f_mapview(mapa_filtro,"heating_degree_15_summer")+
  f_mapview(mapa_filtro,"heating_degree_15_winter")


# Save file as html
mapshot(m, "Figuras/MapaParametrosComuna.html", selfcontained=F)
rm(mapa, m, f_mapview)


## EoF