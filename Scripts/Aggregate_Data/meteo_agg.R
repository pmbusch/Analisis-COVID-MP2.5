### Analisis-COVID-MP2.5
## Agrego datos meteorologicos a nivel de comuna
## PBH Julio 2020


### METODO 1: Uso datos SINCA ----------------
# Util para modelo transversal incorporando la contaminacion atmosferica
# Entrega mayor precision al HD, dado qe datos vienen en nivel horario

# Carga Datos ------
df <-read_rds("Data/Data_Modelo/Datos_MeteorologiaSinca_raw.rsd")
source("Scripts/00-Funciones.R", encoding = "UTF-8")

# Filtro por años de interes (reduce tiempos de operacion mas adelante)
df <- df %>% filter(year %in% 2017:2019)
df %>% names()
df$pollutant %>% unique()
df$date %>% year() %>% unique()

# Extraigo temp y hr ----
df <- df %>% mutate(tipo=str_extract(pollutant,"temp|hr") %>% 
                      str_replace_all("temp","tmed"))

# Eligo altura de medicion con mayor cantidad de datos ----
df_site <- df %>% 
  group_by(codigo_comuna,site,pollutant,tipo) %>% 
  summarise(count=n()) %>% ungroup()
df_site <- df_site %>% 
  group_by(codigo_comuna,site,tipo) %>% 
  summarise(maxCount=max(count),
            pollutantMax=pollutant[which.max(count)]) %>% ungroup()
tupla_site_pollutant <- df_site %>% 
  mutate(tupla=paste(site,pollutantMax,sep="-")) %>% pull(tupla) %>% unique()
# Filtro en datos
df <- df %>% mutate(tupla=paste(site,pollutant,sep="-")) %>% 
  filter(tupla %in% tupla_site_pollutant) %>% select(-tupla)
rm(tupla_site_pollutant,df_site)

# Numero estaciones: 79
df$site %>% unique() %>% length()

## Calculo Heating Degree ----
df %>% filter(tipo=="tmed") %>% pull(unidad) %>% unique()
df %>% filter(tipo=="tmed") %>% pull(valor) %>% range()
# 18 grados
df_hd18 <- df %>% 
  filter(tipo=="tmed") %>%
  mutate(heating_degree=18-valor,
         heating_degree=if_else(heating_degree>0,heating_degree, 0),
         tipo="heating_degree_18",
         valor=NULL) %>% 
  rename(valor=heating_degree)
# 15 grados
df_hd15 <- df %>% 
  filter(tipo=="tmed") %>%
  mutate(heating_degree=15-valor,
         heating_degree=if_else(heating_degree>0,heating_degree, 0),
         tipo="heating_degree_15",
         valor=NULL) %>% 
  rename(valor=heating_degree)       

df <- df %>% rbind(df_hd18) %>% rbind(df_hd15)
rm(df_hd18,df_hd15)


## Promedio diario ----
df_dia <- df %>% 
  group_by(codigo_comuna,site, tipo, year, month, day) %>% 
  summarise(valor=mean(valor, na.rm=T),
            count=n()) %>% ungroup()
# Filtro 75% de datos en un dia, y expando Heating degree para un dia entero
df_dia <- df_dia %>% 
  filter(count>=18) %>% 
  mutate(valor=if_else(str_detect(tipo,"heating_degree"),valor*24,valor))

## Duplico datos para hacer estimacion por season y anual ----
df_season <- df_dia %>% 
  mutate(season=getSeason(paste(day,month,year,sep="-") %>% 
                            strptime("%d-%m-%Y") %>% as_date()))

df_dia <- df_dia %>% mutate(season="anual") %>% rbind(df_season)
rm(df_season)

df_anual <- df_dia %>% 
  group_by(codigo_comuna, site, tipo,year,season) %>% 
  summarise(valor=mean(valor, na.rm=T),
            disponibilidad=n()/365) %>% ungroup()

# Disponibilidad mayor a 80% en el año, y con los tres años de datos -----
sitios_validos <- df_anual %>% 
  filter(season=="anual" & disponibilidad>0.8) %>% 
  group_by(codigo_comuna,site, tipo) %>% 
  summarise(valor=mean(valor, na.rm=T),
            count=n()) %>% ungroup() %>% 
  filter(count==3) %>% mutate(tupla=paste(site,tipo,sep="-")) %>% pull(tupla)

df_anual <- df_anual %>% 
  mutate(tupla=paste(site,tipo,sep="-")) %>% 
  filter(tupla %in% sitios_validos) %>% 
  group_by(codigo_comuna,site, tipo,season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()
rm(sitios_validos)

df_anual$site %>% unique() %>% length() # 42

rm(df,df_dia)


## Expansion datos FIXED RADIUS 50 KM ---------
## Cargar datos distancia
df_dist <- read_rds("Data/Data_Modelo/distanciacomunaEstacionsinca.rsd")
df_dist_zona <- read_rds("Data/Data_Modelo/distanciazonaEstacionsinca.rsd")

# Cruzo con estaciones dentro del rango del centroide de cada comuna
# Nota: centroide debe ser estimado mediante zonas censales
corte_km <- 50
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

# Add  data
df_avg <- df_avg %>% left_join(df_anual %>% select(-codigo_comuna), 
                               by=c("site")) %>% filter(!is.na(valor))

## Promedio ponderado por inverso de la distancia a nivel de zona censal
df_meteo <- df_avg %>% 
  group_by(geocodigo, codigo_comuna, poblacion, tipo, season) %>% 
  summarise(valor=weighted.mean(valor, 1/(dist)),
            count=n()) %>% ungroup()

## Promedio a nivel de comuna ponderado por la poblacion
df_meteo <- df_meteo %>% 
  group_by(codigo_comuna,tipo,season) %>% 
  summarise(valor=weighted.mean(valor, poblacion)) %>% ungroup()

## Expando los datos
df_meteo <- df_meteo %>% 
  mutate(tipo=paste(tipo, season, sep="_"),
         season=NULL) %>% 
  spread(tipo,valor)

df_meteo %>% nrow() # N comunas con datos:195
df_meteo %>% skim()

# Save data
saveRDS(df_meteo, "Data/Data_Modelo/Datos_Meteorologia.rsd")

## Mapas ------
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")
file_name <- "Scripts/Analisis_Exploratorios/Figuras/meteo/%s.png"
df_map <- df_meteo %>% right_join(mapa_comunas)
df_map %>% names()

# Temperatura
df_map$tmed_anual %>% range(na.rm=T)
fig_mapaChile_facet(df_map, tmed_anual, limites = c(8,17),
                    titulo = "Temperatura media 2017-2019 \n [°C]")
f_savePlot(last_plot(),file_path = sprintf(file_name,"MapaChileFacetTemp_ext"))

# HR
df_map$hr_anual %>% range(na.rm=T)
fig_mapaChile_facet(df_map, hr_anual, limites = c(0,100),
                    titulo = "Humedad relativa 2017-2019 \n [%]")
f_savePlot(last_plot(),file_path = sprintf(file_name,"MapaChileFacetHr_ext"))

# Heating Degree 15
df_map$heating_degree_15_anual %>% range(na.rm=T)
fig_mapaChile_facet(df_map, heating_degree_15_anual, limites = c(30,170),
                    titulo = "Heating Degree diario 2017-2019 \n [°C]")
f_savePlot(last_plot(),file_path = sprintf(file_name,"MapaChileFacetHD15_ext"))


## METODO 2: Uso Datos Meteorologia Chile -------------
## Util para un analisis sin MP, tiene mayor numero de comunas
# No se puede calcular Heating Degree a nivel horario
# Carga datos brutos --------
# source("Scripts/Load_Data/meteo_scrap.R", encoding = "UTF-8") # Baja los datos
df <-read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")
source("Scripts/00-Funciones.R", encoding = "UTF-8")

df$tipo %>% unique()
df$date %>% year() %>% unique()

# Numero estaciones: 416
df$estacion %>% unique() %>% length()

## Agrego season anual
df <- df %>% mutate(season="anual") %>% rbind(df)

## Agregar a nivel de estacion y promediar por años ----------------
df_meteo <- df %>% 
  mutate(year=year(date)) %>% 
  filter(year %in% 2017:2019 &
           tipo %in% c("tmed", "hr","heating_degree_18","heating_degree_15")) %>% 
  group_by(estacion, tipo,year, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()

## Faltan filtros por disponibilidad!

df_meteo <- df_meteo %>% 
  group_by(estacion, tipo, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()

## Expando los datos
df_meteo <- df_meteo %>% 
  mutate(tipo=paste(tipo, season, sep="_"),
         season=NULL) %>% 
  filter(!is.nan(valor))

## CRITERIO TEMPORAL: Asigno datos de monitor mas cercano a cada comuna
# Cruzo todos los datos
df_dist <- read_rds("Data/Data_Modelo/distanciaComunaEstacionMeteo.rsd")
df_dist <- df_dist %>% filter(rank==1)
df_com <- df_dist %>% select(codigo_comuna, site, dist, rank) %>% 
  left_join(df_meteo, by=c("site"="estacion")) %>% na.omit()

## Filtro por ranking mayor
(df_com$tipo %>% unique() %>% length())*343 # Largo arreglo, tipos x comunas
df_meteo <- df_com %>% 
  group_by(codigo_comuna, tipo) %>% 
  summarise(rank=min(rank),
            site=site[which.min(rank)],
            dist=dist[which.min(rank)],
            valor=valor[which.min(rank)]) %>% ungroup()

# Rango distancia
df_meteo$dist %>% range()/1e3

# Expandir datos
df_meteo <- df_meteo %>% 
  select(-rank,-site, -dist) %>% 
  spread(tipo,valor)

df_meteo %>% skim()

# N comunas con al menos 1 dato de meteorologia (todas dado la aproximacion utilizada)
df_meteo %>% n_distinct("estacion")

# Save data
saveRDS(df_meteo, "Data/Data_Modelo/Datos_Meteorologia.rsd")



## OTRAS PRUEBAS: Comparacion Horario MeteoCHile--------
df_hora <- read_rds("Data/Data_Modelo/Datos_Meteorologia_raw_horario.rsd")



# Numero estaciones (364 vs 60)
df %>% filter(tipo=="tmed") %>% pull(estacion) %>% unique() %>% length()
df_hora$estacion %>% unique() %>% length()

estaciones <- df %>% filter(tipo=="tmed") %>% pull(estacion) %>% 
  unique() %>% as_tibble() %>% rename(site=value)
estaciones_hora <- df_hora$estacion %>% unique() %>% 
  as_tibble() %>% rename(site=value) %>% mutate(estacion=site)

estaciones <- left_join(estaciones, estaciones_hora)


# estacion sin datos se pueden descargar segun este url
# https://climatologia.meteochile.gob.cl/application/historicos/datosHistoricosEma/230004
# https://climatologia.meteochile.gob.cl/application/productos/getDatosHistoricosEma/230004/230004_202007_Temperatura.csv.zip
# NOTA: Son zip engorrosos con datos horarios, para cada mes un zip


# prubea
library(rvest)

web <- read_html("https://climatologia.meteochile.gob.cl/application/historicos/datosHistoricosEma/230004")

# Select by parameter
url_temp <- html_node(web, "[aria-labelledby='Temperatura']")

url_temp
## Get all zip of months data
url_temp %>% str_extract_all("http.*zip")

## PROBLEMA: PRESENTAN DATOS HASTA 2018, no tengo como saber cuanto existe hacia atras





## EoF