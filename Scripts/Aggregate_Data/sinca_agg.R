### Analisis-COVID-MP2.5
## Agrega datos de concentraciones
## PBH Julio 2020

## METODO 1: Expansion concentracion FIXED RADIUS ------
# Carga datos Concentracion --------
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")
df <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")


## Expandir a otros años y season ---------
## Añadir Año especial (promedio 2017-2019 y años por separado)
df <- df %>%
  filter(year %in% 2017:2020 & pollutant=="mp2.5") %>%
  mutate(season=as.character(year))
df$season %>% unique()

# Df con promedios por años
df_anos <- df
df <- df %>% filter( year %in% 2017:2019) %>% mutate(season=NULL)

## Añadir Season (promedio 2017-2019)
df %>% names() %>% sort()
df <- df %>% mutate(season=getSeason(date))
## Add anual: promedio 2017-2019
df <- df %>% mutate(season="anual") %>% rbind(df)
# Add other years as season
df <- df %>% rbind(df_anos)
df$season %>% unique(); df$pollutant %>% unique()
rm(df_anos)

# Concentracion promedio en periodos seleccionados (season)
df_conc <- df %>% 
  group_by(codigo_comuna, pollutant, unidad, site,year,season) %>% 
  summarise(valor=mean(valor, na.rm=T),
            disponibilidad=n()/365) %>% ungroup()

# Disponibilidad mayor a 80% en el año, y con los tres años de datos
sitios_validos <- df_conc %>% 
  filter(season=="anual" & disponibilidad>0.8) %>% 
  group_by(codigo_comuna,site) %>% 
  summarise(valor=mean(valor, na.rm=T),
            count=n()) %>% ungroup() %>% 
  filter(count==3) %>% pull(site)


df_conc <- df_conc %>% 
  filter(site %in% sitios_validos) %>% 
  group_by(codigo_comuna, site, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()
rm(sitios_validos)
df_conc$site %>% unique()


## Cruze con datos distancia: Fixed Radius
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
df_avg <- df_avg %>% left_join(df_conc %>% select(-codigo_comuna), 
                               by=c("site")) %>% filter(!is.na(valor))

## Promedio ponderado por inverso de la distancia a nivel de zona censal
df_mp <- df_avg %>% 
  group_by(geocodigo, codigo_comuna, poblacion, season) %>% 
  summarise(valor=weighted.mean(valor, 1/(dist)),
            count=n()) %>% ungroup()

# View map on zonas
library(RColorBrewer)
m2 <- left_join(mapa_zonas, df_mp, by=c("geocodigo")) %>% st_as_sf()
m3 <- m2 %>% filter(season=="anual") %>% 
  mapview(zcol="valor",col.regions=brewer.pal(9, "YlOrRd"))
mapshot(m3, "Figuras/ConcentracionMP25_zonas.html",
        selfcontained=F)
rm(m2,m3)

## Promedio a nivel de comuna ponderado por la poblacion
df_mp <- df_mp %>% 
  group_by(codigo_comuna, season) %>% 
  summarise(valor=weighted.mean(valor, poblacion)) %>% ungroup() %>% 
  mutate(season=paste("mp25",season,sep="_")) %>% 
  spread(season,valor) %>% rename(mp25=mp25_anual) %>% 
  right_join(mapa_comuna) %>% left_join(codigos_territoriales)

df_mp %>% filter(!is.na(mp25)) %>% nrow() # N comunas con datos:120

m1 <- mapview(df_mp %>% st_as_sf(), 
      label=paste(df_mp$nombre_comuna,": ",
                  round(df_mp$mp25,2),"[ug/m3]",sep=""),
      layer.name="Concentracion MP2.5 2016-2019",
      zcol="mp25",
      na.color="white",
      col.regions=brewer.pal(9, "YlOrRd"))
m1
mapshot(m1, "Figuras/ConcentracionMP25.html", selfContained=F)
rm(m1)

# Guarda datos ----------
df_conc <- df_mp %>% 
  select(codigo_comuna, mp25, mp25_fall, mp25_winter, mp25_spring, mp25_summer,
         mp25_2017, mp25_2018, mp25_2019, mp25_2020) %>% 
  filter(!is.na(mp25))
saveRDS(df_conc, "Data/Data_Modelo/Datos_Concentraciones.rsd")

## Mapas Chile --------
# Chile Facet
file_name <- "Scripts/Analisis_Exploratorios/Figuras/SINCA/%s.png"
df_mp$mp25 %>% range(na.rm = T)
fig_mapaChile_facet(df_mp, mp25, limites=c(0,50),
                    titulo = "Promedio 2017-2019 \n MP2.5 [ug/m3]")
f_savePlot(last_plot(),
           file_path =sprintf(file_name,"MapaChileMP25Facet_Exp"),dpi=300)

# Santiago
df_mp %>% 
  filter(mapa_rm==1) %>% 
  fig_mapa(mp25,limites = c(0,50), titulo="Promedio 2017-2019 \n MP2.5 [ug/m3]")
f_savePlot(last_plot(), sprintf(file_name,"MapaSantiagoMP25_Exp"))



## METODO 2: Asignacion comuna donde se encuentra el monitor ------------
# Carga datos brutos --------
# source("Scripts/Load_Data/sinca_scrap.R", encoding = "UTF-8") # Baja los datos
df <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")

## Agregar a nivel comunal -----------
# Promedio 2017-2019
df_conc <- df %>% 
  filter(year %in% 2017:2019 & pollutant=="mp2.5") %>% 
  group_by(codigo_comuna, pollutant, unidad, site,year) %>% 
  summarise(valor=mean(valor, na.rm=T),
            disponibilidad=n()/365) %>% ungroup()
# Disponibildiad mayor a 80% y estaciones con todos los años de datos
df_conc <- df_conc %>% 
  filter(disponibilidad>0.8) %>% 
  group_by(site,codigo_comuna, pollutant, unidad) %>% 
  summarise(valor=mean(valor, na.rm=T),
            count=n()) %>% ungroup() %>% 
  filter(count==3) %>% select(-count)

# Numero estaciones
df_conc %>% n_distinct("site")

# Promedio por comuna
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
## EoF