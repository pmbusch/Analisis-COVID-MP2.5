### Analisis-COVID-MP2.5
## Meteorologia
## PBH Julio 2020

theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_Exploratorios/Figuras/meteo/%s.png"
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")

# Carga datos brutos y Mapa --------
df_meteo <- read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")
df_meteo <- df_meteo %>% mutate(year=year(date))


## Numero estaciones con datos por año ------
df_anual <- df_meteo %>% 
  filter(year %in% 2010:2019) %>% 
  group_by(estacion,codigo_comuna, tipo, year) %>% 
  summarise(valor=mean(valor, na.rm=T),
            disponibilidad=n()/365) %>% ungroup()

df_anual %>% 
  # filter(disponibilidad>0.8) %>%
  filter(tipo %in% c("tmed","tmax","tmin","hr")) %>% 
  group_by(year,tipo) %>% 
  summarise(count=n()) %>% ungroup() %>% 
  ggplot(aes(year, count))+
  geom_col(fill="brown")+
  facet_wrap(~tipo)+
  scale_x_continuous(breaks=2010:2019)+
  coord_cartesian(expand = F)+
  labs(x="", y="Numero estaciones con datos")
f_savePlot(last_plot(), sprintf(file_name,"NSite_anos_meteo"))

rm(df_anual)

# Promedio 2010-2019
df_avg <- df_meteo %>% 
  filter(year %in% 2010:2019) %>% 
  group_by(estacion,region,codigo_comuna, year,tipo) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% 
  group_by(estacion, region, codigo_comuna, tipo) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()


## MAPAS ------------------
# NOTA IMPORTANTE: Dado que voy a usar facets, cada tipo debe contener
# todas las comunas con su NA correspondiente
# Por eso expando, hago el join y dp aplano, para asegurar este cruce
# Problema similar: https://stackoverflow.com/questions/54426144/how-to-remove-na-from-facet-wrap-in-ggplot2
df_map <- df_avg %>% 
  filter(tipo %in% c("tmed","tmin","tmax")) %>%
  # filter(tipo %in% c("heating_degree")) %>%
  group_by(codigo_comuna, tipo) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  spread(tipo,valor) %>% 
  right_join(mapa_comuna) %>% 
  gather(tipo, valor, tmed, tmin, tmax)
  
# Chile
df_map %>% fig_mapa(valor, lwd=0.01, limites= c(-5,35),
                    titulo="Promedio 2016-2019 \n [°C]",
                    facets=~tipo,
                    fileName = sprintf(file_name,"MapaChileTemp"))
# Santiago
df_map %>%
  filter(mapa_rm==1) %>% 
  fig_mapa(valor, lwd=0.01, limites= c(-5,35),
                    titulo="Promedio 2016-2019 \n [°C]",
                    facets=~tipo,
                    fileName = sprintf(file_name,"MapaSantiagoTemp"))
# Zona Sur
df_map %>% filter(codigo_region %in% c("08","09","10","14","11")) %>%
  fig_mapa(valor, lwd=0.01, limites= c(-5,35),
           titulo="Promedio 2016-2019 \n [°C]",
           facets=~tipo,
           fileName = sprintf(file_name,"MapaSurTemp"))


## Analisis Heating degree ---------
df_meteo %>% filter(tipo=="heating_degree" & year(date)==2019) %>%
  ggplot(aes(date, valor, col=nombre_estacion))+
  geom_smooth(aes(col=nombre_estacion),se=F)+
  theme(legend.position = "none")+
  facet_wrap(~region)

# Mapas por Season
df_avg <- df_meteo %>% 
  mutate(year=year(date)) %>% 
  filter(year>2016) %>% 
  group_by(estacion,region,codigo_comuna, year,tipo, season) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% 
  group_by(estacion, region, codigo_comuna, tipo, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()

df_map <- df_avg %>% 
  filter(tipo %in% c("heating_degree")) %>%
  group_by(codigo_comuna, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  spread(season,valor) %>% 
  right_join(mapa_comuna) %>% 
  gather(season, valor,spring, summer, fall, winter)

# Chile
df_map %>% fig_mapa(valor, lwd=0.01,limites = c(0,20),
                    facets = ~season,
                    titulo="Heating Degree 2016-2019 \n [°C]",
                    fileName = sprintf(file_name,"MapaChileHeatDeg"))

# Santiago
df_map %>%
  filter(mapa_rm==1) %>% 
  fig_mapa(valor, lwd=0.01,limites=c(0, 20),
           facets = ~season,
           titulo="Heating Degree 2016-2019 \n [°C]",
           fileName = sprintf(file_name,"MapaSantiagoHeatDeg"))

# Zona Sur
df_map %>% filter(codigo_region %in% c("08","09","10","14","11")) %>% 
  fig_mapa(valor, lwd=0.01,limites=c(0, 20),
           facets = ~season,
           titulo="Heating Degree 2016-2019 \n [°C]",
           fileName = sprintf(file_name,"MapaSurHeatDeg"))


## Datos Comunales ----------
df_meteo <- read_rds("Data/Data_Modelo/Datos_Meteorologia.rsd") %>% 
  gather(tipo,valor, -codigo_comuna) %>% 
  left_join(mapa_comuna)

## Mapas chequeos
df_meteo$tipo %>% unique()

# Heat degree 15
df_meteo %>% 
  filter(tipo %>% str_detect("heating_degree_15")) %>%
  mutate(tipo=case_when(
    str_detect(tipo,"fall") ~ "Fall",
    str_detect(tipo,"winter") ~ "Winter",
    str_detect(tipo,"spring") ~ "Spring",
    str_detect(tipo,"summer") ~ "Summer") %>% 
      factor(levels = c("Fall","Winter","Spring","Summer"))) %>% 
  fig_mapa(valor, facets = ~tipo)


df_meteo %>% 
  filter(tipo %>% str_detect("heating_degree_15")) %>% 
  mutate(tipo=case_when(
    str_detect(tipo,"fall") ~ "Fall",
    str_detect(tipo,"winter") ~ "Winter",
    str_detect(tipo,"spring") ~ "Spring",
    str_detect(tipo,"summer") ~ "Summer") %>% 
      factor(levels = c("Fall","Winter","Spring","Summer"))) %>% 
  fig_mapaChile_facet(valor, facets=~tipo, titulo="HD a 15°C")


df_meteo %>% 
  filter(tipo %>% str_detect("heating_degree_15")) %>% 
  mutate(tipo=case_when(
    str_detect(tipo,"fall") ~ "Fall",
    str_detect(tipo,"winter") ~ "Winter",
    str_detect(tipo,"spring") ~ "Spring",
    str_detect(tipo,"summer") ~ "Summer") %>% 
      factor(levels = c("Fall","Winter","Spring","Summer"))) %>% 
  group_by(region,tipo) %>% summarise(valor=mean(valor)) %>%
  spread(tipo,valor) %>% view()


## EoF