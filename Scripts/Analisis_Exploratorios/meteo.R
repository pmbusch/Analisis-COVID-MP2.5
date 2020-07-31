### Analisis-COVID-MP2.5
## Meteorologia
## PBH Julio 2020

theme_set(theme_bw())
file_name <- "Scripts/Analisis_Exploratorios/Figuras/meteo/%s.png"
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")

# Carga datos brutos y Mapa --------
df_meteo <- read_rds("Data/Data_Modelo/Datos_Meteorologia_raw.rsd")


# Agregar codigos comunales ---------
df_meteo <- df_meteo %>% 
  mutate(nombre_comuna=comuna %>% 
           str_replace_all("Viña del Mar","Vina del Mar") %>% 
           str_replace_all("Purrangue","Purranque") %>% 
           str_replace_all("San Bernando","San Bernardo") %>% 
           str_replace_all("Cabo de Hornos \\(Ex-Navarino\\)","Cabo de Hornos") %>% 
           str_replace_all("de Zapallar, V region","Zapallar") %>% 
           str_replace_all("de Retiro, VII region","Retiro")) %>%
  left_join(codigos_territoriales %>% select(codigo_comuna, nombre_comuna),
            by=c("nombre_comuna"))


# Promedio 2016-2019
df_avg <- df_meteo %>% 
  mutate(year=year(date)) %>% 
  filter(year>2016) %>% 
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
df_map %>% filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
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


## EoF