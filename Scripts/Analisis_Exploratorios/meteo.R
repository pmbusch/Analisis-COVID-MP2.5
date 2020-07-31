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
df_map <- df_avg %>% 
  filter(tipo %in% c("tmed","tmin","tmax")) %>%
  # filter(tipo %in% c("heating_degree")) %>%
  group_by(codigo_comuna, tipo) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  right_join(mapa_comuna) %>% 
  filter(!is.na(tipo))

# Chile
df_map %>% fig_mapa(valor, lwd=0.01, limites= c(-5,35),
                    titulo="Promedio 2016-2019 \n [°C]")


ggplot(df_map) + 
  geom_sf(aes(fill = valor, geometry = geometry), lwd=0.01) +
  facet_grid(.~tipo)+
  scale_fill_viridis_c(name = "Promedio 2016-2019 \n [°C]", 
                       option="B", direction=-1, na.value = "white",
                       limits=c(-5,35)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)

ggsave(sprintf(file_name,"MapaChileTemp"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

# Santiago
# Remueve tmb Lo Barnechea (codigo 13115)
df_map %>% filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  ggplot() + 
  geom_sf(aes(fill = valor, geometry = geometry), lwd=0.01) +
  facet_grid(.~tipo)+
  scale_fill_viridis_c(name = "Promedio 2016-2019 \n [°C]", 
                       option="B", direction=-1, na.value = "white",
                       limits=c(-5,35)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)

ggsave(sprintf(file_name,"MapaSantiagoTemp"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

# Zona Sur
df_map %>% filter(codigo_region %in% c("08","09","10","14","11")) %>% 
  ggplot() + 
  geom_sf(aes(fill = valor, geometry = geometry), lwd=0.01) +
  facet_grid(.~tipo)+
  scale_fill_viridis_c(name = "Promedio 2016-2019 \n [°C]", 
                       option="B", direction=-1, na.value = "white",
                       limits=c(-5,35)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)

ggsave(sprintf(file_name,"MapaSurTemp"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")


## Analisis Heating degree ---------
df %>% filter(tipo=="heating_degree" & year(date)==2019) %>%
  ggplot(aes(date, valor, col=nombre_estacion))+
  geom_smooth(aes(col=nombre_estacion),se=F)+
  theme(legend.position = "none")+
  facet_wrap(~region)


## EoF