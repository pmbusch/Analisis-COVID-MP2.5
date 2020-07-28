### Analisis-COVID-MP2.5
## Muertes Covid
## PBH Julio 2020

theme_set(theme_bw())


source("Scripts/Load_Data/covidMuertes_load.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")


## Mapa Chile -------
df_muertes %>% left_join(mapa_comuna) %>% 
  ggplot() + 
  geom_sf(aes(fill = tasa_mortalidad, geometry = geometry)) +
  scale_fill_viridis_c(name = "Tasa Mortalidad Covid \n [muertes/100mil hab]", 
                       option="B", direction=-1, na.value = "white", 
                       limits=c(0,200)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)

ggsave("Figuras/MapaChileCOVID.png", 
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

# Santiago
# Remueve tmb Lo Barnechea (codigo 13115)
df_muertes %>% left_join(mapa_comuna) %>% 
  left_join(codigos_territoriales) %>% 
  filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  ggplot() + 
  geom_sf(aes(fill = tasa_mortalidad, geometry = geometry)) +
  # geom_sf_label(aes(label=nombre_comuna, geometry=geometry))+
  scale_fill_viridis_c(name = "Tasa Mortalidad Covid \n [muertes/100mil hab]", 
                       option="B", direction=-1, na.value = "white",
                       limits=c(0,200)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)
  # theme(legend.position = "none")

ggsave("Figuras/MapaSantiagoCOVID.png", 
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

# Zona Sur
df_muertes %>% left_join(mapa_comuna) %>% 
  filter(codigo_region %in% c("08","09","10","14","11")) %>% 
  ggplot() + 
  geom_sf(aes(fill = tasa_mortalidad, geometry = geometry)) +
  scale_fill_viridis_c(name = "Tasa Mortalidad Covid \n [muertes/100mil hab]", 
                       option="B", direction=-1, na.value = "white", 
                       limits=c(0,200)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)

ggsave("Figuras/MapaSurCOVID.png", 
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")
  

## Geo Facet Santiago ---------------
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados
df_muertes <- read_csv(paste(url,"producto38","CasosFallecidosPorComuna_std.csv", sep="/"))
names(df_muertes) <- names(df_muertes) %>% str_to_lower() %>% str_replace_all(" ","_")
df_muertes <- df_muertes %>% na.omit() # limpio NA

library(geofacet)

df_muertes_tiempo <- df_muertes %>% 
  mutate(code=as.numeric(codigo_comuna)) %>% 
  right_join(cl_santiago_prov_grid1, by=c("code"))
  

df_muertes_tiempo %>%  
  ggplot(aes(x=fecha, y=casos_fallecidos))+
  geom_line()+
  facet_geo(~ name, grid="cl_santiago_prov_grid1")+
  labs(x="", y="")+
  ggtitle("Total Muertes acumuladas")+theme(plot.title = element_text(hjust = 0.5))

ggsave("Figuras/MuertesSantiago.png", 
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

## EoF