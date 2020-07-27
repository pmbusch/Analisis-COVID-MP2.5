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
                       option="B", direction=-1, na.value = "white") +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 13)

ggsave("Figuras/MapaChileCOVID.png", 
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

# Santiago
df_muertes %>% left_join(mapa_comuna) %>% 
  left_join(codigos_territoriales) %>% 
  filter(codigo_provincia=="131") %>% 
  ggplot() + 
  geom_sf(aes(fill = tasa_mortalidad, geometry = geometry)) +
  # geom_sf_label(aes(label=nombre_comuna, geometry=geometry))+
  scale_fill_viridis_c(name = "Tasa Mortalidad Covid \n [muertes/100mil hab]", 
                       option="B", direction=-1, na.value = "white") +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 13)

ggsave("Figuras/MapaSantiagoCOVID.png", 
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")


## Geo Facet Santiago ---------------
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados
df_muertes <- read_csv(paste(url,"producto38","CasosFallecidosPorComuna_std.csv", sep="/"))
names(df_muertes) <- names(df_muertes) %>% str_to_lower() %>% str_replace_all(" ","_")
df_muertes <- df_muertes %>% na.omit() # limpio NA


df_muertes_tiempo <- df_muertes %>% 
  mutate(code=as.numeric(codigo_comuna)) %>% 
  right_join(cl_santiago_prov_grid1, by=c("code"))
  
library(geofacet)

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