### Analisis-COVID-MP2.5
## Muertes Covid
## PBH Julio 2020

theme_set(theme_bw())
file_name <- "Scripts/Analisis_Exploratorios/Figuras/COVID/%s.png"
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")

# Carga datos brutos --------
source("Scripts/Load_Data/covidMuertes_load.R", encoding = "UTF-8")


## Mapas -------
# Chile
df_muertes %>% left_join(mapa_comuna) %>% 
  fig_mapa(tasa_mortalidad, lwd=0.01,limites=c(0,200), 
           titulo="Tasa Mortalidad Covid \n [muertes/100mil hab]",
           fileName = sprintf(file_name,"MapaChileCOVID"))
# Santiago: Remueve Lo Barnechea (codigo 13115)
df_muertes %>% left_join(mapa_comuna) %>% 
  left_join(codigos_territoriales) %>% 
  filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  fig_mapa(tasa_mortalidad, limites = c(0,200),
           titulo= "Tasa Mortalidad Covid \n [muertes/100mil hab]",
           fileName = sprintf(file_name,"MapaSantiagoCOVID"))
# Zona Sur
df_muertes %>% left_join(mapa_comuna) %>% 
  filter(codigo_region %in% c("08","09","10","14","11")) %>% 
  fig_mapa(tasa_mortalidad, limites = c(0,200),
           titulo= "Tasa Mortalidad Covid \n [muertes/100mil hab]",
           fileName = sprintf(file_name,"MapaSurCOVID"))
  

## Geo Facet Santiago ---------------
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados
df_muertes <- read_csv(paste(url,"producto38","CasosFallecidosPorComuna_std.csv", sep="/"))
names(df_muertes) <- names(df_muertes) %>% str_to_lower() %>% str_replace_all(" ","_")
df_muertes <- df_muertes %>% na.omit() # limpio NA

library(geofacet)

df_muertes_tiempo <- df_muertes %>% 
  left_join(df_poblacion) %>% 
  mutate(tasa_mortalidad=casos_fallecidos/poblacion*1e5,
         code=as.numeric(codigo_comuna)) %>% 
  right_join(cl_santiago_prov_grid1, by=c("code"))
  

df_muertes_tiempo %>%  
  ggplot(aes(x=fecha, y=tasa_mortalidad))+
  geom_line()+
  facet_geo(~ name, grid="cl_santiago_prov_grid1")+
  labs(x="", y="")+
  ggtitle("Tasa mortalidad COVID [por 100mil]")+theme(plot.title = element_text(hjust = 0.5))

ggsave(sprintf(file_name,"MuertesSantiago"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

## EoF