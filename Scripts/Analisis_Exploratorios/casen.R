### Analisis-COVID-MP2.5
## CASEN: Analisis datos
## PBH Julio 2020

theme_set(theme_bw())
file_name <- "Scripts/Analisis_Exploratorios/Figuras/casen/%s.png"

# Carga datos brutos y Mapa --------
source("Scripts/Load_Data/casen_load.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")

## Mapa Mediana Ingresos Chile --------
df_ingreso %>% 
  right_join(mapa_comuna) %>% 
  ggplot()+
  geom_sf(aes(fill = ingresoAutonomo_mediana, geometry = geometry), lwd=0.01) +
  scale_fill_viridis_c(name = "Mediana Ingreso autonomo [CLP]", 
                       option="B", direction=-1, na.value = "white",
                       limits=c(0,1.2*1e6),
                       labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)

ggsave(sprintf(file_name,"MapaChileIngresos"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")


# Santiago
# Remueve tmb Lo Barnechea (codigo 13115)
df_ingreso %>% 
  right_join(mapa_comuna) %>% 
  filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  ggplot() + 
  geom_sf(aes(fill = ingresoAutonomo_mediana, geometry = geometry), lwd=0.5) +
  scale_fill_viridis_c(name = "Mediana Ingreso autonomo [CLP]", 
                       option="B", direction=-1, na.value = "white",
                       limits=c(0,1.2*1e6),
                       labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)

ggsave(sprintf(file_name,"MapaSantiagoIngresos"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

## EoF