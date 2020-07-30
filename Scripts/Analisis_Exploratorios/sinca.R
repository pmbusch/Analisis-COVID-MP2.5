### Analisis-COVID-MP2.5
## SINCA: Datos concentracion
## PBH Julio 2020

theme_set(theme_bw())
file_name <- "Scripts/Analisis_Exploratorios/Figuras/SINCA/%s.png"

# Carga datos brutos y Mapa --------
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
source("Scripts/00-Funciones.R", encoding = "UTF-8")

df_conc <- df_conc %>% mutate(region=factor(region, levels_region)) %>% 
  filter(pollutant=="mp2.5")

## Promedio 2016-2019: Norte a Sur -----------
df_avg <- df_conc %>% 
  filter(year>2016) %>% 
  group_by(site,region,comuna, year) %>% 
  summarise(valor=mean(valor,na.rm=T),
            disponibilidad=n()/365) %>% 
  filter(disponibilidad>0.8) %>% ungroup() %>% 
  group_by(site, region, comuna) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()

df_avg %>% 
  mutate(highlight=if_else(valor>20,"yes","no")) %>%
  ggplot(aes(x=reorder(site, valor), y=valor, fill=highlight)) +
  geom_col()+
  geom_hline(yintercept = 20, col="red", linetype = "dashed", size=1)+
  facet_grid(region~., scales = "free", space="free")+
  coord_flip(clip="off")+
  scale_fill_manual(values = c("#B0B0B0D0", "#BD3828D0"), guide = "none")+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name="Promedio 2016-2019 MP2.5 [ug/m3]",
                     expand = c(0, 0),
                     labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave(sprintf(file_name,"MP25ChileRegion"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

## Promedio 2016-2019: Mapa Comunas -----------
# Agregar codigos comunales
df_map <- df_avg %>% 
  group_by(comuna) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  mutate(nombre_comuna=f_remover_acentos(comuna) %>% 
           str_replace_all("Aysen","Aisen") %>% 
           str_replace_all("Coyhaique","Coihaique")) %>% 
  left_join(codigos_territoriales,by=c("nombre_comuna")) %>% 
  right_join(mapa_comuna)

ggplot(df_map) + 
  geom_sf(aes(fill = valor, geometry = geometry), lwd=0.01) +
  scale_fill_viridis_c(name = "Promedio 2016-2019 \n MP2.5 [ug/m3]", 
                       option="B", direction=-1, na.value = "white",
                       limits=c(0,50)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)

ggsave(sprintf(file_name,"MapaChileMP25"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

## Mapa solo Santiago
# Remueve tmb Lo Barnechea (codigo 13115)
df_map %>% filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  ggplot() + 
  geom_sf(aes(fill = valor, geometry = geometry), lwd=0.5) +
  geom_sf_label(aes(label=nombre_comuna, geometry=geometry))+
  scale_fill_viridis_c(name = "Promedio 2016-2019 \n MP2.5 [ug/m3]", 
                       option="B", direction=-1, na.value = "white",
                       limits=c(0,50)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)
  # theme(legend.position = "none")

ggsave(sprintf(file_name,"MapaSantiagoMP25"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

## Mapa Zona Sur
df_map %>% filter(codigo_region %in% c("08","09","10","14","11")) %>% 
  ggplot() + 
  geom_sf(aes(fill = valor, geometry = geometry), lwd=0.5) +
  scale_fill_viridis_c(name = "Promedio 2016-2019 \n MP2.5 [ug/m3]", 
                       option="B", direction=-1, na.value = "white",
                       limits=c(0,50)) +
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 8)

ggsave(sprintf(file_name,"MapaSurMP25"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

### Ubicacion Estaciones -----------
df_estaciones <- df_conc %>% 
  filter(pollutant=="mp2.5") %>% 
  group_by(site, longitud, latitud) %>% 
  summarise(avg=mean(valor, na.rm=T)) %>% na.omit()
  

ggplot(df_estaciones)+
  geom_sf(data=mapa_regiones,aes(geometry=geometry),fill="white", lwd=0.01)+
  geom_point(aes(longitud, latitud), shape=1, col="red")+
  # geom_text_repel(aes(longitud, latitud, label=site))+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = T)+
  theme_minimal(base_size = 13)

ggsave(sprintf(file_name,"MapaEstacionesAire"),
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

## EoF