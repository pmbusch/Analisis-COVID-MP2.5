### Analisis-COVID-MP2.5
## SINCA: Datos concentracion
## PBH Julio 2020

theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_Exploratorios/Figuras/SINCA/%s.png"
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")


# Carga datos brutos y Mapa --------
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")

df_conc <- df_conc %>% mutate(region=factor(region, levels_region)) %>% 
  filter(pollutant=="mp2.5" & year %in% 2010:2019)

# Numero estaciones con datos
df_conc$site %>% unique() %>% length()
# Numero comunas con datos
df_conc$codigo_comuna %>% unique() %>% length()


## Series de Tiempo -------

# Heatmap Estaciones
df_mes <- df_conc %>% 
  group_by(year,month,site,latitud,codigo_comuna) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% ungroup() %>% 
  mutate(date=paste(year,month,1,sep="-") %>%  
           strptime(format="%Y-%m-%d") %>% 
           as_date()) %>% 
  left_join(mapa_comuna)

df_mes %>% 
  ggplot(aes(x = date, y = reorder(site, latitud), fill = valor)) + 
  geom_tile() + 
  facet_grid(region~., scales = "free", space="free")+
  scale_fill_distiller(palette = "YlOrRd", type = 'seq', 
                       na.value = "white", direction = 1,
                       name="Promedio MP2.5 [ug/m3]",
                       trans="sqrt") + 
  scale_y_discrete(name = NULL)+
  scale_x_date(name="", date_breaks = "2 years",date_labels = "%Y")+
  coord_cartesian(expand=F)
f_savePlot(last_plot(), 
           sprintf(file_name,"HeatMap_Site",dpi=600))


# Heatmap Comunas
df_mes <- df_mes %>% left_join(codigos_territoriales) %>% 
  group_by(date, nombre_comuna, region) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% ungroup()

df_mes %>% 
  ggplot(aes(x = date, y = reorder(nombre_comuna, valor), fill = valor)) + 
  geom_tile() + 
  facet_grid(region~., scales = "free", space="free")+
  scale_fill_distiller(palette = "YlOrRd", type = 'seq', 
                       na.value = "white", direction = 1,
                       name="Promedio MP2.5 [ug/m3]",
                       trans="sqrt") + 
  scale_y_discrete(name = NULL)+
  scale_x_date(name="", date_breaks = "2 years",date_labels = "%Y")+
  coord_cartesian(expand=F)
f_savePlot(last_plot(), 
           sprintf(file_name,"HeatMap_Comuna",dpi=300))

rm(df_mes)

## Barras promedio ordenadas Norte-Sur ---------
# Estacion
df_avg <- df_conc %>% 
  group_by(site,region,codigo_comuna, year) %>% 
  summarise(valor=mean(valor,na.rm=T),
            disponibilidad=n()/365) %>% ungroup() %>% 
  filter(disponibilidad>0.8) %>%
  group_by(site, region, codigo_comuna) %>% 
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
  scale_y_continuous(name="Promedio 2010-2019 MP2.5 [ug/m3]",
                     expand = c(0, 0),
                     labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

f_savePlot(last_plot(), sprintf(file_name,"Barras_Site"), dpi=600)

# Comuna
df_avg <- df_avg %>% 
  group_by(codigo_comuna, region) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% ungroup() %>% 
  left_join(codigos_territoriales)

# Numero comunas
df_avg$codigo_comuna %>% unique() %>% length()

  
df_avg %>% 
  mutate(highlight=if_else(valor>20,"yes","no")) %>%
  ggplot(aes(x=reorder(nombre_comuna, valor), y=valor, fill=highlight)) +
  geom_col()+
  geom_hline(yintercept = 20, col="red", linetype = "dashed", size=1)+
  facet_grid(region~., scales = "free", space="free")+
  coord_flip(clip="off")+
  scale_fill_manual(values = c("#B0B0B0D0", "#BD3828D0"), guide = "none")+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name="Promedio 2010-2019 MP2.5 [ug/m3]",
                     expand = c(0, 0),
                     labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

f_savePlot(last_plot(), sprintf(file_name,"Barras_Comuna"), dpi=150)


## Scatter Norte Sur -----------
df_com <- df_avg %>% 
  left_join(mapa_comuna)

fig_scatterComuna(df_com, valor, limites = c(0,50),
                  titulo="Promedio 2010-2019 MP2.5 [ug/m3]")


## Promedio 2016-2019: Mapa Comunas -----------
# Agregar codigos comunales
df_map <- df_avg %>% 
  right_join(mapa_comuna)

# Chile
fig_mapa(df_map, valor, lwd=0.01,
         limites=c(0,50), titulo="Promedio 2010-2019 \n MP2.5 [ug/m3]",
         fileName=sprintf(file_name,"MapaChileMP25"))

# Chile Facet
fig_mapaChile_facet(df_map, valor, limites=c(0,50),
                    titulo = "Promedio 2010-2019 \n MP2.5 [ug/m3]")
f_savePlot(last_plot(),
           file_path =sprintf(file_name,"MapaChileMP25Facet"),dpi=300)

# Santiago
df_map %>% 
  filter(mapa_rm==1) %>% 
  fig_mapa(valor,limites = c(0,50), titulo="Promedio 2010-2019 \n MP2.5 [ug/m3]")+
  geom_sf_label(aes(label=nombre_comuna, geometry=geometry))
f_savePlot(last_plot(), sprintf(file_name,"MapaSantiagoMP25"))
  

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

f_savePlot(last_plot(), sprintf(file_name,"MapaEstacionesAire"))

## Expansion concentracion FIXED RADIUS ------

## Cargar datos distancia
df_dist <- read_rds("Data/Data_Modelo/distanciaComunaEstacionSinca.rsd")

## Datos de monitoreo
df_avg <- df_conc %>% 
  group_by(site,region,codigo_comuna, year) %>% 
  summarise(valor=mean(valor,na.rm=T),
            disponibilidad=n()/365) %>% ungroup() %>% 
  filter(disponibilidad>0.8) %>%
  group_by(site, region,codigo_comuna) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()

# Cruzo con estaciones con comunas, en base a distnacia
corte_km <- 20
# corte_km <- Inf
df_dist <- df_dist %>% filter(dist<corte_km*1e3)
# Join
df_avg <- df_dist %>% left_join(df_avg, by=c("site","codigo_comuna"))

## Promedio ponderado por inverso de la distancia
df_avg <- df_avg %>% 
  group_by(codigo_comuna, nombre_comuna) %>% 
  summarise(avg=weighted.mean(avg, 1/(dist))) %>% ungroup() %>% 
  right_join(mapa_comuna)

## Mapas
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")

# Chile
df_avg %>% 
  fig_mapa(avg, limites = c(0,50), lwd=0.01,
           titulo="Promedio 2010-2019 \n MP2.5 [ug/m3]")
f_savePlot(last_plot(),file_path=sprintf(file_name,"MapaChile"))

## Chile Facet
fig_mapaChile_facet(df_avg, avg, limites=c(0,50),
                    titulo = "Promedio 2010-2019 \n MP2.5 [ug/m3]")
f_savePlot(last_plot(), file_path = sprintf(file_name,"MapaChileFacet"))

# Santiago
df_avg %>% 
  filter(mapa_rm==1) %>% 
  fig_mapa(avg, limites = c(0,50), titulo="Promedio 2010-2019 \n MP2.5 [ug/m3]")
f_savePlot(last_plot(),file_path = sprintf(file_name,"MapaSantiago"))
           

# Poblacion en comunas con datos MP2.5
total_pob <- df_poblacion$poblacion %>% sum()
pob_mp25 <- df_avg %>% left_join(df_poblacion) %>% pull(poblacion) %>% sum(na.rm=T)
cat(round(pob_mp25/total_pob*100,1),
    "% de poblacion en comunas con monitoreo de MP2.5")

# ## Guardo Datos de MP2.5 expandidos a nivel comunal
# df_avg %>% rename(mp25=avg) %>% select(codigo_comuna,mp25) %>% 
#   saveRDS("Data/Data_Modelo/Datos_Concentraciones_20km.rsd")

## EoF