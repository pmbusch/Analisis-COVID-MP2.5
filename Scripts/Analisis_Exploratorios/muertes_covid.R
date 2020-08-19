### Analisis-COVID-MP2.5
## Muertes Covid
## PBH Julio 2020

theme_set(theme_bw())
file_name <- "Scripts/Analisis_Exploratorios/Figuras/COVID/%s.png"
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")

# Carga datos brutos --------
source("Scripts/Load_Data/covidMuertes_load.R", encoding = "UTF-8")

# Poblacion en comunas con muertes ---------
total_comunas <- nrow(df_muertes)
comunas_muerte <- df_muertes %>% filter(casos_fallecidos>0) %>% nrow()
cat(round(comunas_muerte/346*100,1),
    "% de comunas con muertes COVID")

rm(total_comunas, comunas_muerte)

## Mapas -------
df_muertes$tasa_mortalidad %>% range() # rango para los graficos
# Chile
df_muertes %>% left_join(mapa_comuna) %>% 
  fig_mapa(tasa_mortalidad, lwd=0.01,limites=c(0,250), 
           titulo="Tasa Mortalidad Covid \n [muertes/100mil hab]")
f_savePlot(last_plot(), 
           file_path = sprintf(file_name,"MapaChileCOVID"),dpi = 300)

# Chile Facet
df_muertes %>% left_join(mapa_comuna) %>% 
  fig_mapaChile_facet(tasa_mortalidad,limites=c(0,250),
                      titulo="Tasa Mortalidad Covid \n [muertes/100mil hab]")
f_savePlot(last_plot(), file_path = sprintf(file_name,"MapaChileCOVIDFacet"),dpi = 300)


# Santiago
df_muertes %>% left_join(mapa_comuna) %>% 
  left_join(codigos_territoriales) %>% 
  filter(mapa_rm==1) %>% 
  fig_mapa(tasa_mortalidad, limites = c(0,250),
           titulo= "Tasa Mortalidad Covid \n [muertes/100mil hab]")
f_savePlot(last_plot(), file_path = sprintf(file_name,"MapaSantiagoCOVID"), dpi=100)


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

## Heatmap -------
df_muertes_tiempo <- df_muertes %>% 
  left_join(df_poblacion) %>% 
  mutate(tasa_mortalidad=casos_fallecidos/poblacion*1e5,
         code=as.numeric(codigo_comuna),
         region=NULL) %>% 
  left_join(mapa_comuna, by=c("codigo_comuna")) %>% 
  filter(!is.na(region))

df_muertes_tiempo %>% 
  group_by(region,fecha) %>% 
  summarise(poblacion=sum(poblacion,na.rm=T),
            casos_fallecidos=sum(casos_fallecidos, na.rm=T)) %>% ungroup() %>% 
  mutate(tasa_mortalidad=casos_fallecidos/poblacion*1e5) %>% 
  ggplot(aes(x=fecha, y=reorder(region,desc(region)), fill=tasa_mortalidad))+
  geom_tile()+
  scale_fill_distiller(palette = "YlOrRd", type = 'seq', 
                       na.value = "white", direction = 1,
                       name="Tasa mortalidad COVID [por 100mil]")+
  scale_x_date(name="", date_breaks = "1 weeks",date_labels = "%d-%m")+
  scale_y_discrete(name=NULL)+
  coord_cartesian(expand = F)+
  theme(legend.position = "bottom")

## Muertes nuevas
df_muertes_tiempo <- df_muertes_tiempo %>% 
  arrange(comuna, fecha) %>% 
  group_by(comuna) %>% 
  mutate(fallecidos=casos_fallecidos-lag(casos_fallecidos,default = 0)) %>% 
  ungroup()

df_muertes_tiempo %>% 
  group_by(region,fecha) %>% 
  summarise(poblacion=sum(poblacion,na.rm=T),
            fallecidos=sum(fallecidos, na.rm=T)) %>% ungroup() %>% 
  mutate(tasa_mortalidad=fallecidos/poblacion*1e5) %>% 
  ggplot(aes(x=fecha, y=reorder(region,desc(region)), fill=tasa_mortalidad))+
  geom_tile()+
  scale_fill_distiller(palette = "YlOrRd", type = 'seq', 
                       na.value = "white", direction = 1,
                       name="Casos mortalidad nuevos [por 100mil]")+
  scale_x_date(name="", date_breaks = "1 weeks",date_labels = "%d-%m")+
  scale_y_discrete(name=NULL)+
  coord_cartesian(expand = F)+
  theme(legend.position = "bottom")
f_savePlot(last_plot(), sprintf(file_name,"nuevasMuertes",dpi=300))


# Idem contagios nuevos -----------
df_casos <- read_csv(paste(url,"producto1","Covid-19_std.csv", sep="/"))
names(df_casos) <- names(df_casos) %>% str_to_lower() %>% str_replace_all(" ","_")
df_casos <- df_casos %>% na.omit() # limpio NA

df_casos_tiempo <- df_casos %>% 
  left_join(df_poblacion) %>% 
  mutate(tasa=casos_confirmados/poblacion*1e5,
         code=as.numeric(codigo_comuna),
         region=NULL) %>% 
  left_join(mapa_comuna, by=c("codigo_comuna")) %>% 
  filter(!is.na(region))

df_casos_tiempo <- df_casos_tiempo %>% 
  arrange(comuna, fecha) %>% 
  group_by(comuna) %>% 
  mutate(contagios=casos_confirmados-lag(casos_confirmados,default = 0)) %>% 
  ungroup()

# Por region
df_casos_tiempo %>% 
  group_by(region,fecha) %>% 
  summarise(poblacion=sum(poblacion,na.rm=T),
            contagios=sum(contagios, na.rm=T)) %>% ungroup() %>% 
  mutate(tasa=contagios/poblacion*1e5) %>% 
  ggplot(aes(x=fecha, y=reorder(region,desc(region)), fill=tasa))+
  geom_tile()+
  scale_fill_distiller(palette = "YlOrRd", type = 'seq', 
                       na.value = "white", direction = 1,
                       name="Nuevos contagios [por 100mil]")+
  scale_x_date(name="", date_breaks = "1 weeks",date_labels = "%d-%m")+
  scale_y_discrete(name=NULL)+
  coord_cartesian(expand = F)+
  theme(legend.position = "bottom")

# Por comuna
df_casos_tiempo %>% 
  group_by(comuna,fecha,region) %>% 
  summarise(poblacion=sum(poblacion,na.rm=T),
            contagios=sum(contagios, na.rm=T)) %>% ungroup() %>% 
  mutate(tasa=contagios/poblacion*1e5) %>% 
  ggplot(aes(x=fecha, y=reorder(comuna,desc(comuna)), fill=tasa))+
  geom_tile()+
  facet_grid(region~., scales = "free", space="free")+
  scale_fill_distiller(palette = "YlOrRd", type = 'seq', 
                       na.value = "white", direction = 1,
                       trans="sqrt",
                       name="Casos mortalidad nuevos [por 100mil]")+
  scale_x_date(name="", date_breaks = "1 weeks",date_labels = "%d-%m")+
  scale_y_discrete(name=NULL)+
  coord_cartesian(expand = F)+
  theme(legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size=4))
f_savePlot(last_plot(), sprintf(file_name,"nuevosContagios",dpi=900))


## DEIS ----------
df_deis <- df_deis %>% select(-region) %>% left_join(codigos_territoriales) %>% 
  left_join(mapa_regiones %>% select(-geometry))

## SANKEY Diagram
library(ggforce)
data <- df_deis %>% group_by(sexo, grupo_edad, tipo,region) %>% 
  summarise(value=n()) %>% arrange(desc(value))

## Function para preparar datos para la funcion
data <- gather_set_data(data, 1:4)
data %>% names()
# Orden
data$x <- factor(data$x, levels = c("tipo", "sexo", "grupo_edad","region"))

ggplot(data, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = tipo), alpha = 0.3, axis.width = 0.1, sep=0.01) +
  geom_parallel_sets_axes(axis.width = 0.1, fill = "grey80", color = "grey80",sep=0.01) +
  geom_parallel_sets_labels(color = 'black', size = 14/.pt, angle = 90,sep=0.01) +
  scale_x_discrete(name = NULL, expand = c(0, 0.2), 
                   labels=c("Tipo","Sexo"," Grupo etario","Region"))+
  scale_y_continuous(breaks = NULL, expand = c(0, 0))+
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        text = element_text(size=20),
        legend.position = "none", plot.margin = margin(14, 1.5, 2, 1.5))

f_savePlot(last_plot(), 
           sprintf(file_name,"SankeyMuertesCovid"), dpi=100)

## EoF