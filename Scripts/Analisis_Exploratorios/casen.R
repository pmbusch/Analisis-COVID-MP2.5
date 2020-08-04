### Analisis-COVID-MP2.5
## CASEN: Analisis datos
## PBH Julio 2020

theme_set(theme_bw())
file_name <- "Scripts/Analisis_Exploratorios/Figuras/casen/%s.png"
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")

# Carga datos brutos y Mapa --------
source("Scripts/Load_Data/casen_load.R", encoding = "UTF-8") 


## Mapa Mediana Ingresos --------
# Chile
df_ingreso %>% 
  right_join(mapa_comuna) %>% 
  fig_mapa(ingresoAutonomo_mediana, lwd=0.01, limites=c(0,1.2*1e6),
           titulo="Mediana Ingreso autonomo [CLP]")
f_savePlot(last_plot(), sprintf(file_name,"MapaChileIngresos"))

# Chile Facet
df_ingreso %>% 
  right_join(mapa_comuna) %>% 
  fig_mapaChile_facet(ingresoAutonomo_mediana, limites=c(0,1.2*1e6),
           titulo="Mediana Ingreso autonomo [CLP]")
f_savePlot(last_plot(), sprintf(file_name,"MapaChileIngresosFacet"))


# Santiago
df_ingreso %>% 
  right_join(mapa_comuna) %>% 
  filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  fig_mapa(ingresoAutonomo_mediana, lwd=0.01, limites=c(0,1.2*1e6),
           titulo="Mediana Ingreso autonomo [CLP]",
           fileName = sprintf(file_name,"MapaSantiagoIngresos"))

## Scatter -----------
df_ingreso %>%
  left_join(codigos_territoriales) %>% 
  left_join(mapa_comuna) %>% 
  fig_scatterComuna(ingresoAutonomo_mediana, limites = c(0,1.2*1e6),
                  titulo="Mediana Ingreso autonomo [CLP]")

## Distribucion
df_prevision %>% names()

df <- df_prevision %>% 
  filter(s12!=99) %>% # filtro respuesta no sabe
  mutate(prev=case_when(
    s12==1 ~ "Fonasa-A",
    s12==2 ~ "Fonasa-B",
    s12==3 ~ "Fonasa-C",
    s12==4 ~ "Fonasa-D",
    s12==6 ~ "FF.AA.",
    s12==7 ~ "Isapre",
    T~"Otro") %>% factor(levels=c("Fonasa-A","Fonasa-B","Fonasa-C","Fonasa-D",
                                "Isapre","FF.AA.","Otro"))) %>% 
  group_by(codigo_comuna,prev) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% select(-hab) %>% 
  left_join(codigos_territoriales) %>% 
  left_join(mapa_comuna)

df %>%
  mutate(cent_lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  ggplot(aes(x=reorder(codigo_comuna, cent_lat),y=perc))+
  geom_col(aes(fill=prev),width=1)+
  facet_grid(region~., scales = "free", space="free")+
  coord_flip(expand = F)+
  labs(x="Comunas",y="Porcentaje", fill="Prevision")+
  scale_y_continuous(labels = scales::percent, limits=c(0,1))+
  scale_fill_viridis_d(guide = guide_legend(byrow=T,reverse = T),
                       direction = -1)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")

f_savePlot(last_plot(), sprintf(file_name, "previsionComuna"))

## EoF