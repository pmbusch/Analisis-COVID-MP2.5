### Analisis-COVID-MP2.5
## Figuras exploracion datos
## Funciones de visualizacion fijas
## PBH Julio 2020

theme_set(theme_bw())
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
source("Scripts/00-Funciones.R", encoding = "UTF-8")
library(patchwork)

## MAPAS ## ---------
# Funcion para graficar mediante colores una variable y guardarla
# Utiliza como base un df unido a la variable mapa_comuna
# Se puede filtar antes el df para graficar una zona geografica exclusiva
# Recibe dataframe, valor a graficar, facets, escala, titulo leyenda, guardar
fig_mapa <- function(df, val, facets=NULL, limites=NULL, 
                     titulo="", fileName=NULL, lwd=0.5){
  p <- ggplot(df) +
    geom_sf(aes(fill = {{val}}, geometry = geometry), lwd=lwd) +
    facet_grid({{facets}})+
    scale_fill_viridis_c(name = titulo, 
                         option="plasma", direction=1, na.value = "white",
                         limits=limites,
                         labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F)) +
    labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
    theme_minimal(base_size = 14)
  # Guardo
  if (!is.null(fileName)){
    f_savePlot(p,fileName)
  }
  p
}

## Figura con zoom a partes de Chile de mayor interes
# Considera 4 zonas: Norte, Centro, Sur y Austral
# Utiliza como base la funcion de fig_mapa
fig_mapaChile_facet <- function(df, val, limites=NULL, 
                     titulo="", fileName=NULL){
  
  # Creo las zonas, en base a las regiones
  df <- df %>% mutate(zona_facet=case_when(
    codigo_region %in% c("15","01","02","03","04") ~ "Norte",
    codigo_region %in% c("05","13","06","07") ~ "Centro",
    codigo_region %in% c("08","16","09","14") ~ "Sur",
    codigo_region %in% c("10","11","12") ~ "Austral") %>% factor())
  
  # Creo los graficos, con coordenadas fijas para cada zona (mejora la estetica)
  p1 <- df %>% filter(zona_facet=="Norte") %>% 
    fig_mapa({{val}}, limites = limites, lwd=0.01,titulo=titulo)+
    labs(title="Norte",subtitle = "Arica a Coquimbo")+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-72, -66.5), ylim = c(-32.5, -17.5),datum = NA,expand=F)
  p2 <- df %>% filter(zona_facet=="Centro") %>% 
    fig_mapa({{val}}, limites = limites, lwd=0.01,titulo=titulo)+
    labs(title="Centro",subtitle = "RM-V a Talca")+
    coord_sf(xlim = c(-73, -69.5), ylim = c(-37, -32),datum = NA, expand=F)
  p3 <- df %>% filter(zona_facet=="Sur") %>% 
    fig_mapa({{val}}, limites = limites, lwd=0.01,titulo=titulo)+
    labs(title="Sur",subtitle = "Biobio a Los Rios")+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-74, -70.5), ylim = c(-41, -36),datum = NA,expand=F)
  p4 <- df %>% filter(zona_facet=="Austral") %>% 
    fig_mapa({{val}}, limites = limites, lwd=0.01,titulo=titulo)+
    labs(title="Austral",subtitle = "Pto Montt a Punta Arenas")+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-76, -66), ylim = c(-56, -40),datum = NA,expand=F)
  
  # Nota: alineacion de la leyenda fue dificil, solucion heuristica actual es buena
  p <- (p1|p2)/(p3|p4)+plot_layout(guide="auto")&
    theme(plot.subtitle = element_text(size=8))
  
  
  
  # Guardo
  if (!is.null(fileName)){
    f_savePlot(p,fileName)
  }
  p
}

## SCATTER COMUNA -----------
# Funcion para graficar en puntos valores de comuna
# Se ordena segun geografia de las regiones y latitud de las comunas
# Df debe estar con join a codigos territoriales y mapa comuna
# Recibe dataframe, valor a graficar, facets, escala, titulo leyenda, guardar
fig_scatterComuna <- function(df, val, col_aes=NULL, limites=NULL,
                              titulo=""){
  df %>% 
    mutate(cent_lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
    ggplot(aes(x=reorder(codigo_comuna, cent_lat), y={{val}}, col={{col_aes}}))+
    geom_point()+
    facet_grid(region~., scales = "free", space="free")+
    labs(x="Comunas",y=titulo)+
    scale_y_continuous(limits = limites,
                       labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    scale_color_viridis_d()+
    coord_flip()+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.y = element_blank())
}



## BARRAS REGION-COMUNA --------
# Funcion para graficar en barras algun valor promedio
# Se ordena segun geografia de las regiones, y se muestran las comunas de Chile
# Df debe estar con join a codigos territoriales y mapa regiones (levels region)
# Se recomienda filtrar antes por las comunas con MP, para evitar un grafico gigante
# Recibe dataframe, valor a graficar, facets, escala, titulo leyenda, guardar
fig_barrasRegion <- function(df, val, fill_col=NULL, limites=NULL,
                             titulo=""){
  df %>% 
    ggplot(aes(x=reorder(nombre_comuna, {{val}}), y={{val}}, fill={{fill_col}})) +
    geom_col()+
    # geom_hline(yintercept = val_highlight, col="red", linetype = "dashed", size=1)+
    facet_grid(region~., scales = "free", space="free")+
    coord_flip(clip="off")+
    scale_fill_viridis_d()+
    # scale_fill_manual(values = c("#B0B0B0D0", "#BD3828D0"), guide = "none")+
    scale_x_discrete(name = NULL)+
    scale_y_continuous(name=titulo,
                       expand = c(0, 0),
                       limits=limites,
                       labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank())
}

# df_prueba %>% mutate(highlight=if_else(valor>20,"yes","no")) %>%
#   fig_barrasRegion(valor, fill_col = highlight)
# 
# fig_barrasRegion(df_prueba, valor)

# df_prueba <- df_avg %>% 
#   group_by(comuna) %>% 
#   summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
#   mutate(nombre_comuna=f_remover_acentos(comuna) %>% 
#            str_replace_all("Aysen","Aisen") %>% 
#            str_replace_all("Coyhaique","Coihaique")) %>% 
#   left_join(codigos_territoriales,by=c("nombre_comuna")) %>% 
#   left_join(mapa_regiones)



## EoF