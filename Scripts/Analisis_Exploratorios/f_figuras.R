### Analisis-COVID-MP2.5
## Figuras exploracion datos
## Funciones de visualizacion fijas
## PBH Julio 2020

theme_set(theme_bw())
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
source("Scripts/00-Funciones.R", encoding = "UTF-8")


## Save Plot ----------
f_savePlot <- function(p1, file_path){
  cat("Saving: ",file_path)
  ggsave(file_path, {{p1}},dpi=600,
         width = 14.87, height = 9.30, units = "in")
}


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
                         option="B", direction=-1, na.value = "white",
                         limits=limites,
                         labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F)) +
    labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
    theme_minimal(base_size = 8)
  # Guardo
  if (!is.null(fileName)){
    f_savePlot(p,fileName)
  }
  p
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

df_prueba %>% mutate(highlight=if_else(valor>20,"yes","no")) %>%
  fig_barrasRegion(valor, fill_col = highlight)

fig_barrasRegion(df_prueba, valor)

# df_prueba <- df_avg %>% 
#   group_by(comuna) %>% 
#   summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
#   mutate(nombre_comuna=f_remover_acentos(comuna) %>% 
#            str_replace_all("Aysen","Aisen") %>% 
#            str_replace_all("Coyhaique","Coihaique")) %>% 
#   left_join(codigos_territoriales,by=c("nombre_comuna")) %>% 
#   left_join(mapa_regiones)



## EoF