### Analisis-COVID-MP2.5
## Figuras exploracion datos
## Funciones de visualizacion fijas
## PBH Julio 2020

theme_set(theme_bw())
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
source("Scripts/00-Funciones.R", encoding = "UTF-8")


## MAPAS ## ---------
## Mapa Chile ------------
# Funcion para graficar mediante colores una variable y guardarla
# Utiliza como base un df unido a la variable mapa_comuna
# Se puede filtar antes el df para graficar una zona geografica exclusiva
# Recibe dataframe, valor a graficar, escala, titulo leyenda, guardar
fig_mapa <- function(df, val, limites=NULL, titulo="", fileName=NULL, lwd=0.5){
  p <- df %>% 
    ggplot() + 
    geom_sf(aes(fill = {{val}}, geometry = geometry), lwd=lwd) +
    scale_fill_viridis_c(name = titulo, 
                         option="B", direction=-1, na.value = "white",
                         limits=limites,
                         labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F)) +
    labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
    theme_minimal(base_size = 8)
  # Guardo
  if (!is.null(fileName)){
    cat("Saving: ",fileName)
    ggsave(fileName,last_plot(),dpi=600,
           width = 14.87, height = 9.30, units = "in")
  }
  p
}



## EoF