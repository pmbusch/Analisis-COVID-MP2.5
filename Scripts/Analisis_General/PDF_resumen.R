### Analisis-COVID-MP2.5
## PDF Resumen datos numericos de variables a nivel comunal
## PBH Septiembre 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_General/Figuras/%s.pdf"

# Librerias especiales
library(cowplot)
library(grid)
library(rlang)
library(officer)

# Data frame para generar los pdf ------
df_pdf <- mapa_comunas %>% 
  mutate(centroide=st_centroid(geometry),
         longitud=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         latitud=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  select(codigo_comuna,longitud, latitud) %>% 
  right_join(df_modelo, by=c("codigo_comuna")) %>% 
  filter(!is.na(latitud)) %>% arrange(desc(latitud))


## Funcion que genera figura resumen ---------
f_generaFiguraResumen <- function(df, var){
  # Tabla resumen
  tabla <- df %>% select((!!sym(var))) %>% 
    summarise(Promedio=mean((!!sym(var)),na.rm=T),
              SD=sd((!!sym(var)),na.rm=T),
              Min=min((!!sym(var)),na.rm=T),
              P25=quantile((!!sym(var)),0.25,na.rm=T),
              Mediana=median((!!sym(var)),na.rm=T),
              P75=quantile((!!sym(var)),0.75,na.rm=T),
              Max=max((!!sym(var)),na.rm=T),
              `Obs`=sum(!is.na((!!sym(var)))),
              `Obs faltantes`=sum(is.na((!!sym(var)))),
              `% Completitud`=(1-`Obs faltantes`/n())*100)
  tabla <- tabla %>% mutate_all(function(x) round(x,2))
  names_tabla <- names(tabla)
  tabla <- rbind(names_tabla, tabla) %>% t() %>% as.data.frame()
  rm(names_tabla)
  
  ## Flextable as plot object
  tabla <- tabla %>% flextable() %>% 
    autofit(add_w = 0.1, add_h = 0.2) %>%
    align(j=1, align = "left") %>% 
    delete_part(part = "header") %>% 
    flextable::border(j=1:2,i=1, part="body",
           border.top = fp_border(style = "solid", width=2))
  
  tabla_fig <- ggplot()+theme_void()+
    annotation_custom(rasterGrob(as_raster(tabla)), 
                      xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  rm(tabla)
  
  # Grafico dispersion jitter
  comuna_label <- df_pdf %>%
    mutate(comuna_label=case_when(
      # nombre_comuna=="General Lagos" ~ "Norte",
      # nombre_comuna=="Cabo de Hornos" ~ "Sur",
      nombre_comuna %in% c("Arica"," La Serena","Santiago",
                           "Talca","Concepcion","Temuco",
                           "Puerto Montt",
                           "Punta Arenas") ~ nombre_comuna,
      T ~ "")) %>% pull(comuna_label)
  comuna_label <- rev(comuna_label)  
  
  p_jitter <- df_pdf %>% 
    ggplot(aes(x=reorder(nombre_comuna,latitud),y=(!!sym(var))))+
    geom_point(alpha=.3)+
    scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    scale_x_discrete(labels=comuna_label)+
    coord_flip(expand = T)+
    labs(x="",y="")+
    theme(axis.title.y=element_blank(),
          axis.ticks.y=element_blank())
  
  # Grafico densidad
  p_dens <- ggplot(df, aes((!!sym(var))))+
    geom_density(fill="brown", alpha=.5)+
    scale_x_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    coord_cartesian(expand = F)+
    labs(y="",x="")
  
  # Grafico ECDF
  p_ecdf <- ggplot(df, aes((!!sym(var))))+
    stat_ecdf(col="black", size=1)+
    scale_x_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    coord_cartesian(expand = F)+
    labs(y="",x="")
  
  ## Gather plots
  p <- plot_grid(tabla_fig, p_jitter, p_dens, p_ecdf, ncol=2)
  
  # Title
  title <- ggdraw() +
    draw_label(paste(f_addTypeVar(var), f_replaceVar(var),sep=": "), fontface='bold')
  p <- plot_grid(title,p, ncol=1, rel_heights=c(0.1, 1))
  return(p)
}
# f_generaFiguraResumen(df_pdf, "perc_isapre")
# f_generaFiguraResumen(df_pdf, "perc_lenaCalefaccion")
# f_generaFiguraResumen(df_pdf, "perc_salud")
# f_generaFiguraResumen(df_pdf, "cons_lena_kg")
# f_generaFiguraResumen(df_pdf, "perc_vivHacMedio")
# f_generaFiguraResumen(df_pdf, "perc_vivAntes2002")
# f_generaFiguraResumen(df_pdf, "movilidad")

## Iteracion por columnas numericas --------------
# df_pdf <- df_pdf %>% filter(!is.na(mp25)) # Filtro MP2.5
options(warn=-1) # supress warnings
col_names <- df_pdf %>% select_if(is.numeric) %>% 
  select(-latitud,-longitud) %>% names()
# Ordenar por tipo variable
col_names <- tibble(tipo=f_addTypeVar(col_names),
                    nombre=col_names) %>% arrange(tipo) %>% pull(nombre)
pdf(sprintf(file_name, "Resumen_Var"), width = 14.87, height = 9.30)
# pdf(sprintf(file_name, "Resumen_Var_mp25"), width = 14.87, height = 9.30)
for (c in col_names){
  f_generaFiguraResumen(df_pdf, c) %>% print()
}
dev.off()
options(warn=0) # activate warnings

## EoF