### Analisis-COVID-MP2.5
## PDF Resumen datos numericos de variables a nivel comunal
## PBH Septiembre 2020



## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figuras/%s.pdf"

# Librerias especiales
library(cowplot)
library(grid)
library(rlang)

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
    border(j=1:2,i=1, part="body",
           border.top = officer::fp_border(style = "solid", width=2))
  
  tabla_fig <- ggplot()+theme_void()+
    annotation_custom(rasterGrob(as_raster(tabla)), 
                      xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  rm(tabla)
  
  # Grafico dispersion jitter
  p_jitter <- ggplot(df, aes(x="",y=(!!sym(var))))+
    geom_jitter(alpha=.5)+
    scale_y_continuous(
      labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    coord_flip(expand = F)+
    labs(x="",y="")
  
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
# p_prueba <- f_generaFiguraResumen(df_modelo, "perc_isapre")
# p_prueba
# rm(p_prueba)

## Iteracion por columnas numericas --------------
options(warn=-1) # supress warnings
col_names <- names(df_modelo %>% select_if(is.numeric))
# Ordenar por tipo variable
col_names <- tibble(tipo=f_addTypeVar(col_names),
                   nombre=col_names) %>% arrange(tipo) %>% pull(nombre)
pdf(sprintf(file_name, "Resumen_Var"),
    paper="a4r")
for (c in col_names){
  f_generaFiguraResumen(df_modelo, c) %>% print()
}
dev.off()
options(warn=0) # activate warnings
  