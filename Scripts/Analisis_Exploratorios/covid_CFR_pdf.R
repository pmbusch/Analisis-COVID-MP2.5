### Analisis-COVID-MP2.5
## Impresion PDF Resumen  CFR (case fatality rate) asociado a Covid con metodologia de lag (rezago)
## IFR (Infection Fatality Rate)
## PBH Septiembre 2020

theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))

## Load Data ------------
# source("Scripts/Aggregate_Data/covid_CFR.R", encoding = "UTF-8") 


### Impresion PDF Resumen ---------------
library(cowplot)
library(grid)
library(rlang)
library(officer)

## Funcion para generar pagina resumen. Recibe el dataframe con la 
## serie de tiempo de casos y muertes
f_resumenCFR <- function(df, nivel="Nacional", zona="Nacional",cfr=T){
  if (cfr==F){
    df <- df %>% mutate(muertes=muertes+muertes_sospechoso,
                        muertes_acc=muertes_acc+muertes_sospechoso_acc,
                        muertes_sospechoso=0,muertes_sospechoso_acc=0)
  }
  
  ## Serie temporal
  serie_tiempo <- df %>% select(date,casos,muertes) %>% 
    gather(key,val,-date) %>% 
    ggplot(aes(date,val))+geom_line()+labs(x="",y="")+
    scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
    facet_grid(key~., scales = "free_y")+
    ggtitle("Serie de Tiempo")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # Tabla datos Brutos
  cfr_1 <- CFR.lags.F(df,dof = 5, lags = c(0,0),
                      detail = F, gg=T, geo=zona, cfr = cfr, out = T)
  cfr_2 <- CFR.lags.F(df,dof = 5, lags = c(10,10),
                      detail = F, gg=T, geo=zona, cfr = cfr, out = T)
  cfr_3 <- CFR.lags.F(df,dof = 5, lags = c(20,20),
                      detail = F, gg=T, geo=zona, cfr = cfr, out = T)
  
  tabla <- df %>% 
    summarise(Poblacion=max(poblacion, na.rm=T),
              `Contagios acumulados`=max(casos_acc, na.rm = T),
              `Muertes acumuladas`=max(muertes_acc, na.rm = T)) %>% 
    mutate(`% Letalidad`=`Muertes acumuladas`/`Contagios acumulados`*100 %>% round(2),
           `% CFR Bruto Lag 0`=cfr_1$fit*100 %>% round(2),
           `% CFR Bruto Lag 10`=cfr_2$fit*100 %>% round(2),
           `% CFR Bruto Lag 20`=cfr_3$fit*100 %>% round(2)) %>% 
    t() %>% as.data.frame() %>% rownames_to_column() %>% 
    rename(`Datos Brutos`=rowname, `Valor`=V1)
  
  tabla <- tabla %>%
    flextable() %>% 
    colformat_num(big.mark=" ", digits=0,i=1:3, j=2, na_str="s/i") %>%
    colformat_num(big.mark=" ", digits=2,i=4:7, j=2, na_str="s/i") %>%
    autofit(add_w = 0.1, add_h = 0.2) %>%
    align(j=1, align = "left")
  
  tabla_fig <- ggplot()+theme_void()+
    annotation_custom(rasterGrob(as_raster(tabla)), 
                      xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  
  
  ## Combinaciones CFR DOF 5 y Lag: 0-20, 10-20, 0-30
  cfr_4 <- CFR.lags.F(df,dof = 5, lags = c(0,20),
                      detail = F, gg=T, geo=zona, cfr = cfr)
  cfr_5 <- CFR.lags.F(df,dof = 5, lags = c(10,20),
                      detail = F, gg=T, geo=zona, cfr = cfr)
  cfr_6 <- CFR.lags.F(df,dof = 5, lags = c(0,30),
                      detail = F, gg=T, geo=zona, cfr = cfr)
  
  ## Plot conjunto
  p_sup <- plot_grid(tabla_fig, serie_tiempo,ncol=2,rel_widths = c(1,3))
  p_cfr <- plot_grid(cfr_4,cfr_5,cfr_6, ncol=3)
  p <- plot_grid(p_sup, p_cfr, ncol=1, rel_heights = c(1,1.5))
  
  # Title
  title <- ggdraw() +
    draw_label(paste("Estimacion ",if (cfr) "CFR" else "IFR",
                     ": Nivel ",nivel,"-",zona, sep=""), fontface='bold')
  p <- plot_grid(title,p, ncol=1, rel_heights=c(0.1, 1))
  
  return(p)
}
# Pruebas
# f_resumenCFR(df_covid_tiempo_nacional)
# f_resumenCFR(df_covid_tiempo_nacional %>% mutate(casos=casos_aplanados))
# f_resumenCFR(df_covid_tiempo_nacional, cfr = F)
# f_resumenCFR(df_covid_tiempo_region %>% filter(region=="M"),
#              nivel = "Regional", zona = "M")
# f_resumenCFR(df_covid_tiempo_region %>% filter(region=="IX"),
#              nivel = "Regional", zona = "IX")
# f_resumenCFR(df_covid_tiempo %>% filter(codigo_comuna=="13114"),
#              nivel = "Comunal", zona = "Las Condes")
# f_resumenCFR(df_covid_tiempo %>% filter(codigo_comuna=="13201"),
#              nivel = "Comunal", zona = "Puente Alto")
# f_resumenCFR(df_covid_edad %>% filter(sexo=="hombre" & grupo_edad=="65+"),
#              nivel = "Nacional", zona = "hombre-65+")


## Iteracion para generar PDF Resumen ----------
## Itero por las distintas df, en pos de generar resumens
options(warn=-1) # supress warnings
file_name <- "Figuras/%s.pdf"

## MODIFICAR AQUI PARA CALCULAR CON CASOS APLANADOS O ORIGINALES
bool_casos_aplanados <- T
if (bool_casos_aplanados){
  file_name <- "Figuras/%s_casos_aplan.pdf"
  df_covid_tiempo_nacional <- df_covid_tiempo_nacional %>% mutate(casos=casos_aplanados)
  df_covid_sexo <- df_covid_sexo %>% mutate(casos=casos_aplanados)
  df_covid_edad <- df_covid_edad %>% mutate(casos=casos_aplanados)
  df_covid_edadSexo <- df_covid_edadSexo %>% mutate(casos=casos_aplanados)
  df_covid_tiempo_region <- df_covid_tiempo_region %>% mutate(casos=casos_aplanados)
  df_covid_tiempo <- df_covid_tiempo %>% mutate(casos=casos_aplanados) 
}

## MODIFICAR AQUI PARA OBTENER CFR O IFR
pdf(sprintf(file_name, "CFR_lag"), width = 14.87, height = 9.30)
# pdf(sprintf(file_name, "IFR_lag"), width = 14.87, height = 9.30)
cfr_pdf <- T

# Nacional
f_resumenCFR(df_covid_tiempo_nacional, cfr = cfr_pdf) %>% print()

# Sexo
sexo <- df_covid_sexo$sexo %>% unique()
for (s in sexo){
  data_sexo <- df_covid_sexo %>% filter(sexo==s)
  fecha_min <- data_sexo %>% filter(casos!=0) %>% pull(date) %>% min()
  fecha_max <- data_sexo %>% filter(casos!=0) %>% pull(date) %>% max()
  data_sexo <- data_sexo %>% filter(date>=fecha_min & date<=fecha_max)
  f_resumenCFR(data_sexo, nivel = "Nacional",zona=s, cfr = cfr_pdf) %>% print()
}
rm(data_sexo, fecha_min, fecha_max,s)

# Grupo Edad
grupoEdad <- df_covid_edad$grupo_edad %>% unique()
for (g in grupoEdad){
  data_edad <- df_covid_edad %>% filter(grupo_edad==g)
  fecha_min <- data_edad %>% filter(casos!=0) %>% pull(date) %>% min()
  fecha_max <- data_edad %>% filter(casos!=0) %>% pull(date) %>% max()
  data_edad <- data_edad %>% filter(date>=fecha_min & date<=fecha_max)
  f_resumenCFR(data_edad, nivel = "Nacional",zona=g, cfr = cfr_pdf) %>% print()
}
rm(data_edad, fecha_min, fecha_max, g)

# Grupo Edad-Sexo
for (s in sexo){
  for (g in grupoEdad){
    data_edadSexo <- df_covid_edadSexo %>% filter(sexo==s & grupo_edad==g)
    fecha_min <- data_edadSexo %>% filter(casos!=0) %>% pull(date) %>% min()
    fecha_max <- data_edadSexo %>% filter(casos!=0) %>% pull(date) %>% max()
    data_edadSexo <- data_edadSexo %>% filter(date>=fecha_min & date<=fecha_max)
    f_resumenCFR(data_edadSexo, nivel = "Nacional",zona=paste(s,g,sep="-"), cfr = cfr_pdf) %>% print()
  }
}
rm(grupoEdad,sexo, data_edadSexo, fecha_min, fecha_max, s, g)

# Regional
regiones <- df_covid_tiempo_region$region %>% unique()
for (r in regiones){
  data_reg <- df_covid_tiempo_region %>% filter(region==r)
  fecha_min <- data_reg %>% filter(casos!=0) %>% pull(date) %>% min()
  fecha_max <- data_reg %>% filter(casos!=0) %>% pull(date) %>% max()
  data_reg <- data_reg %>% filter(date>=fecha_min & date<=fecha_max)
  f_resumenCFR(data_reg, nivel = "Regional",zona=r, cfr = cfr_pdf) %>% print()
}
rm(regiones, data_reg, fecha_min, fecha_max, r)

# Comunal
comunas <- df_covid_tiempo %>% 
  left_join(mapa_comuna) %>% 
  left_join(codigos_territoriales) %>% 
  arrange(region, nombre_comuna) %>% 
  filter(!(nombre_comuna %in% c("Chile Chico", "Vichuquen",
                                "Chonchi","Futaleufu","Antuco"))) %>% # Comunas con error
  pull(nombre_comuna) %>% unique()
df_covid_tiempo_c <- df_covid_tiempo %>% left_join(codigos_territoriales)
for (c in comunas){
  data_com <- df_covid_tiempo_c %>% filter(nombre_comuna==c)
  fecha_min <- data_com %>% filter(casos!=0) %>% pull(date) %>% min()
  fecha_max <- data_com %>% filter(casos!=0) %>% pull(date) %>% max()
  data_com <- data_com %>% filter(date>=fecha_min & date<=fecha_max)
  f_resumenCFR(data_com, nivel = "Comunal",zona=c, cfr = cfr_pdf) %>% print()
}
rm(comunas, data_com, fecha_min, fecha_max, c, df_covid_tiempo_c, bool_casos_aplanados)

# Cierro pdf
dev.off()
options(warn=0) # activate warnings


## EoF