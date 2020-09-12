### Analisis-COVID-MP2.5
## Calculo CRF (case fatality rate) asociado a covid con metodologia de lag (rezago)
## PBH Septiembre 2020

theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))

## Load Data ------------
# Muertes
source("Scripts/Aggregate_Data/covidMuertes_agg.R", encoding = "UTF-8") 

# Remuevo edad
df_deis_tiempo <- df_deis_tiempo %>% 
  group_by(date, codigo_comuna) %>% 
  summarise(muertes= sum(muertes, na.rm = T),
            muertes_acc = sum(muertes_acc, na.rm = T)) %>% ungroup()
  # Nacional
df_muertes_nacional <- df_deis_tiempo %>%
  group_by(date) %>%
  summarise(muertes_acc=sum(muertes_acc,na.rm=T),
            muertes=sum(muertes, na.rm=T)) %>% ungroup()
df_muertes_nacional$muertes %>% sum()
ggplot(df_muertes_nacional, aes(date, muertes))+geom_line()+
  labs(x="",y="Casos mortalidad COVID confirmados")
# Region
df_muertes_region <- df_deis_tiempo %>% 
  left_join(mapa_comuna) %>% 
  filter(!is.na(region)) %>% ## comunas de antartica y otras
  group_by(region,date) %>% 
  summarise(muertes_acc=sum(muertes_acc,na.rm=T),
            muertes=sum(muertes, na.rm=T)) %>% ungroup()
ggplot(df_muertes_region, aes(date, muertes))+geom_line()+
  facet_wrap(~region, scales="free")+
  labs(x="",y="Casos mortalidad COVID confirmados")


# Contagios
source("Scripts/Load_Data/covidCasos_load.R", encoding = "UTF-8") 

# Interpola dado que reporte es cada 3 dias
library(zoo)
df_casos_tiempo <- df_casos_tiempo %>%
  select(fecha, codigo_comuna, casos_confirmados) %>% 
  rename(casos_acc=casos_confirmados) %>% 
  # mutate(original=1) %>%
  group_by(codigo_comuna) %>% 
  complete(fecha = seq.Date(min(df_casos_tiempo$fecha), 
                           max(df_casos_tiempo$fecha),
                           by = "day"),codigo_comuna) %>%
  arrange(codigo_comuna,fecha ) %>% ungroup() %>% 
  mutate(casos_acc=na.approx(casos_acc) %>% round(0))

# Calcula casos por dia (resta del acumulado)
df_casos_tiempo <- df_casos_tiempo %>% 
  arrange(codigo_comuna,fecha) %>% 
  group_by(codigo_comuna) %>%
  mutate(casos = casos_acc - lag(casos_acc,default = first(casos_acc))) %>% 
  ungroup()

## Nota: Existen comunas que disminuyen sus casos acumulados entre informes
## ERROR DE LOS DATOS
df_casos_tiempo <- df_casos_tiempo %>% 
  mutate(casos=if_else(casos<0, 0, casos))


## Calculo a nivel nacional
df_casos_nacional <- df_casos_tiempo %>% group_by(fecha) %>% 
  summarise(casos_acc=sum(casos_acc, na.rm=T),
            casos=sum(casos, na.rm = T)) %>% ungroup()
ggplot(df_casos_nacional, aes(fecha, casos))+geom_line()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="Casos contagios COVID confirmados")

## Calculo a nivel regional
df_casos_region <- df_casos_tiempo %>% 
  left_join(mapa_comuna) %>% 
  filter(!is.na(region)) %>% ## comunas de antartica y otras
  group_by(region,fecha) %>% 
  summarise(casos_acc=sum(casos_acc, na.rm=T),
            casos=sum(casos, na.rm = T)) %>% ungroup()
ggplot(df_casos_region, aes(fecha, casos))+geom_line()+
  facet_wrap(~region, scales="free")+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="Casos contagios COVID confirmados")


## Juntar casos con  muertes ------------

## Filtro por rango de interseccion de fechas
df_deis_tiempo$date %>% range()
df_casos_tiempo$fecha %>% range()
## Borro primera fecha, dado que tiene 0 casos por el acumulado
fecha_min <- max(min(df_deis_tiempo$date), min(df_casos_tiempo$fecha))+1
fecha_max <- min(max(df_deis_tiempo$date), max(df_casos_tiempo$fecha))
df_deis_tiempo <- df_deis_tiempo %>% 
  filter(date>=fecha_min & date<=fecha_max)
df_casos_tiempo <- df_casos_tiempo %>% rename(date=fecha) %>% 
  filter(date>=fecha_min & date<=fecha_max)

df_covid_tiempo <- df_deis_tiempo %>% 
  left_join(df_casos_tiempo)

## Agrupo a nivel nacional
df_covid_tiempo_nacional <- df_covid_tiempo %>% 
  group_by(date) %>% 
  summarise(casos=sum(casos, na.rm = T),
            casos_acc=sum(casos_acc, na.rm = T),
            muertes=sum(muertes, na.rm=T),
            muertes_acc=sum(muertes_acc, na.rm = T)) %>% ungroup()
sum(df_covid_tiempo_nacional$muertes); sum(df_covid_tiempo_nacional$casos)
sum(df_covid_tiempo_nacional$muertes)/sum(df_covid_tiempo_nacional$casos)*100 # % letalidad

# Scatter
ggplot(df_covid_tiempo_nacional, aes(casos, muertes))+ geom_point(alpha=.5)

# Serie tiempo
df_covid_tiempo_nacional %>% select(date,casos,muertes) %>% 
  gather(key,val,-date) %>% 
  ggplot(aes(date,val))+geom_line()+labs(x="",y="")+
  facet_grid(key~., scales = "free_y")

## Agrupo a nivel region
df_covid_tiempo_region <- df_covid_tiempo %>% 
  left_join(mapa_comuna) %>% 
  group_by(date, region) %>% 
  summarise(casos=sum(casos, na.rm = T),
            casos_acc=sum(casos_acc, na.rm = T),
            muertes=sum(muertes, na.rm=T),
            muertes_acc=sum(muertes_acc, na.rm = T)) %>% ungroup()
# Scatter
ggplot(df_covid_tiempo_region, aes(casos, muertes))+ 
  geom_point(alpha=.5)+
  facet_wrap(~region, scales="free")

## Calculo CRF Lag -----------
## funcion para imprimir IC en forma bonita y sintetica
ci.F = function(mid, low, high, digits=3, unidad='%', fact=1) {
  if (unidad=="%") fact=100
  paste(format(fact*mid,digits=digits),unidad," (",
        format(fact*low, digits=digits-1),"-", 
        format(fact*high, digits=digits-1),")", sep="")
}


# Funcion estimacion de CRF mediante lags
## LAC 2020
library(dlnm)
CRF.lags.F <-  function(data, dof=5, lags=c(10,20), add=T, detail=F,gg=F, geo="") {
  last_date <- data$date %>% max()
  llags <- paste("Lags ", lags[1]," a ",lags[2],sep="")
  lmod <- paste("Poly ",dof," DoF",sep="")
  
  basis.covid <- crossbasis(data$casos,
                            lag=lags,     #Lag de N dias
                            #  argvar=list(fun = "ns", df=3),
                            argvar=list(fun="lin"),
                            #  arglag=list(fun="poly",degree=dof),
                            arglag=list(fun = "ns", df=dof))
  model <- lm(muertes ~ basis.covid, data)
  model.pred <- crosspred(basis.covid, model, at=seq(1,100,10), cumul =T)
  if (detail) plot.crosspred(model.pred,"3d", cumul = F,  xlab="casos", zlab="muertes", main=lmod)
  
  ## calculamos todo para 1 caso
  modelo <- crosspred(basis.covid, model, at=1, cumul =T)
  # modelo %>% names()
  
  # ----- extraemos los coeficientes de los lags
  tem <- data.frame( t(modelo$matfit), t(modelo$matlow), 
                    t(modelo$mathigh), t(modelo$matse))  # estos SI son los coefs- calzan con el plot
  coef.lags <- data.frame( fit=tem[,1], lwr=tem[,2], upr=tem[,3], se=tem[,4], 
                          lag=seq(lags[1],lags[2],1), lagn=rownames(tem))
  # coeficientes totales
  all.fit <- modelo$allfit
  all.lwr <- modelo$alllow
  all.upr <- modelo$allhigh
  coef.all <- data.frame(fit=all.fit, lwr=all.lwr, upr=all.upr, 
                         se=modelo$allse, lag=0, lagn="overall")
  
  row.names <- paste(rownames(tem), "overall")  # carretero pero funciona
  coef.all <- rbind(coef.lags, coef.all) %>% rowid_to_column()
  if (detail) print(coef.all)
  # print(coef.lags)
  
  CI.char = ci.F(all.fit, all.lwr, all.upr, digits=3 )
  stitulo = paste("Modelo lags:", lmod,", ", llags, "   Last date:", last_date)
  titulo=paste(geo, ": CRF=", CI.char, sep="")
  if (detail) print(titulo)
  
  
  ## ploteamos SOLO los slices
  if (detail) plot(modelo, "slices",  type = "p", pch = 19, cex = 1.5, var = 1, 
                   ci = "bars", ylab = "CRF segun lag", main = titulo, sub=stitulo)
  # plot(modelo, "overall", type = "p", pch = 19, cex = 1.5, var = 1, 
  #      ci = "bars", ylab = "CRF", main = "CRF total")
  
  p1 <- ggplot(coef.all, aes(x=reorder(lagn, rowid), y=fit)) + 
    geom_hline(yintercept=0,color = "gray30", size=1) +
    geom_line(lwd=1, col="red")  +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.1 ) +  
    geom_point(shape=21, size=3, col="black", fill="red", stroke=0.3) + 
    labs(x="",y="CRF")+
    scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
    # scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE)) +
    labs(title=titulo, subtitle=stitulo)+theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  if (detail|gg) return(p1)

}

CRF.lags.F(df_covid_tiempo_nacional,dof = 5, lags = c(10,20),
           detail = F, gg=T, geo="Nacional")

CRF.lags.F(df_covid_tiempo_region %>% filter(region=="M"),
           dof = 5, lags = c(10,20),
           detail = F, gg=T, geo="Metropolitana")

CRF.lags.F(df_covid_tiempo %>% filter(codigo_comuna=="13114"),
           dof = 5, lags = c(10,20),
           detail = F, gg=T, geo="Las Condes")


### Impresion PDF Resumen ---------------
library(cowplot)
library(grid)
library(rlang)
library(officer)


## Funcion para generar pagina resumen. Recibe el dataframe con la 
## serie de tiempo de caoss y muertes
f_resumenCRF <- function(df, nivel="Nacional", zona="Nacional"){
  
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
  tabla <- df %>% 
    summarise(`Contagios acumulados`=max(casos_acc, na.rm = T),
              `Muertes acumuladas`=max(muertes_acc, na.rm = T)) %>% 
    mutate(`% Letalidad`=`Muertes acumuladas`/`Contagios acumulados`*100 %>% round(2)) %>% 
    t() %>% as.data.frame() %>% rownames_to_column() %>% 
    rename(`Datos Brutos`=rowname, `Valor`=V1)
  
  tabla <- tabla %>%
    flextable() %>% 
    colformat_num(big.mark=" ", digits=0,i=1:2, j=2, na_str="s/i") %>%
    colformat_num(big.mark=" ", digits=2,i=3, j=2, na_str="s/i") %>%
    autofit(add_w = 0.1, add_h = 0.2) %>%
    align(j=1, align = "left")
  
  tabla_fig <- ggplot()+theme_void()+
    annotation_custom(rasterGrob(as_raster(tabla)), 
                      xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  
  
  ## Combinaciones CRF = Lag: 10-20, DOF: 5; Lag: 10-20, DOF: 3; 
  ## Lag: 0-30, DOF: 5; Lag: 0-30, DOF: 3
  crf_1 <- CRF.lags.F(df,dof = 5, lags = c(10,20),
             detail = F, gg=T, geo=zona)
  crf_2 <- CRF.lags.F(df,dof = 3, lags = c(10,20),
             detail = F, gg=T, geo=zona)
  crf_3 <- CRF.lags.F(df,dof = 5, lags = c(0,30),
             detail = F, gg=T, geo=zona)
  crf_4 <- CRF.lags.F(df,dof = 3, lags = c(0,30),
             detail = F, gg=T, geo=zona)
  
  ## Plot conjunto
  p_sup <- plot_grid(tabla_fig, serie_tiempo,ncol=2,rel_widths = c(1,4))
  p_crf <- plot_grid(crf_1,crf_2,crf_3,crf_4, ncol=2)
  p <- plot_grid(p_sup, p_crf, ncol=1, rel_heights = c(1,1.5))
  
  # Title
  title <- ggdraw() +
    draw_label(paste("Estimacion CRF: Nivel ", nivel,"-",zona, sep=""), 
               fontface='bold')
  p <- plot_grid(title,p, ncol=1, rel_heights=c(0.1, 1))
  
  return(p)
}

# Pruebas
f_resumenCRF(df_covid_tiempo_nacional)
f_resumenCRF(df_covid_tiempo_region %>% filter(region=="M"),
             nivel = "Regional", zona = "M")
f_resumenCRF(df_covid_tiempo_region %>% filter(region=="IX"),
             nivel = "Regional", zona = "IX")
f_resumenCRF(df_covid_tiempo %>% filter(codigo_comuna=="13114"),
             nivel = "Comunal", zona = "Las Condes")
f_resumenCRF(df_covid_tiempo %>% filter(codigo_comuna=="13201"),
             nivel = "Comunal", zona = "Puente Alto")



## Iteracion para generar PDF Resumen
options(warn=-1) # supress warnings
file_name <- "Figuras/%s.pdf"
pdf(sprintf(file_name, "CRF_lag"), width = 14.87, height = 9.30)

# Nacional
f_resumenCRF(df_covid_tiempo_nacional) %>% print()

# Regional
regiones <- df_covid_tiempo_region$region %>% unique()
for (r in regiones){
  data_reg <- df_covid_tiempo_region %>% filter(region==r)
  f_resumenCRF(data_reg, nivel = "Regional",zona=r) %>% print()
}
rm(regiones, data_reg)

# Comunal
# df_covid_tiempo <- df_covid_tiempo %>% left_join(codigos_territoriales)
comunas <- df_covid_tiempo %>% 
  arrange(region, nombre_comuna) %>% 
  filter(!(nombre_comuna %in% c("Chile Chico"))) %>% # Comunas con error
  pull(nombre_comuna) %>% unique()
for (c in comunas){
  data_com <- df_covid_tiempo %>% filter(nombre_comuna==c)
  f_resumenCRF(data_com, nivel = "Comunal",zona=c) %>% print()
}
rm(comunas, data_com)

# Cierro pdf
dev.off()
options(warn=0) # activate warnings


## EoF