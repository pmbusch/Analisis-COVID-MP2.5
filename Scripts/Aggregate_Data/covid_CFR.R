### Analisis-COVID-MP2.5
## Calculo CFR (case fatality rate) asociado a Covid con metodologia de lag (rezago)
## IFR (Infection Fatality Rate)
## PBH Septiembre 2020

theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")

## Load Data ------------
# Muertes -----
source("Scripts/Aggregate_Data/covidMuertes_agg.R", encoding = "UTF-8") 

# Remuevo edad y sexo
df_deis_edad <- df_deis_tiempo %>% 
  group_by(date, grupo_edad,sexo) %>% 
  summarise(muertes= sum(muertes, na.rm = T),
            muertes_acc = sum(muertes_acc, na.rm = T),
            muertes_sospechoso= sum(muertes_sospechoso, na.rm = T),
            muertes_sospechoso_acc = sum(muertes_sospechoso_acc, na.rm = T)) %>% ungroup()

df_deis_tiempo <- df_deis_tiempo %>% 
  group_by(date, codigo_comuna) %>% 
  summarise(muertes= sum(muertes, na.rm = T),
            muertes_acc = sum(muertes_acc, na.rm = T),
            muertes_sospechoso= sum(muertes_sospechoso, na.rm = T),
            muertes_sospechoso_acc = sum(muertes_sospechoso_acc, na.rm = T)) %>% ungroup()

# Nacional
df_muertes_nacional <- df_deis_tiempo %>%
  group_by(date) %>%
  summarise(muertes_acc=sum(muertes_acc,na.rm=T),
            muertes=sum(muertes, na.rm=T),
            muertes_sospechoso= sum(muertes_sospechoso, na.rm = T),
            muertes_sospechoso_acc = sum(muertes_sospechoso_acc, na.rm = T)) %>% ungroup()
df_muertes_nacional$muertes %>% sum()
ggplot(df_muertes_nacional, aes(date, muertes))+geom_line()+
  labs(x="",y="Casos mortalidad COVID confirmados")
# Region
df_muertes_region <- df_deis_tiempo %>% 
  left_join(mapa_comuna) %>% 
  filter(!is.na(region)) %>% ## comunas de antartica y otras
  group_by(region,date) %>% 
  summarise(muertes_acc=sum(muertes_acc,na.rm=T),
            muertes=sum(muertes, na.rm=T),
            muertes_sospechoso= sum(muertes_sospechoso, na.rm = T),
            muertes_sospechoso_acc = sum(muertes_sospechoso_acc, na.rm = T)) %>% ungroup()
ggplot(df_muertes_region, aes(date, muertes))+geom_line()+
  facet_wrap(~region, scales="free")+
  labs(x="",y="Casos mortalidad COVID confirmados")


# Contagios -----
source("Scripts/Aggregate_Data/covidCasos_agg.R", encoding = "UTF-8") 

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

## Add poblacion
df_covid_tiempo <- df_covid_tiempo %>% 
  left_join(df_poblacion %>% select(codigo_comuna, poblacion))

## Agrupo a nivel nacional
df_covid_tiempo_nacional <- df_covid_tiempo %>% 
  group_by(date) %>% 
  summarise(casos=sum(casos, na.rm = T),
            casos_acc=sum(casos_acc, na.rm = T),
            muertes=sum(muertes, na.rm=T),
            muertes_acc=sum(muertes_acc, na.rm = T),
            muertes_sospechoso= sum(muertes_sospechoso, na.rm = T),
            muertes_sospechoso_acc = sum(muertes_sospechoso_acc, na.rm = T),
            poblacion=sum(poblacion, na.rm=T)) %>% ungroup()
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
            muertes_acc=sum(muertes_acc, na.rm = T),
            muertes_sospechoso= sum(muertes_sospechoso, na.rm = T),
            muertes_sospechoso_acc = sum(muertes_sospechoso_acc, na.rm = T),
            poblacion=sum(poblacion, na.rm=T)) %>% ungroup()
# Scatter
ggplot(df_covid_tiempo_region, aes(casos, muertes))+ 
  geom_point(alpha=.5)+
  facet_wrap(~region, scales="free")


## Serie nacional con grupo edad y sexo --------------
df_deis_edad$date %>% range()
df_casos_edad$fecha %>% range()
## Borro primera fecha, dado que tiene 0 casos por el acumulado
fecha_min <- max(min(df_deis_edad$date), min(df_casos_edad$fecha))+1
fecha_max <- min(max(df_deis_edad$date), max(df_casos_edad$fecha))
df_deis_edad <- df_deis_edad %>% 
  filter(date>=fecha_min & date<=fecha_max)
df_casos_edad <- df_casos_edad %>% rename(date=fecha) %>% 
  filter(date>=fecha_min & date<=fecha_max)

df_covid_edadSexo <- df_deis_edad %>% 
  left_join(df_casos_edad)

## Add poblacion
pob <- df_grupoEdad %>% group_by(grupo_edad,sexo) %>% 
  summarise(poblacion=sum(poblacion, na.rm=T)) %>% ungroup()
df_covid_edadSexo <- df_covid_edadSexo %>% 
  left_join(pob)
rm(pob)

## Agrupo a nivel de solo sexo
df_covid_sexo <- df_covid_edadSexo %>% 
  group_by(date,sexo) %>% 
  summarise(casos=sum(casos, na.rm = T),
            casos_acc=sum(casos_acc, na.rm = T),
            muertes=sum(muertes, na.rm=T),
            muertes_acc=sum(muertes_acc, na.rm = T),
            muertes_sospechoso= sum(muertes_sospechoso, na.rm = T),
            muertes_sospechoso_acc = sum(muertes_sospechoso_acc, na.rm = T),
            poblacion=sum(poblacion, na.rm=T)) %>% ungroup()

## Agrupo a nivel de solo edad
df_covid_edad <- df_covid_edadSexo %>% 
  group_by(date,grupo_edad) %>% 
  summarise(casos=sum(casos, na.rm = T),
            casos_acc=sum(casos_acc, na.rm = T),
            muertes=sum(muertes, na.rm=T),
            muertes_acc=sum(muertes_acc, na.rm = T),
            muertes_sospechoso= sum(muertes_sospechoso, na.rm = T),
            muertes_sospechoso_acc = sum(muertes_sospechoso_acc, na.rm = T),
            poblacion=sum(poblacion, na.rm=T)) %>% ungroup()

## Calculo CFR Lag -----------
## funcion para imprimir IC en forma bonita y sintetica
ci.F = function(mid, low, high, digits=3, unidad='%', fact=1) {
  if (unidad=="%") fact=100
  paste(format(fact*mid,digits=digits),unidad," (",
        format(fact*low, digits=digits-1),"-", 
        format(fact*high, digits=digits-1),")", sep="")
}


# Funcion estimacion de CFR mediante lags
## LAC 2020
library(dlnm)
CFR.lags.F <-  function(data, dof=5, lags=c(10,20), add=T, detail=F,
                        gg=F, geo="", cfr=T) {
  last_date <- data$date %>% max()
  llags <- paste("Lags ", lags[1]," a ",lags[2],sep="")
  lmod <- paste("Poly ",dof," DoF",sep="")
  cfr_label <- if(cfr) "CFR" else "IFR"
  
  
  basis.covid <- crossbasis(data$casos,
                            lag=lags,     #Lag de N dias
                            #  argvar=list(fun = "ns", df=3),
                            argvar=list(fun="lin"),
                            #  arglag=list(fun="poly",degree=dof),
                            arglag=list(fun = "ns", df=dof))
  if (cfr==F){
    data <- data %>% mutate(muertes=muertes+muertes_sospechoso)
  }
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
  titulo=paste(geo, ": ",cfr_label,"=", CI.char, sep="")
  if (detail) print(titulo)
  
  
  ## ploteamos SOLO los slices
  if (detail) plot(modelo, "slices",  type = "p", pch = 19, cex = 1.5, var = 1, 
                   ci = "bars", ylab = paste(cfr_label," segun lag",sep=""), 
                   main = titulo, sub=stitulo)
  # plot(modelo, "overall", type = "p", pch = 19, cex = 1.5, var = 1, 
  #      ci = "bars", ylab = cfr_label, main = paste(cfr_label," total",sep=""))
  
  p1 <- ggplot(coef.all, aes(x=reorder(lagn, rowid), y=fit)) + 
    geom_hline(yintercept=0,color = "gray30", size=1) +
    # geom_line(lwd=1, col="red")  +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.1 ) +  
    geom_point(shape=21, size=3, col="black", fill="red", stroke=0.3) + 
    labs(x="",y=cfr_label)+
    scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
    # scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE)) +
    labs(title=titulo, subtitle=stitulo)+theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  if (detail|gg) return(p1)

}

CFR.lags.F(df_covid_tiempo_nacional,dof = 5, lags = c(10,20),
           detail = F, gg=T, geo="Nacional")
CFR.lags.F(df_covid_tiempo_nacional,dof = 5, lags = c(10,20),
           detail = F, gg=T, geo="Nacional", cfr = F)
CFR.lags.F(df_covid_tiempo_region %>% filter(region=="M"),
           dof = 5, lags = c(10,20),
           detail = F, gg=T, geo="Metropolitana")

CFR.lags.F(df_covid_tiempo %>% filter(codigo_comuna=="13114"),
           dof = 5, lags = c(10,20),
           detail = F, gg=T, geo="Las Condes")


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
  tabla <- df %>% 
    summarise(Poblacion=max(poblacion, na.rm=T),
              `Contagios acumulados`=max(casos_acc, na.rm = T),
              `Muertes acumuladas`=max(muertes_acc, na.rm = T)) %>% 
    mutate(`% Letalidad`=`Muertes acumuladas`/`Contagios acumulados`*100 %>% round(2)) %>% 
    t() %>% as.data.frame() %>% rownames_to_column() %>% 
    rename(`Datos Brutos`=rowname, `Valor`=V1)
  
  tabla <- tabla %>%
    flextable() %>% 
    colformat_num(big.mark=" ", digits=0,i=1:3, j=2, na_str="s/i") %>%
    colformat_num(big.mark=" ", digits=2,i=4, j=2, na_str="s/i") %>%
    autofit(add_w = 0.1, add_h = 0.2) %>%
    align(j=1, align = "left")
  
  tabla_fig <- ggplot()+theme_void()+
    annotation_custom(rasterGrob(as_raster(tabla)), 
                      xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  
  
  ## Combinaciones CFR = Lag: 10-20, DOF: 5; Lag: 10-20, DOF: 3; 
  ## Lag: 0-30, DOF: 5; Lag: 0-30, DOF: 3
  cfr_1 <- CFR.lags.F(df,dof = 5, lags = c(10,20),
             detail = F, gg=T, geo=zona, cfr = cfr)
  cfr_2 <- CFR.lags.F(df,dof = 3, lags = c(10,20),
             detail = F, gg=T, geo=zona, cfr = cfr)
  cfr_3 <- CFR.lags.F(df,dof = 5, lags = c(0,30),
             detail = F, gg=T, geo=zona, cfr = cfr)
  cfr_4 <- CFR.lags.F(df,dof = 3, lags = c(0,30),
             detail = F, gg=T, geo=zona, cfr = cfr)
  
  ## Plot conjunto
  p_sup <- plot_grid(tabla_fig, serie_tiempo,ncol=2,rel_widths = c(1,4))
  p_cfr <- plot_grid(cfr_1,cfr_2,cfr_3,cfr_4, ncol=2)
  p <- plot_grid(p_sup, p_cfr, ncol=1, rel_heights = c(1,1.5))
  
  # Title
  title <- ggdraw() +
    draw_label(paste("Estimacion ",if (cfr) "CFR" else "IFR",
                     ": Nivel ",nivel,"-",zona, sep=""), fontface='bold')
  p <- plot_grid(title,p, ncol=1, rel_heights=c(0.1, 1))
  
  return(p)
}

# Pruebas
f_resumenCFR(df_covid_tiempo_nacional)
f_resumenCFR(df_covid_tiempo_nacional, cfr = F)
f_resumenCFR(df_covid_tiempo_region %>% filter(region=="M"),
             nivel = "Regional", zona = "M")
f_resumenCFR(df_covid_tiempo_region %>% filter(region=="IX"),
             nivel = "Regional", zona = "IX")
f_resumenCFR(df_covid_tiempo %>% filter(codigo_comuna=="13114"),
             nivel = "Comunal", zona = "Las Condes")
f_resumenCFR(df_covid_tiempo %>% filter(codigo_comuna=="13201"),
             nivel = "Comunal", zona = "Puente Alto")
f_resumenCFR(df_covid_edad %>% filter(sexo=="hombre" & grupo_edad=="65+"),
             nivel = "Nacional", zona = "hombre-65+")


## Iteracion para generar PDF Resumen ----------
## Itero por las distintas df, en pos de generar resumens
options(warn=-1) # supress warnings
file_name <- "Figuras/%s.pdf"
# pdf(sprintf(file_name, "CFR_lag"), width = 14.87, height = 9.30)
pdf(sprintf(file_name, "IFR_lag"), width = 14.87, height = 9.30)
cfr_pdf <- F

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
  filter(!(nombre_comuna %in% c("Chile Chico"))) %>% # Comunas con error
  pull(nombre_comuna) %>% unique()
df_covid_tiempo_c <- df_covid_tiempo %>% left_join(codigos_territoriales)
for (c in comunas){
  data_com <- df_covid_tiempo_c %>% filter(nombre_comuna==c)
  fecha_min <- data_com %>% filter(casos!=0) %>% pull(date) %>% min()
  fecha_max <- data_com %>% filter(casos!=0) %>% pull(date) %>% max()
  data_com <- data_com %>% filter(date>=fecha_min & date<=fecha_max)
  f_resumenCFR(data_com, nivel = "Comunal",zona=c, cfr = cfr_pdf) %>% print()
}
rm(comunas, data_com, fecha_min, fecha_max, c, df_covid_tiempo_c)

# Cierro pdf
dev.off()
options(warn=0) # activate warnings


## EoF