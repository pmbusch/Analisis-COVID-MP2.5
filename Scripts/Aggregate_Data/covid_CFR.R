### Analisis-COVID-MP2.5
## Calculo CFR (case fatality rate) asociado a Covid con metodologia de lag (rezago)
## IFR (Infection Fatality Rate)
## PBH Septiembre 2020

theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
print_ggplot <- F

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

if (print_ggplot) ggplot(df_muertes_nacional, aes(date, muertes))+geom_line()+
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

if (print_ggplot) ggplot(df_muertes_region, aes(date, muertes))+geom_line()+
  facet_wrap(~region, scales="free")+
  labs(x="",y="Casos mortalidad COVID confirmados")


# Contagios -----
source("Scripts/Aggregate_Data/covidCasos_agg.R", encoding = "UTF-8")

## Calculo a nivel nacional
df_casos_nacional <- df_casos_tiempo %>% group_by(fecha) %>% 
  summarise(casos_acc=sum(casos_acc, na.rm=T),
            casos=sum(casos, na.rm = T),
            casos_aplanados_acc=sum(casos_aplanados_acc, na.rm=T),
            casos_aplanados=sum(casos_aplanados, na.rm = T)) %>% ungroup()

if (print_ggplot) ggplot(df_casos_nacional, aes(fecha, casos))+geom_line()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="Casos contagios COVID confirmados")

## Calculo a nivel regional
df_casos_region <- df_casos_tiempo %>% 
  left_join(mapa_comuna) %>% 
  filter(!is.na(region)) %>% ## comunas de antartica y otras
  group_by(region,fecha) %>% 
  summarise(casos_acc=sum(casos_acc, na.rm=T),
            casos=sum(casos, na.rm = T),
            casos_aplanados_acc=sum(casos_aplanados_acc, na.rm=T),
            casos_aplanados=sum(casos_aplanados, na.rm = T)) %>% ungroup()

if (print_ggplot) ggplot(df_casos_region, aes(fecha, casos))+geom_line()+
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
            casos_aplanados_acc=sum(casos_aplanados_acc, na.rm=T),
            casos_aplanados=sum(casos_aplanados, na.rm = T),
            muertes=sum(muertes, na.rm=T),
            muertes_acc=sum(muertes_acc, na.rm = T),
            muertes_sospechoso= sum(muertes_sospechoso, na.rm = T),
            muertes_sospechoso_acc = sum(muertes_sospechoso_acc, na.rm = T),
            poblacion=sum(poblacion, na.rm=T)) %>% ungroup()
sum(df_covid_tiempo_nacional$muertes); sum(df_covid_tiempo_nacional$casos)
sum(df_covid_tiempo_nacional$muertes)/sum(df_covid_tiempo_nacional$casos)*100 # % letalidad

# Scatter
if (print_ggplot) ggplot(df_covid_tiempo_nacional, aes(casos, muertes)) + 
  geom_point(alpha=.5)

# Serie tiempo casos y muertes
if (print_ggplot) df_covid_tiempo_nacional %>% select(date,casos,muertes) %>% 
  gather(key,val,-date) %>% 
  ggplot(aes(date,val))+geom_line()+labs(x="",y="")+
  facet_grid(key~., scales = "free_y")

## Serie tiempo CFR bruto
if (print_ggplot) df_covid_tiempo_nacional %>% 
  mutate(cfr=muertes/casos*100) %>% 
  ggplot(aes(date,cfr))+geom_line()+
  geom_hline(aes(yintercept = mean(cfr)), linetype="dashed")+
  labs(x="",y="CFR Bruta [%]")


## Agrupo a nivel region
df_covid_tiempo_region <- df_covid_tiempo %>% 
  left_join(mapa_comuna) %>% 
  group_by(date, region) %>% 
  summarise(casos=sum(casos, na.rm = T),
            casos_acc=sum(casos_acc, na.rm = T),
            casos_aplanados_acc=sum(casos_aplanados_acc, na.rm=T),
            casos_aplanados=sum(casos_aplanados, na.rm = T),
            muertes=sum(muertes, na.rm=T),
            muertes_acc=sum(muertes_acc, na.rm = T),
            muertes_sospechoso= sum(muertes_sospechoso, na.rm = T),
            muertes_sospechoso_acc = sum(muertes_sospechoso_acc, na.rm = T),
            poblacion=sum(poblacion, na.rm=T)) %>% ungroup()
# Scatter
if (print_ggplot) ggplot(df_covid_tiempo_region, aes(casos, muertes))+ 
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
            casos_aplanados_acc=sum(casos_aplanados_acc, na.rm=T),
            casos_aplanados=sum(casos_aplanados, na.rm = T),
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
            casos_aplanados_acc=sum(casos_aplanados_acc, na.rm=T),
            casos_aplanados=sum(casos_aplanados, na.rm = T),
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
CFR.lags.F <-  function(data, dof=5, lags=c(10,20), add=T, detail=F, out=F,
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
  if (out) return(coef.all %>% select(fit, lwr, upr)) # return overall CRF
  
  row.names <- paste(rownames(tem), "overall")  # carretero pero funciona
  # coef.all <- rbind(coef.lags, coef.all) %>% rowid_to_column() # Add overall to graph
  coef.all <- coef.lags %>% rowid_to_column()
  
  
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

# CFR.lags.F(df_covid_tiempo_nacional,dof = 5, lags = c(10,20),
#            detail = F, gg=T, geo="Nacional", cfr=T, out=F)
# CFR.lags.F(df_covid_tiempo_nacional,dof = 5, lags = c(10,20),
#            detail = F, gg=T, geo="Nacional", cfr = F)
# CFR.lags.F(df_covid_tiempo_nacional %>% mutate(casos=casos_aplanados),
#            dof = 5, lags = c(10,20),
#            detail = F, gg=T, geo="Nacional", cfr=T, out=F)
# CFR.lags.F(df_covid_tiempo_region %>% filter(region=="M"),
#            dof = 5, lags = c(10,20),
#            detail = F, gg=T, geo="Metropolitana")
# CFR.lags.F(df_covid_tiempo %>% filter(codigo_comuna=="13114"),
#            dof = 5, lags = c(10,20),
#            detail = F, gg=T, geo="Las Condes")

## Lags brutos??
# CFR.lags.F(df_covid_tiempo_nacional,dof = 5, lags = c(0,0),
#            detail = F, gg=T, geo="Nacional", cfr=T, out=F)
# CFR.lags.F(df_covid_tiempo_nacional,dof = 5, lags = c(10,10),
#            detail = F, gg=T, geo="Nacional", cfr=T, out=F)
# CFR.lags.F(df_covid_tiempo_nacional,dof = 5, lags = c(20,20),
#            detail = F, gg=T, geo="Nacional", cfr=T, out=F)


## Obtencion CFR/IFR comunas -----------------
## Iteracion para obtener el CFR a nivel de comuna
## CFR Obtenidos:
## Brutos (lags en un solo dia): 0, 10, 20 (modelo funciona igual, predice a partir de estos efectos rezagos, pero solo de un dia)
## 0-20, 10-20, 0-30

comunas <- df_covid_tiempo %>% 
  left_join(mapa_comuna) %>% 
  left_join(codigos_territoriales) %>% 
  arrange(region, nombre_comuna) %>% 
  filter(!(nombre_comuna %in% c("Chile Chico","Vichuquen",
                                "Chonchi","Futaleufu", "Antuco",
                                "Curaco de Velez"))) %>% # Comunas con error
  pull(nombre_comuna) %>% unique()
df_covid_tiempo_c <- df_covid_tiempo %>% left_join(codigos_territoriales)

## CFR a probar mediante iteracion
cfr_array <- c("cfr", "ifr")
## Lags a probar mediante iteracion
lags_array <- data.frame(c(0,0), c(10,10), c(20,20), c(0,20), c(10,20), c(0,30)) %>% 
  t() %>% as.data.frame() %>% rename(lag_low=V1, lag_upper=V2)
# Itero por casos observados vs casos aplanados (corregidos)
casos_aplanados_array <- c(F,T)
# DF para guardar todo
cfr_comunas <- data.frame()
for (c in comunas){
  cat("Comuna ",c, " \n", sep = "")
  data_com <- df_covid_tiempo_c %>% filter(nombre_comuna==c)
  fecha_min <- data_com %>% filter(casos!=0) %>% pull(date) %>% min()
  fecha_max <- data_com %>% filter(casos!=0) %>% pull(date) %>% max()
  data_com <- data_com %>% filter(date>=fecha_min & date<=fecha_max)
  
  cfr_com <- data.frame()
  ## Itero segun casos aplanados o no
  for (x in casos_aplanados_array){
    if (x){
      data_com <- data_com %>% mutate(casos=casos_aplanados) #modifico mi variable de casos
    }
    # Obtiene CFR e IFR. Itero ademas por intervalos de lags
    for (z in cfr_array){
      for( l in 1:nrow(lags_array)){
        lag_aux <- c(lags_array[l,1],lags_array[l,2])
        df_cfr_aux <- CFR.lags.F(data_com,dof = 5, lags = lag_aux,
                                 detail = F, gg=F, out=T,
                                 cfr=(z=="cfr")) %>% 
          mutate_if(is.numeric, function(x) x*100) %>% 
          mutate(modelo=z,
                 lag_low=lags_array[l,1],
                 lag_upper=lags_array[l,2],
                 casos_orig=if_else(x,"aplanados","original"))
        cfr_com <- rbind(cfr_com, df_cfr_aux)
        rm(lag_aux, df_cfr_aux)
      }
    }
  }
  
  # Asigno comuna
  cfr_com <- cfr_com %>% 
    mutate(codigo_comuna=data_com$codigo_comuna[1])
  # Uno todo
  cfr_comunas <- rbind(cfr_comunas, cfr_com)
  # Clean
  rm(data_com, fecha_min, fecha_max,cfr_com)
}
rm(comunas, c,l,z,x, df_covid_tiempo_c, lags_array, cfr_array)

## Expando dataframe para crear multiples columnas
# genera nombre columnas
cfr_comunas <- cfr_comunas %>% 
  mutate(lag_low=if_else(lag_low==lag_upper,"raw",as.character(lag_low)),
         key=paste(modelo,lag_low,lag_upper,casos_orig,sep="_") %>% 
           str_remove("_original"))
# expande
cfr_comunas <- cfr_comunas %>% select(codigo_comuna, key, fit) %>% 
  spread(key,fit)
  
rm(print_ggplot, df_muertes_nacional, df_muertes_region, df_casos_nacional, 
   df_casos_region, df_deis_tiempo, df_casos_tiempo, df_covid_tiempo_nacional,
   df_covid_tiempo_region, df_deis_edad, df_casos_edad, df_covid_edadSexo,
   df_covid_sexo, df_covid_edad, df_covid_tiempo)

## EoF