### Analisis-COVID-MP2.5
## Analisis Transversal
## Fuente principal codigo: Exposure to air pollution and COVID-19 mortality in the United States A nationwide cross-sectional study
## https://github.com/wxwx1993/PM_COVID
## Fuente: Bates, D., Mächler, M., Bolker, B., & Walker, S. (2014). Fitting linear mixed-effects models using lme4. arXiv preprint arXiv:1406.5823.
## PBH Julio 2020

## Librerias ------
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figuras/Analisis_transversal/%s.png"
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/05-FuncionesAnalisisTransversal.R", encoding = "UTF-8")

library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
df_modelo %>% na.omit() %>% nrow() # dimension todas las variables


## MODELO DEFAULT-----
# Varaibles se seleccionaron con ayuda de Step, pero con logica externa, 
# segun cuales podrian ser explicativas (y usadas en otros estudios)
df_modelo %>% names()
df <- df_modelo %>% 
  dplyr::select(nombre_comuna,region, poblacion,tasa_mortalidad_covid,
                covid_fallecidos,mp25,
                densidad_pob, `65+`, `15-44`, perc_puebloOrig, perc_rural,
                dias_primerContagio, dias_cuarentena,tasa_camas,
                perc_lenaCocina,
                ingresoTotal_media,perc_menor_media,
                perc_fonasa_A, perc_fonasa_D, perc_isapre,
                tmed_summer, tmed_winter, 
                heating_degree_15_summer, heating_degree_15_winter,
                heating_degree_18_summer, heating_degree_18_winter)

df %>% na.omit() %>% dim() #dimension
# df <- df %>% na.omit()

## Poblacion va a off_set, dado que se estima la tasa
# Buena explicacion: https://stats.stackexchange.com/questions/11182/when-to-use-an-offset-in-a-poisson-regression
mod <- glmer.nb(covid_fallecidos ~ 
                  mp25 +
                  scale(densidad_pob) + scale(`15-44`) + scale(`65+`) +
                  scale(perc_puebloOrig) + scale(perc_rural) +
                  scale(dias_primerContagio) +  scale(dias_cuarentena) + 
                  scale(tasa_camas) + 
                  scale(perc_lenaCocina) + 
                  scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                  scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                  scale(tmed_summer) + scale(tmed_winter) + 
                  scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                  (1|region)+
                  offset(log(poblacion)), 
                data = df,
                na.action=na.omit)

# Solucion no convergencia
ss <- getME(mod,c("theta","fixef"))
# mod2 <- update(mod,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
# summary(mod2)
mod3 <- update(mod,start=ss,control=glmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=2e5)))
summary(mod3)
mod <- mod3
# Summary
summary(mod)

##  MRR -------
# Se interpreta como el aumento relativo en la tasa de mortalidad covid por 1 ug/m3
# Dado que los confundentes estan estandarizados, su MRR se interepreta como 
# variacion relativa al aumento en 1 desviacion estandar de la variable confundente
# Fuente: https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/

# Coef of fixed effects
f_tableCoef(mod)
  # print(preview="pptx")

## MRR
f_tableMRR(mod)
  # print(preview="pptx")
  # print(preview="docx")

## Figura MRR
f_figMRR(mod)+scale_y_continuous(breaks=seq(0,9,1))
f_savePlot(last_plot(), sprintf(file_name,"MRR"), dpi=100)



## Varianza random effects -----
#  Si es muy cercana a cero un modelo si en este efecto es casi igual
print(vc <- VarCorr(mod), comp = c("Variance","Std.Dev."))
nobs(mod) # number of rows into model
ngrps(mod) # number of levels of the Subject grouping factor
sigma(mod) # residual standard deviation
formula(mod)
# Coeficients of grouping variable, condicionados
ranef(mod)

## Residuales -----
residuals(mod, scaled=T)
quantile(residuals(mod, "pearson", scaled = TRUE))


# Diagnostic plots ----
plot(mod,type=c("p","smooth")) ## fitted vs residual
plot(mod,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth")) ## scale-location
lattice::qqmath(mod,id=0.05) ## quantile-quantile

## Prediction ------
# type=responde https://stackoverflow.com/questions/47486589/what-is-the-difference-between-type-response-terms-and-link-in-predict-f
# type=link entrega el logaritmo natural de la tasa de mortalidad (variable dependiente)
# MODELO PREDICE CASOS FALLECIDOS!
# df %>% 
#   mutate(y=predict(mod,type="response"),
#          res=covid_fallecidos-y,
#          residuals=residuals(mod, type="response")) %>% 
#   dplyr::select(nombre_comuna,poblacion,covid_fallecidos,y,res,residuals,
#                 mp25,densidad_pob, tasa_mortalidad_covid) %>% view()

# comparacion prediccion
df %>% 
  ggplot(aes(x = mp25, y = covid_fallecidos/poblacion*1e5)) +
  geom_point(col="green",alpha=.5) +
  geom_line(aes(y = predict(mod,type="response")/poblacion*1e5),
            size=1, col="red", alpha=.5)+
  labs(x="Concentración MP2.5 2016-2019 [ug/m3]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]")

df %>% 
  na.omit() %>% 
  mutate(rm=if_else(region=="M","RM","Resto Chile") %>% factor()) %>% 
  ggplot(aes(x = covid_fallecidos/poblacion*1e5)) +
  geom_point(aes(y = predict(mod,type="response")/poblacion*1e5,
                 col=rm),
             # col="red", 
             size=4,
             alpha=.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  coord_cartesian(xlim=c(0,250),ylim=c(0,250), expand = T)+
  labs(x="Observada", y="Predicción", col="")+
  # ggtitle("Tasa Mortalidad COVID [muertes/100mil hab]")+
  theme(panel.grid.minor = element_blank())
f_savePlot(last_plot(), sprintf(file_name,"Obs_vs_Pred"), dpi=300)

## Mapa comparativo ---------
# detach("package:gamm4", unload = TRUE)
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")
df_map <- df %>%
  na.omit() %>% 
  mutate(pred=predict(mod, type="response")/poblacion*1e5) %>% 
  left_join(codigos_territoriales) %>% 
  dplyr::select(codigo_comuna, covid_fallecidos,poblacion, pred) %>% 
  mutate(obs=covid_fallecidos/poblacion*1e5) %>% 
  right_join(mapa_comuna) %>% 
  gather(tipo,tasa,obs, pred)

df_map %>% 
  fig_mapaChile_facet(tasa, facets = ~tipo, limites=c(0,500),
                      titulo="Tasa Mortalidad COVID [muertes/100mil hab]")
f_savePlot(last_plot(), sprintf(file_name,"ModeloChile"))
rm(df_map)

## Comparacion con Poisson -----------
# BinNeg vs Poisson
# https://stats.stackexchange.com/questions/311556/help-interpreting-count-data-glmm-using-lme4-glmer-and-glmer-nb-negative-binom
modelformula <- formula(covid_fallecidos ~ 
                          mp25 +
                          scale(densidad_pob) + scale(`15-44`) + scale(`65+`) +
                          scale(perc_puebloOrig) + scale(perc_rural) +
                          scale(dias_primerContagio) +  scale(dias_cuarentena) + 
                          scale(tasa_camas) + 
                          scale(perc_lenaCocina) + 
                          scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                          scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                          scale(tmed_summer) + scale(tmed_winter) + 
                          scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                          offset(log(poblacion)))
poismodel <- glm(modelformula, data = df, family = "poisson")   
nbmodel <- glm.nb(modelformula, data = df)
library(lmtest)
# Poisson vs Negative Binomial
lrtest(poismodel, nbmodel)
# Neg-Bin is a better fit

# Negative Binomial vs Mixed Model
lrtest(nbmodel, mod)
# Mixed Model is a better fit

rm(modelformula, poismodel, nbmodel)


## STEPWISE ------------
# https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection/20856#20856
library(caret)

## Creo df solo con variables numericas de interes (y fuera de COVID)
df <-  df_modelo %>% select_if(is.numeric) %>% 
  dplyr::select(
    # -covid_fallecidos, -poblacion,
    -tasa_mortalidad_covid,
    -tasa_contagios,-casos_confirmados,
    -pcr_region, -perc_letalidad,-defunciones,-tasa_mortalidad_all) %>% 
  na.omit()

# Columnas a remover dado que serian redundantes por su correlacion con otras variables
# identify and eliminate collinear variables
cols <- df %>% 
  cor() %>% 
  findCorrelation()
# Columnas fuera
df[,cols] %>% names()

df <- df[,-cols]

## Train with ML
getModelInfo("glmStepAIC")
modelLookup("glmStepAIC")

# AIC estimates the relative amount of information lost by a given model: 
# the less information a model loses, the higher the quality of that model
glm_fit <- train(covid_fallecidos~ . +offset(log(poblacion)),
  # tasa_mortalidad ~ ., 
                 data=df,
                 method="glmStepAIC",
                 # method="glmnet",
                 family="poisson",
                 link="log",
                 na.action = na.omit)
glm_fit
summary(glm_fit)

# NOTAS: Algoritmo step funciona con familia Poisson, y sin efectos aleatorios


# Poisson GLM peude dar el valor inicial del theta
# Theta is usually interpreted as a measure of overdispersion with respect to the Poisson distribution
# https://stats.stackexchange.com/questions/10419/what-is-theta-in-a-negative-binomial-regression-fitted-with-r
getME(mod,"glmer.nb.theta") #valor del modelo origal
# getME(glm_fit,"glmer.nb.theta")
glm_fit_nb <- train(covid_fallecidos~ . +offset(log(poblacion)),
                 data=df,
                 method="glmStepAIC",
                 family=negative.binomial(theta=279.9946,link="log"),
                 na.action = na.omit)
glm_fit_nb
summary(glm_fit_nb)


# ## Otra prueba
# df <-  df_modelo %>% 
#   dplyr::select(-codigo_comuna, -nombre_comuna,-codigo_provincia,-nombre_provincia,
#                 -codigo_region,-nombre_region,-region,
#                 -tasa_mortalidad_all,-geometry,
#                 -tasa_contagios,-casos_confirmados,
#                 -pcr_region, -perc_letalidad,-defunciones,-tasa_mortalidad_all) %>% 
#   na.omit()
# 
# # Debo cambiar la semilla, sino no encuentra solucion factible
# set.seed(3, sample.kind="Rounding")
# glm_fit <- train(covid_fallecidos ~ .+ offset(log(poblacion)), 
#                  data=df,
#                  method="glm.nb",
#                  na.action = na.omit)
# 
# glm_fit
# summary(glm_fit)


# cols <- df %>% 
#   cor() %>% 
#   findCorrelation()
# # Columnas fuera
# df[,cols] %>% names()
# 
# df <- df[,-cols]
# Debo cambiar la semilla, sino no encuentra solucion factible
# set.seed(64, sample.kind="Rounding")
# glm_fit <- train(covid_fallecidos ~ .+ offset(log(poblacion)), 
#                  data=df,
#                  method="glm.nb",
#                  na.action = na.omit)

# Debo cambiar la semilla, sino no encuentra solucion factible
# No convergio hasta 1890
# for (i in 1:10000){
#   cat(i, "\n")
#   set.seed(i, sample.kind="Rounding")
#   glm_fit <- tryCatch({
#     train(covid_fallecidos ~ .+ offset(log(poblacion)),
#                    data=df,
#                    method="glm.nb",
#                    na.action = na.omit)
#   }, error = function(cond) return(NULL))
#   if (!is.null(glm_fit)) break
# }

# glm_fit
# summary(glm_fit)

# eliminate variables with a low t-statistic
# rfe(df,df$tasa_mortalidad_covid, rfeControl=rfeControl(functions=lmFuncs))


### PRUEBAS OTROS MODELOS -------------------
file_name <- "Figuras/Analisis_transversal/Otros_Modelos/%s.png"
df <- df_modelo %>% 
  dplyr::select(nombre_comuna,region,nombre_provincia,zona, zona_termica,
                poblacion,tasa_mortalidad_covid,covid_fallecidos_65,
                covid_fallecidos,mp25,
                densidad_pob, `65+`, `15-44`, perc_puebloOrig, perc_rural,
                dias_primerContagio, dias_cuarentena,tasa_camas,
                perc_lenaCocina,
                ingresoTotal_media,perc_menor_media,
                perc_fonasa_A, perc_fonasa_D, perc_isapre,
                tmed_summer, tmed_winter, 
                heating_degree_15_summer, heating_degree_15_winter,
                heating_degree_18_summer, heating_degree_18_winter)


## Sin MP2.5---------
mod_sinMP <- glmer.nb(covid_fallecidos ~ 
                  scale(densidad_pob) + scale(`15-44`) + scale(`65+`) +
                  scale(perc_puebloOrig) + scale(perc_rural) +
                  scale(dias_primerContagio) +  scale(dias_cuarentena) + 
                  scale(tasa_camas) + 
                  scale(perc_lenaCocina) + 
                  scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                  scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                  scale(tmed_summer) + scale(tmed_winter) + 
                  scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                  (1|region)+
                  offset(log(poblacion)), 
                data = df,
                na.action=na.omit)
summary(mod_sinMP)
f_tableCoef(mod_sinMP)
f_tableMRR(mod_sinMP)
  # print(preview="pptx")
f_figMRR(mod_sinMP)
f_savePlot(last_plot(), sprintf(file_name,"sinMP"),dpi=150)

# Pq no converge?
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# Check singularity
tt <- getME(mod_sinMP,"theta")
ll <- getME(mod_sinMP,"lower")
min(tt[ll==0])
# Double-checking gradient calculations
derivs1 <- mod_sinMP@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
# restart (LO SOLUCIONA!)
ss <- getME(mod_sinMP,c("theta","fixef"))
m2 <- update(mod_sinMP,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(m2)
# try different optimizer  (LO SOUCIONA!)
m3 <- update(mod_sinMP,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
summary(m3)

rm(mod_sinMP)
## Sin RM---------
mod_sinRM <- glmer.nb(covid_fallecidos ~ 
                        mp25 +
                        scale(densidad_pob) + scale(`15-44`) + scale(`65+`) +
                        scale(perc_puebloOrig) + scale(perc_rural) +
                        scale(dias_primerContagio) +  scale(dias_cuarentena) + 
                        scale(tasa_camas) + 
                        scale(perc_lenaCocina) + 
                        scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                        scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                        scale(tmed_summer) + scale(tmed_winter) + 
                        scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                        (1|region)+
                        offset(log(poblacion)), 
                      data = df %>% filter(region!="M"),
                      na.action=na.omit)
summary(mod_sinRM)
exp(summary(mod_sinRM)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_sinRM)
f_tableMRR(mod_sinRM)
  # print(preview="pptx")
f_figMRR(mod_sinRM)
f_savePlot(last_plot(), sprintf(file_name,"sinRM"),dpi=150)

rm(mod_sinRM)

## Solo RM---------
mod_RM <- glm.nb(covid_fallecidos ~ 
                        mp25 +
                        scale(densidad_pob) + scale(`15-44`) + scale(`65+`) +
                        scale(perc_puebloOrig) + scale(perc_rural) +
                        scale(dias_primerContagio) +  scale(dias_cuarentena) + 
                        scale(tasa_camas) + 
                        scale(perc_lenaCocina) + 
                        scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                        scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                        scale(tmed_summer) + scale(tmed_winter) + 
                        scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                        offset(log(poblacion)), 
                      data = df %>% filter(region=="M"),
                      na.action=na.omit)
summary(mod_RM)
nobs(mod_RM)
exp(summary(mod_RM)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_RM)
f_tableMRR(mod_RM)
  # print(preview="pptx")
f_figMRR(mod_RM)
f_savePlot(last_plot(), sprintf(file_name,"soloRM"),dpi=150)
rm(mod_RM)


## Solo MP2.5---------
modMP <- glmer.nb(covid_fallecidos ~ mp25 +
                  (1|region)+
                    offset(log(poblacion)),
                  data = df,
                  na.action=na.omit)
summary(modMP)
exp(summary(modMP)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(modMP)
f_tableMRR(modMP)
f_figMRR(modMP)
f_savePlot(last_plot(), sprintf(file_name,"soloMP"),dpi=150)

## Random por Provincia---------
modProv <- glmer.nb(covid_fallecidos ~ 
                  mp25 +
                  scale(densidad_pob) + scale(`15-44`) + scale(`65+`) +
                  scale(perc_puebloOrig) + scale(perc_rural) +
                  scale(dias_primerContagio) +  scale(dias_cuarentena) + 
                  scale(tasa_camas) + 
                  scale(perc_lenaCocina) + 
                  scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                  scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                  scale(tmed_summer) + scale(tmed_winter) + 
                  scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                  (1|nombre_provincia)+
                  offset(log(poblacion)), 
                data = df,
                na.action=na.omit)

summary(modProv)
exp(summary(modProv)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(modProv)
f_tableMRR(modProv)
  # print(preview="pptx")
f_figMRR(modProv)
f_savePlot(last_plot(), sprintf(file_name,"randomProvincia"),dpi=150)


## Sin Random Intercept---------
mod_nb <- glm.nb(covid_fallecidos ~ 
                   mp25 +
                   scale(densidad_pob) + scale(`15-44`) + scale(`65+`) +
                   scale(perc_puebloOrig) + scale(perc_rural) +
                   scale(dias_primerContagio) +  scale(dias_cuarentena) + 
                   scale(tasa_camas) + 
                   scale(perc_lenaCocina) + 
                   scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(tmed_summer) + scale(tmed_winter) + 
                   scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                   offset(log(poblacion)), 
                 data = df,
                 na.action=na.omit)
summary(mod_nb)
nobs(mod_nb)
exp(summary(mod_nb)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_nb)
f_tableMRR(mod_nb) %>% 
  print(preview="pptx")
f_figMRR(mod_nb)
f_savePlot(last_plot(), sprintf(file_name,"sinRandomIntercept"),dpi=150)
rm(mod_nb)


## Fallecidos 65+---------
mod_65 <- glmer.nb(covid_fallecidos_65 ~ 
                        mp25 +
                        scale(densidad_pob) + 
                        scale(perc_puebloOrig) + scale(perc_rural) +
                        scale(dias_primerContagio) +  scale(dias_cuarentena) + 
                        scale(tasa_camas) + 
                        scale(perc_lenaCocina) + 
                        scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                        scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                        scale(tmed_summer) + scale(tmed_winter) + 
                        scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                        (1|region)+
                        offset(log(poblacion*`65+`)), 
                      data = df,
                      na.action=na.omit)
summary(mod_65)
exp(summary(mod_65)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_65)
f_tableMRR(mod_65)
  # print(preview="pptx")
f_figMRR(mod_65)
f_savePlot(last_plot(), sprintf(file_name,"fallecidos65"),dpi=150)


## Random Zonas---------
mod_zona <- glmer.nb(covid_fallecidos ~ 
                   mp25 +
                   scale(densidad_pob) + scale(`15-44`) + scale(`65+`) +
                   scale(perc_puebloOrig) + scale(perc_rural) +
                   scale(dias_primerContagio) +  scale(dias_cuarentena) + 
                   scale(tasa_camas) + 
                   scale(perc_lenaCocina) + 
                   scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(tmed_summer) + scale(tmed_winter) + 
                   scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                   (1|zona)+  
                   offset(log(poblacion)), 
                 data = df,
                 na.action=na.omit)
summary(mod_zona)
exp(summary(mod_zona)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
ranef(mod_zona)
f_tableCoef(mod_zona)
f_tableMRR(mod_zona)
  # print(preview="pptx")
f_figMRR(mod_zona)
f_savePlot(last_plot(), sprintf(file_name,"randomZonas"),dpi=150)
rm(mod_zona, df_zonas)


### Grafico MRR Resumen modelos probados ------------

# Por el momento los ingreso manualmente
df_mrr <- data.frame(
  method = c("Principal", "Sin RM", "Solo RM",
             "Random Provincia","Random Zona","Sin Random", 
             "Fallecidos 65+"),
  RR = c(0.98, 0.98, 0.95, 0.98, 0.97, 1.01, 0.98),
  lower_CI = c(0.95,0.94,0.87,0.95,0.95,0.98,0.95),
  upper_CI = c(1.01,1.01,1.04,1.02,1.00,1.04,1.01))

## Figure MRR
df_mrr %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=reorder(method,desc(rowid)), y=RR))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(x="",y="MRR")+
  coord_flip()
f_savePlot(last_plot(), sprintf(file_name,"resumenMRR"),dpi=150)

## EoF