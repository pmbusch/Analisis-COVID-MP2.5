### Analisis-COVID-MP2.5
## Analisis Transversal
## Fuente principal codigo: Exposure to air pollution and COVID-19 mortality in the United States A nationwide cross-sectional study
## https://github.com/wxwx1993/PM_COVID
## Fuente: Bates, D., Mächler, M., Bolker, B., & Walker, S. (2014). Fitting linear mixed-effects models using lme4. arXiv preprint arXiv:1406.5823.
## PBH Julio 2020

## Librerias ------
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figuras/Analisis_transversal/%s.png"
file_mod <- "Data/Data_Modelo/Modelos/%s.rsd"
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
                covid_fallecidos,mp25,densidad_pob_censal,
                densidad_pob, `65+`, `15-44`, perc_puebloOrig, perc_rural,
                dias_primerContagio, dias_cuarentena,tasa_camas,
                perc_lenaCocina,dias_primerMuerte,
                ingresoTotal_media,perc_menor_media,ingresoAutonomo_media,
                perc_fonasa_A, perc_fonasa_D, perc_isapre,
                tmed_summer, tmed_winter, perc_vivHacMedio,hr_anual,
                heating_degree_15_summer, heating_degree_15_winter,
                heating_degree_18_summer, heating_degree_18_winter)

df %>% na.omit() %>% dim() #dimension
# df <- df %>% na.omit()

## Poblacion va a off_set, dado que se estima la tasa
# Buena explicacion: https://stats.stackexchange.com/questions/11182/when-to-use-an-offset-in-a-poisson-regression
mod <- glmer.nb(covid_fallecidos ~ 
                  mp25 + 
                  scale(densidad_pob_censal) +
                  scale(`15-44`) + scale(`65+`) +
                  scale(perc_puebloOrig) + scale(perc_rural) +
                  scale(dias_primerMuerte) +
                  scale(tasa_camas) +
                  scale(perc_lenaCalefaccion) +
                  scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                  scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                  scale(perc_vivHacMedio)+
                  scale(hr_anual) +
                  scale(heating_degree_15_winter) +
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


saveRDS(mod, sprintf(file_mod,"original"))

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
                          scale(perc_lenaCalefaccion) + 
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
df_modelo %>% names() %>% sort()
df <-  df_modelo %>% select_if(is.numeric) %>% 
  dplyr::select(
    # -covid_fallecidos, -poblacion,
    -tasa_mortalidad_covid,
    -tasa_contagios,-casos_confirmados, -camas,
    -cfr_0_20,-cfr_0_20_aplanados,-cfr_0_30,-cfr_0_30_aplanados,
    -cfr_10_20,-cfr_10_20_aplanados,-cfr_raw_0,-cfr_raw_0_aplanados,
    -cfr_raw_10,-cfr_raw_10_aplanados,-cfr_raw_20,-cfr_raw_20_aplanados,
    -ifr_0_20,-ifr_0_20_aplanados,-ifr_0_30,-ifr_0_30_aplanados,-ifr_10_20,
    -ifr_10_20_aplanados,-ifr_raw_0,-ifr_raw_0_aplanados,-ifr_raw_10,
    -ifr_raw_10_aplanados,-ifr_raw_20,-ifr_raw_20_aplanados,
    -pcr_region, -perc_letalidad,-defunciones,-tasa_mortalidad_all,
    -cons_lena_calefactor_pp,-consumo_lena_m3,-consumo_lena_pp,-penetracion_lena,
    -cons_lena_cocina_pp,
    -superficie, -superficie_censal,-perimetro, -viviendas) %>% 
  na.omit()


# Columnas a remover dado que serian redundantes por su correlacion con otras variables
# identify and eliminate collinear variables
cols <- df %>% 
  cor() %>% 
  findCorrelation()
# Columnas fuera
df[,cols] %>% names() %>% sort()
## Keep covid_fallecidos and poblacion
# cols <- cols[cols!=1]
# Columnas remanentes
df[,-cols] %>% names() %>% sort()

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
varImp(glm_fit)

# NOTAS: Algoritmo step funciona con familia Poisson, y sin efectos aleatorios


# Poisson GLM peude dar el valor inicial del theta
# Theta is usually interpreted as a measure of overdispersion with respect to the Poisson distribution
# https://stats.stackexchange.com/questions/10419/what-is-theta-in-a-negative-binomial-regression-fitted-with-r

initial_mod <- glm.nb(covid_fallecidos~ . +offset(log(poblacion)),
                      data=df,
                      na.action=na.omit)
summary(initial_mod)
initial_mod$theta
# getME(glm_fit,"glmer.nb.theta")
glm_fit_nb <- train(covid_fallecidos~ . +offset(log(poblacion)),
                 data=df,
                 method="glmStepAIC",
                 preProcess = c("scale"),
                 # offset=(log(poblacion)),
                 family=negative.binomial(theta=initial_mod$theta,link="log"),
                 na.action = na.omit)
glm_fit_nb
summary(glm_fit_nb)
f_tableCoef(glm_fit_nb)
f_tableMRR(glm_fit_nb) 
# print(preview="pptx")
f_figMRR(glm_fit_nb)
f_savePlot(last_plot(), sprintf(file_name,"step"),dpi=150)
saveRDS(glm_fit_nb, sprintf(file_mod,"step"))

rm(glm_fit_nb, initial_mod)

rm(a)


mod <- glm.nb(covid_fallecidos~ scale(`15-44`) + scale(`45-64`) + scale(`65+`)+
                scale(densidad_pob_manzana_media) + scale(mp25_summer) + 
                scale(heating_degree_18_winter)  +
                scale(heating_degree_18_summer)  +
                scale(hr_anual) + scale(perc_menor_media) + 
                scale(ingresoAutonomo_media)+
                scale(perc_FFAA) + scale(perc_fonasa_A) + 
                scale(perc_fonasa_C) + 
                scale(perc_lenaCalefaccion) + scale(perc_lenaCalefaccion) + 
                scale(perc_lenaAgua) + 
                scale(perc_puebloOrig) + scale(perc_rural) +
                scale(densidad_pob) +
                scale(dias_primerMuerte) + scale(dias_primerContagio)+
                offset(log(poblacion)),
              data=df,
              init.theta=initial_mod$theta,
              na.action = na.omit)
summary(mod)
f_tableMRR(mod)

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
  dplyr::select(nombre_comuna, region,nombre_provincia,
                zona, zona_termica,rm, pda,
                poblacion,tasa_mortalidad_covid,covid_fallecidos_65,
                covid_fallecidos,casos_confirmados,
                mp25,mp25_winter,
                densidad_pob, quintil_dens_pob,densidad_pob_censal,
                `65+`, `75+`, `15-44`, perc_puebloOrig, perc_rural,
                dias_primerContagio, dias_primerMuerte, dias_cuarentena,tasa_camas,
                perc_lenaCocina, perc_lenaCalefaccion,
                ingresoTotal_media, ingresoAutonomo_media,perc_menor_media,
                perc_fonasa_A, perc_fonasa_D, perc_isapre,
                tmed_summer, tmed_winter, hr_winter,
                heating_degree_15_summer, heating_degree_15_winter,
                heating_degree_18_summer, heating_degree_18_winter,
                hdd15_winter_lenaCalefaccion, cons_lena_urbana,
                perc_salud,perc_vivAntes2002,perc_vivHacMedio,movilidad,hr_anual,
                cfr_0_20, cfr_raw_0,cfr_0_20_aplanados, cfr_raw_0_aplanados,
                cfr_raw_10)


## Sin MP2.5---------
mod_sinMP <- glmer.nb(covid_fallecidos ~ 
                        scale(densidad_pob_censal) +
                        scale(`15-44`) + scale(`65+`) +
                        scale(perc_puebloOrig) + scale(perc_rural) +
                        scale(dias_primerMuerte) +
                        scale(tasa_camas) +
                        scale(perc_lenaCalefaccion) +
                        scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                        scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                        scale(perc_vivHacMedio)+
                        scale(hr_anual) +
                        scale(heating_degree_15_winter) +
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
saveRDS(mod_sinMP, sprintf(file_mod,"sin_MP"))
rm(mod_sinMP)

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
## Sin RM---------
mod_sinRM <- glmer.nb(covid_fallecidos ~ 
                        mp25 +
                        scale(densidad_pob_censal) +
                        scale(`15-44`) + scale(`65+`) +
                        scale(perc_puebloOrig) + scale(perc_rural) +
                        scale(dias_primerMuerte) +
                        scale(tasa_camas) +
                        scale(perc_lenaCalefaccion) +
                        scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                        scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                        scale(perc_vivHacMedio)+
                        scale(hr_anual) +
                        scale(heating_degree_15_winter) +
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
saveRDS(mod_sinRM, sprintf(file_mod,"sin_RM"))
rm(mod_sinRM)

## Solo RM---------
mod_RM <- glm.nb(covid_fallecidos ~ 
                   mp25 +
                   scale(densidad_pob_censal) +
                   scale(`15-44`) + scale(`65+`) +
                   scale(perc_puebloOrig) + scale(perc_rural) +
                   scale(dias_primerMuerte) +
                   scale(tasa_camas) +
                   scale(perc_lenaCalefaccion) +
                   scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(perc_vivHacMedio)+
                   scale(hr_anual) +
                   scale(heating_degree_15_winter)+
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
saveRDS(mod_RM, sprintf(file_mod,"soloRM"))
rm(mod_RM)


## MP2.5 Winter---------
mod_mp25winter<- glmer.nb(covid_fallecidos ~ 
                          mp25_winter +
                          scale(densidad_pob_censal) +
                          scale(`15-44`) + scale(`65+`) +
                          scale(perc_puebloOrig) + scale(perc_rural) +
                          scale(dias_primerMuerte) +
                          scale(tasa_camas) +
                          scale(perc_lenaCalefaccion) +
                          scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                          scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                          scale(perc_vivHacMedio)+
                          scale(hr_anual) +
                          scale(heating_degree_15_winter) +
                          (1|region)+
                          offset(log(poblacion)), 
                      data = df,
                      na.action=na.omit)
summary(mod_mp25winter)
exp(summary(mod_mp25winter)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_mp25winter)
f_tableMRR(mod_mp25winter)
# print(preview="pptx")
f_figMRR(mod_mp25winter)
f_savePlot(last_plot(), sprintf(file_name,"MP25Winter"),dpi=150)
saveRDS(mod_mp25winter, sprintf(file_mod,"MP25Winter"))
rm(mod_mp25winter)

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
saveRDS(modMP, sprintf(file_mod,"soloMP"))
rm(modMP)

## Random por Provincia---------
modProv <- glmer.nb(covid_fallecidos ~ 
                      mp25 +
                      scale(densidad_pob_censal) +
                      scale(`15-44`) + scale(`65+`) +
                      scale(perc_puebloOrig) + scale(perc_rural) +
                      scale(dias_primerMuerte) +
                      scale(tasa_camas) +
                      scale(perc_lenaCalefaccion) +
                      scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                      scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                      scale(perc_vivHacMedio)+
                      scale(hr_anual) +
                      scale(heating_degree_15_winter) +
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
saveRDS(modProv, sprintf(file_mod,"randomProvincia"))
rm(modProv)


## Random Region---------
mod_region <- glmer.nb(covid_fallecidos ~ 
                       mp25 +
                       scale(densidad_pob_censal) +
                       scale(`15-44`) + scale(`65+`) +
                       scale(perc_puebloOrig) + scale(perc_rural) +
                       scale(dias_primerMuerte) +
                       scale(tasa_camas) +
                       scale(perc_lenaCalefaccion) +
                       scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                       scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                       scale(perc_vivHacMedio)+
                       scale(hr_anual) +
                       scale(heating_degree_15_winter) +
                       (1|zona)+
                       offset(log(poblacion)), 
                     data = df,
                     na.action=na.omit)
summary(mod_region)
exp(summary(mod_region)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
ranef(mod_region)
f_tableCoef(mod_region)
f_tableMRR(mod_region)
# print(preview="pptx")
f_figMRR(mod_region)
f_savePlot(last_plot(), sprintf(file_name,"randomRegion"),dpi=150)
saveRDS(mod_region, sprintf(file_mod,"randomRegion"))
rm(mod_region, df_zonas)


## Random Zonas---------
mod_zona <- glmer.nb(covid_fallecidos ~ 
                       mp25 +
                       scale(densidad_pob_censal) +
                       scale(`15-44`) + scale(`65+`) +
                       scale(perc_puebloOrig) + scale(perc_rural) +
                       scale(dias_primerMuerte) +
                       scale(tasa_camas) +
                       scale(perc_lenaCalefaccion) +
                       scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                       scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                       scale(perc_vivHacMedio)+
                       scale(hr_anual) +
                       scale(heating_degree_15_winter) +
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
saveRDS(mod_zona, sprintf(file_mod,"randomZonas"))
rm(mod_zona, df_zonas)

## Random Zona Termica---------
mod_zonaTermica <- glmer.nb(covid_fallecidos ~ 
                              mp25 +
                              scale(densidad_pob_censal) +
                              scale(`15-44`) + scale(`65+`) +
                              scale(perc_puebloOrig) + scale(perc_rural) +
                              scale(dias_primerMuerte) +
                              scale(tasa_camas) +
                              scale(perc_lenaCalefaccion) +
                              scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                              scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                              scale(perc_vivHacMedio)+
                              scale(hr_anual) +
                              scale(heating_degree_15_winter) +
                              (1|zona_termica)+
                              offset(log(poblacion)), 
                     data = df,
                     na.action=na.omit)
summary(mod_zonaTermica)
exp(summary(mod_zonaTermica)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
ranef(mod_zonaTermica)
f_tableCoef(mod_zonaTermica)
f_tableMRR(mod_zonaTermica)
# print(preview="pptx")
f_figMRR(mod_zonaTermica)
f_savePlot(last_plot(), sprintf(file_name,"randomZonaTermica"),dpi=150)
saveRDS(mod_zonaTermica, sprintf(file_mod,"randomZonaTermica"))
rm(mod_zonaTermica, df_zonas)

## Sin Random Intercept---------
mod_nb <- glm.nb(covid_fallecidos ~ 
                   mp25 +
                   rm +
                   scale(densidad_pob_censal) +
                   scale(`15-44`) + scale(`65+`) +
                   scale(perc_puebloOrig) +
                   scale(perc_rural) +
                   scale(dias_primerMuerte) +
                   scale(tasa_camas) +
                   scale(perc_lenaCalefaccion) +
                   scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(perc_vivHacMedio)+
                   scale(hr_anual) +
                   scale(heating_degree_15_winter) +
                   offset(log(poblacion)), 
                 data = df,
                 na.action=na.omit)

summary(mod_nb)
nobs(mod_nb)
exp(summary(mod_nb)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_nb)
f_tableMRR(mod_nb)
  # print(preview="docx")
  # print(preview="pptx")
f_figMRR(mod_nb)
f_savePlot(last_plot(), sprintf(file_name,"sinRandomIntercept"),dpi=150)
saveRDS(mod_nb, sprintf(file_mod,"sinRandomIntercept"))
rm(mod_nb)

## Sin Random Solo Significativas---------
mod_nb_sig <- glm.nb(covid_fallecidos ~ 
                       # mp25 +
                       rm +
                       scale(`15-44`) +
                       scale(perc_puebloOrig) + scale(perc_rural) +
                       scale(dias_primerMuerte) +
                       scale(tasa_camas) +
                       scale(perc_vivHacMedio)+
                       scale(hr_anual) +
                       scale(heating_degree_15_winter) +
                       offset(log(poblacion)), 
                 data = df,
                 na.action=na.omit)
summary(mod_nb_sig)
nobs(mod_nb_sig)
exp(summary(mod_nb_sig)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_nb_sig)
f_tableMRR(mod_nb_sig) 
  # print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_nb_sig)
f_savePlot(last_plot(), sprintf(file_name,"sinRandomInterceptSign"),dpi=150)
saveRDS(mod_nb_sig, sprintf(file_mod,"sinRandomInterceptSign"))
rm(mod_nb_sig)


## Modelo CR2---------
mod_nb_cr2 <- glm.nb(covid_fallecidos ~ 
                       mp25 + 
                       rm +
                       scale(`65+`) +
                       scale(perc_vivHacMedio) +
                       scale(perc_salud) +
                       scale(perc_vivAntes2002) +
                       scale(hr_anual) + 
                       scale(movilidad) +
                       offset(log(poblacion)), 
                     data = df,
                     na.action=na.omit)
summary(mod_nb_cr2)
nobs(mod_nb_cr2)
exp(summary(mod_nb_cr2)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_nb_cr2)
f_tableMRR(mod_nb_cr2)  
# print(preview="pptx")
f_figMRR(mod_nb_cr2)
f_savePlot(last_plot(), sprintf(file_name,"CR2"),dpi=150)
saveRDS(mod_nb_cr2, sprintf(file_mod,"CR2"))
rm(mod_nb_cr2)


## Modelo CR2 + Leña ---------
mod_nb_cr2_lena <- glm.nb(covid_fallecidos ~ 
                       mp25 +
                       rm +
                       scale(`65+`) +
                       scale(perc_vivHacMedio) +
                       scale(perc_salud) +
                       scale(perc_vivAntes2002) +
                       scale(hr_anual) + 
                       scale(movilidad) +
                       scale(hdd15_winter_lenaCalefaccion) +
                       offset(log(poblacion)), 
                     data = df,
                     na.action=na.omit)
summary(mod_nb_cr2_lena)
nobs(mod_nb_cr2_lena)
exp(summary(mod_nb_cr2_lena)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_nb_cr2_lena)
f_tableMRR(mod_nb_cr2_lena) %>% 
print(preview="pptx")
f_figMRR(mod_nb_cr2_lena)
f_savePlot(last_plot(), sprintf(file_name,"CR2_lena"),dpi=150)
saveRDS(mod_nb_cr2_lena, sprintf(file_mod,"CR2_lena"))
rm(mod_nb_cr2_lena)

## Modelo CR2 + Leña con letalidad acumulada ---------
mod_nb_cr2_lena_let <- glm.nb(covid_fallecidos ~ 
                            mp25 +
                            rm +
                            scale(`65+`) +
                            scale(perc_vivHacMedio) +
                            scale(perc_salud) +
                            scale(perc_vivAntes2002) +
                            scale(hr_anual) + 
                            scale(movilidad) +
                            scale(hdd15_winter_lenaCalefaccion) +
                            offset(log(casos_confirmados)), 
                          data = df,
                          na.action=na.omit)
summary(mod_nb_cr2_lena_let)
nobs(mod_nb_cr2_lena_let)
exp(summary(mod_nb_cr2_lena_let)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_nb_cr2_lena_let)
f_tableMRR(mod_nb_cr2_lena_let) %>% 
  print(preview="pptx")
f_figMRR(mod_nb_cr2_lena_let)
f_savePlot(last_plot(), sprintf(file_name,"CR2_lena_letalidad"),dpi=150)
saveRDS(mod_nb_cr2_lena_let, sprintf(file_mod,"CR2_lena_letalidads"))
rm(mod_nb_cr2_lena_let)


## Modelo CR2 + Leña con CFR ---------
data_mod <- df %>% filter(cfr_raw_0>0 & covid_fallecidos>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_raw_0*100)
mod_nb_cr2_lena_cfr <- glm.nb(covid_fallecidos ~ 
                            mp25 +
                            rm +
                            scale(`65+`) +
                            scale(perc_vivHacMedio) +
                            scale(perc_salud) +
                            scale(perc_vivAntes2002) +
                            scale(hr_anual) + 
                            scale(movilidad) +
                            scale(hdd15_winter_lenaCalefaccion) +
                            offset(log(poblacion)), 
                          data = data_mod,
                          na.action=na.omit)
summary(mod_nb_cr2_lena_cfr)
nobs(mod_nb_cr2_lena_cfr)
exp(summary(mod_nb_cr2_lena_cfr)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_nb_cr2_lena_cfr)
f_tableMRR(mod_nb_cr2_lena_cfr)
  # print(preview="pptx")
f_figMRR(mod_nb_cr2_lena_cfr)
f_savePlot(last_plot(), sprintf(file_name,"CR2_lena_cfr"),dpi=150)
saveRDS(mod_nb_cr2_lena_cfr, sprintf(file_mod,"CR2_lena_cfr"))
rm(mod_nb_cr2_lena_cfr,data_mod)


## Proxy Leña---------
mod_lena <- glm.nb(covid_fallecidos ~ 
                     mp25 + rm +
                     scale(densidad_pob_censal) +
                     scale(`15-44`) + scale(`65+`) +
                     scale(perc_puebloOrig) + scale(perc_rural) +
                     scale(dias_primerMuerte) +
                     scale(tasa_camas) +
                     scale(perc_lenaCalefaccion) +
                     scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                     scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                     scale(perc_vivHacMedio)+
                     scale(hr_anual) +
                     scale(heating_degree_15_winter) +
                     scale(hdd15_winter_lenaCalefaccion) +
                     offset(log(poblacion)),
                 data = df,
                 na.action=na.omit)
summary(mod_lena)
nobs(mod_lena)
exp(summary(mod_lena)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_lena)
f_tableMRR(mod_lena)
# print(preview="pptx")
f_figMRR(mod_lena)
f_savePlot(last_plot(), sprintf(file_name,"proxyLena"),dpi=150)
saveRDS(mod_lena, sprintf(file_mod,"proxyLena"))
rm(mod_lena)


## PDA ---------
mod_pda <- glm.nb(covid_fallecidos ~ 
                     mp25 +
                     rm + pda +
                     scale(densidad_pob_censal) +
                     scale(`15-44`) + scale(`65+`) +
                     scale(perc_puebloOrig) + scale(perc_rural) +
                     scale(dias_primerMuerte) +
                     scale(tasa_camas) +
                     scale(perc_lenaCalefaccion) +
                     scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                     scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                     scale(perc_vivHacMedio)+
                     scale(hr_anual) +
                     scale(heating_degree_15_winter) +
                     offset(log(poblacion)),
                   data = df,
                   na.action=na.omit)
summary(mod_pda)
nobs(mod_pda)
exp(summary(mod_pda)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_pda)
f_tableMRR(mod_pda)
# print(preview="pptx")
f_figMRR(mod_pda)
f_savePlot(last_plot(), sprintf(file_name,"pda"),dpi=150)
saveRDS(mod_pda, sprintf(file_mod,"pda"))
rm(mod_pda)

## Consumo Urbano Leña (n=45) ---------
mod_lenaUrbano <- glm.nb(covid_fallecidos ~ 
                    mp25 +
                    scale(`15-44`) +
                    scale(perc_puebloOrig) + scale(perc_rural) +
                    scale(dias_primerMuerte) +
                    scale(tasa_camas) +
                    scale(perc_vivHacMedio)+
                    # scale(perc_lenaCalefaccion)+
                    scale(hr_anual) +
                    scale(heating_degree_15_winter) +
                    scale(cons_lena_urbana) +
                    offset(log(poblacion)),
                  data = df,
                  na.action=na.omit)
summary(mod_lenaUrbano)
nobs(mod_lenaUrbano)
exp(summary(mod_lenaUrbano)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_lenaUrbano)
f_tableMRR(mod_lenaUrbano)
# print(preview="pptx")
f_figMRR(mod_lenaUrbano)
f_savePlot(last_plot(), sprintf(file_name,"lenaUrbano"),dpi=150)
saveRDS(mod_lenaUrbano, sprintf(file_mod,"lenaUrbano"))
rm(mod_lenaUrbano)


## Consumo Urbano Leña Letalidad ---------
mod_lenaUrbano_letal <- glm.nb(covid_fallecidos ~ 
                           mp25 +
                           scale(`15-44`) +
                           scale(perc_puebloOrig) + scale(perc_rural) +
                           scale(dias_primerMuerte) +
                           scale(tasa_camas) +
                           scale(perc_vivHacMedio)+
                           # scale(perc_lenaCalefaccion)+
                           scale(hr_anual) +
                           scale(heating_degree_15_winter) +
                           scale(cons_lena_urbana) +
                           offset(log(casos_confirmados)),
                         data = df,
                         na.action=na.omit)
summary(mod_lenaUrbano_letal)
nobs(mod_lenaUrbano_letal)
exp(summary(mod_lenaUrbano_letal)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_lenaUrbano_letal)
f_tableMRR(mod_lenaUrbano_letal)
# print(preview="pptx")
f_figMRR(mod_lenaUrbano_letal)
f_savePlot(last_plot(), sprintf(file_name,"lenaUrbano_letalidad"),dpi=150)
saveRDS(mod_lenaUrbano_letal, sprintf(file_mod,"lenaUrbano_letalidad"))
rm(mod_lenaUrbano_letal)

## MP2.5 2019 --------
mod_mp_2019 <- glm.nb(covid_fallecidos ~ 
                   mp25_2019 +
                   rm +
                   scale(densidad_pob_censal) +
                   scale(`15-44`) + scale(`65+`) +
                   scale(perc_puebloOrig) +
                   scale(perc_rural) +
                   scale(dias_primerMuerte) +
                   scale(tasa_camas) +
                   scale(perc_lenaCalefaccion) +
                   scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(perc_vivHacMedio)+
                   scale(hr_anual) +
                   scale(heating_degree_15_winter) +
                   offset(log(poblacion)), 
                 data = df,
                 na.action=na.omit)

summary(mod_mp_2019)
nobs(mod_mp_2019)
exp(summary(mod_mp_2019)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_mp_2019)
f_tableMRR(mod_mp_2019)
# print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_mp_2019)
f_savePlot(last_plot(), sprintf(file_name,"mp25_2019"),dpi=150)
saveRDS(mod_mp_2019, sprintf(file_mod,"mp25_2019"))
rm(mod_mp_2019)


## MP10 --------
mod_mp10 <- glm.nb(covid_fallecidos ~ 
                        mp10 +
                        rm +
                        scale(densidad_pob_censal) +
                        scale(`15-44`) + scale(`65+`) +
                        scale(perc_puebloOrig) +
                        scale(perc_rural) +
                        scale(dias_primerMuerte) +
                        scale(tasa_camas) +
                        scale(perc_lenaCalefaccion) +
                        scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                        scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                        scale(perc_vivHacMedio)+
                        scale(hr_anual) +
                        scale(heating_degree_15_winter) +
                        offset(log(poblacion)), 
                      data = df,
                      na.action=na.omit)

summary(mod_mp10)
nobs(mod_mp10)
exp(summary(mod_mp10)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_mp10)
f_tableMRR(mod_mp10)
# print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_mp10)
f_savePlot(last_plot(), sprintf(file_name,"mp10"),dpi=150)
saveRDS(mod_mp10, sprintf(file_mod,"mp10"))
rm(mod_mp10)

## Fallecidos 65+---------
mod_65 <- glmer.nb(covid_fallecidos_65 ~ 
                     mp25 +
                     scale(densidad_pob_censal) +
                     scale(`15-44`) + 
                     scale(perc_puebloOrig) + scale(perc_rural) +
                     scale(dias_primerMuerte) +
                     scale(tasa_camas) +
                     scale(perc_lenaCalefaccion) +
                     scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                     scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                     scale(perc_vivHacMedio)+
                     scale(hr_anual) +
                     scale(heating_degree_15_winter) +
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
saveRDS(mod_65, sprintf(file_mod,"fallecidos65"))
rm(mod_65)


## Y: % Letalidad acumulado---------
mod_letalidad <- glm.nb(covid_fallecidos ~ 
                          mp25 +
                          rm +
                          scale(densidad_pob_censal) +
                          scale(`15-44`) + scale(`65+`) +
                          scale(perc_puebloOrig) + scale(perc_rural) +
                          scale(dias_primerMuerte) +
                          scale(tasa_camas) +
                          scale(perc_lenaCalefaccion) +
                          scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                          scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                          scale(perc_vivHacMedio)+
                          scale(hr_anual) +
                          scale(heating_degree_15_winter) +
                          offset(log(casos_confirmados)),
                        data = df,
                        na.action=na.omit)
summary(mod_letalidad)
nobs(mod_letalidad)
exp(summary(mod_letalidad)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_letalidad)
f_tableMRR(mod_letalidad)
  # print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_letalidad)
f_savePlot(last_plot(), sprintf(file_name,"Y_letalidad"),dpi=150)
saveRDS(mod_letalidad, sprintf(file_mod,"Y_letalidad"))
rm(mod_letalidad)


## CFR Lag 0 Bruto---------
data_mod <- df %>% filter(cfr_raw_0>0 & covid_fallecidos>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_raw_0*100)
mod_cfr_raw_0 <- glm.nb(covid_fallecidos ~ 
                          mp25 + rm +
                          scale(densidad_pob_censal) +
                          scale(`15-44`) + scale(`65+`) +
                          scale(perc_puebloOrig) + scale(perc_rural) +
                          scale(dias_primerMuerte) +
                          scale(tasa_camas) +
                          scale(perc_lenaCalefaccion) +
                          scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                          scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                          scale(perc_vivHacMedio)+
                          scale(hr_anual) +
                          scale(heating_degree_15_winter) +
                         offset(log(contagios)),
                       data = data_mod,
                       na.action=na.omit)
summary(mod_cfr_raw_0)
nobs(mod_cfr_raw_0)
exp(summary(mod_cfr_raw_0)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_cfr_raw_0)
f_tableMRR(mod_cfr_raw_0) %>% 
  print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_cfr_raw_0)
f_savePlot(last_plot(), sprintf(file_name,"CFR_raw_0"),dpi=150)
saveRDS(mod_cfr_raw_0, sprintf(file_mod,"CFR_raw_0"))
rm(mod_cfr_raw_0, data_mod)


## CFR Lag 0-20---------
data_mod <- df %>% filter(cfr_0_20>0 & covid_fallecidos>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_0_20*100)
mod_cfr_0_20 <- glm.nb(covid_fallecidos ~ 
                         mp25 + rm +
                         scale(densidad_pob_censal) +
                         scale(`15-44`) + scale(`65+`) +
                         scale(perc_puebloOrig) + scale(perc_rural) +
                         scale(dias_primerMuerte) +
                         scale(tasa_camas) +
                         scale(perc_lenaCalefaccion) +
                         scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                         scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                         scale(perc_vivHacMedio)+
                         scale(hr_anual) +
                         scale(heating_degree_15_winter) +
                         offset(log(contagios)),
                       data = data_mod,
                       na.action=na.omit)
summary(mod_cfr_0_20)
nobs(mod_cfr_0_20)
exp(summary(mod_cfr_0_20)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_cfr_0_20)
f_tableMRR(mod_cfr_0_20) %>% 
  print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_cfr_0_20)
f_savePlot(last_plot(), sprintf(file_name,"CFR_0_20"),dpi=150)
saveRDS(mod_cfr_0_20, sprintf(file_mod,"CFR_0_20"))
rm(mod_cfr_0_20, data_mod)


## Y: % Letalidad acumulado significativo---------
# Significativo segun el modelo base de nb (sin random intercept)
mod_letalidad_sign <- glm.nb(covid_fallecidos ~ 
                               # mp25 +
                               rm +
                               scale(`15-44`) +
                               scale(perc_puebloOrig) + scale(perc_rural) +
                               scale(dias_primerMuerte) +
                               scale(tasa_camas) +
                               scale(perc_vivHacMedio)+
                               scale(hr_anual) +
                               scale(heating_degree_15_winter) +
                          offset(log(casos_confirmados)),
                        data = df,
                        na.action=na.omit)
summary(mod_letalidad_sign)
nobs(mod_letalidad_sign)
exp(summary(mod_letalidad_sign)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_letalidad_sign)
f_tableMRR(mod_letalidad_sign)
# print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_letalidad_sign)
f_savePlot(last_plot(), sprintf(file_name,"Y_letalidad_sign"),dpi=150)
saveRDS(mod_letalidad_sign, sprintf(file_mod,"Y_letalidad_sign"))
rm(mod_letalidad_sign)


## CFR Lag 0 Bruto significativo ---------
# Significativo segun el modelo base de nb (sin random intercept)
data_mod <- df %>% filter(cfr_raw_0>0 & covid_fallecidos>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_raw_0*100)
mod_cfr_raw_0_sign <- glm.nb(covid_fallecidos ~ 
                               # mp25 +
                               rm +
                               scale(`15-44`) +
                               scale(perc_puebloOrig) + scale(perc_rural) +
                               scale(dias_primerMuerte) +
                               scale(tasa_camas) +
                               scale(perc_vivHacMedio)+
                               scale(hr_anual) +
                               scale(heating_degree_15_winter) +
                          offset(log(contagios)),
                        data = data_mod,
                        na.action=na.omit)
summary(mod_cfr_raw_0_sign)
nobs(mod_cfr_raw_0_sign)
exp(summary(mod_cfr_raw_0_sign)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_cfr_raw_0_sign)
f_tableMRR(mod_cfr_raw_0_sign) %>% 
  print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_cfr_raw_0_sign)
f_savePlot(last_plot(), sprintf(file_name,"CFR_raw_0_sign"),dpi=150)
saveRDS(mod_cfr_raw_0_sign, sprintf(file_mod,"CFR_raw_0_sign"))
rm(mod_cfr_raw_0_sign, data_mod)


## CFR Lag 0-20 significativo ---------
# Significativo segun el modelo base de nb (sin random intercept)
data_mod <- df %>% filter(cfr_0_20>0 & covid_fallecidos>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_0_20*100)
mod_cfr_0_20_sign <- glm.nb(covid_fallecidos ~ 
                              # mp25 +
                              rm +
                              scale(`15-44`) +
                              scale(perc_puebloOrig) + scale(perc_rural) +
                              scale(dias_primerMuerte) +
                              scale(tasa_camas) +
                              scale(perc_vivHacMedio)+
                              scale(hr_anual) +
                              scale(heating_degree_15_winter) +
                         offset(log(contagios)),
                       data = data_mod,
                       na.action=na.omit)
summary(mod_cfr_0_20_sign)
nobs(mod_cfr_0_20_sign)
exp(summary(mod_cfr_0_20_sign)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_cfr_0_20_sign)
f_tableMRR(mod_cfr_0_20_sign) %>% 
  print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_cfr_0_20_sign)
f_savePlot(last_plot(), sprintf(file_name,"CFR_0_20_sign"),dpi=150)
saveRDS(mod_cfr_0_20_sign, sprintf(file_mod,"CFR_0_20_sign"))
rm(mod_cfr_0_20_sign, data_mod)



## Sin RM significativo---------
mod_sinRM_sign <- glm.nb(covid_fallecidos ~ 
                        mp25 +
                        scale(`15-44`) +
                        scale(perc_puebloOrig) + scale(perc_rural) +
                        scale(dias_primerMuerte) +
                        scale(tasa_camas) +
                        scale(perc_vivHacMedio)+
                        scale(hr_anual) +
                        scale(heating_degree_15_winter) +
                        offset(log(poblacion)), 
                      data = df %>% filter(region!="M"),
                      na.action=na.omit)
summary(mod_sinRM_sign)
nobs(mod_sinRM_sign)
exp(summary(mod_sinRM_sign)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_sinRM_sign)
f_tableMRR(mod_sinRM_sign) %>% 
  print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_sinRM_sign)
f_savePlot(last_plot(), sprintf(file_name,"sinRM_sign"),dpi=150)
saveRDS(mod_sinRM_sign, sprintf(file_mod,"sinRM_sign"))
rm(mod_sinRM_sign)

## Solo RM significativo---------
mod_RM_sign <- glm.nb(covid_fallecidos ~ 
                        mp25 +
                        scale(`15-44`) +
                        scale(perc_puebloOrig) + scale(perc_rural) +
                        scale(dias_primerMuerte) +
                        scale(tasa_camas) +
                        scale(perc_vivHacMedio)+
                        scale(hr_anual) +
                        scale(heating_degree_15_winter) +
                   offset(log(poblacion)),
                 data = df %>% filter(region=="M"),
                 na.action=na.omit)
summary(mod_RM_sign)
nobs(mod_RM_sign)
exp(summary(mod_RM_sign)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_RM_sign)
f_tableMRR(mod_RM_sign) %>% 
  print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_RM_sign)
f_savePlot(last_plot(), sprintf(file_name,"soloRM_sign"),dpi=150)
saveRDS(mod_RM_sign, sprintf(file_mod,"soloRM_sign"))
rm(mod_RM_sign)

## Sin RM Y: % Letalidad acumulado---------
mod_letalidad_sign_Sinrm <- glm.nb(covid_fallecidos ~ 
                               mp25 +
                               scale(`15-44`) +
                               scale(perc_puebloOrig) + scale(perc_rural) +
                               scale(dias_primerMuerte) +
                               scale(tasa_camas) +
                               scale(perc_vivHacMedio)+
                               scale(hr_anual) +
                               scale(heating_degree_15_winter) +
                               offset(log(casos_confirmados)),
                             data = df %>% filter(region!="M"),
                             na.action=na.omit)
summary(mod_letalidad_sign_Sinrm)
nobs(mod_letalidad_sign_Sinrm)
exp(summary(mod_letalidad_sign_Sinrm)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_letalidad_sign_Sinrm)
f_tableMRR(mod_letalidad_sign_Sinrm) %>% 
print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_letalidad_sign_Sinrm)
f_savePlot(last_plot(), sprintf(file_name,"Y_letalidad_sign_Sinrm"),dpi=150)
saveRDS(mod_letalidad_sign_Sinrm, sprintf(file_mod,"Y_letalidad_sign_Sinrm"))
rm(mod_letalidad_sign_Sinrm)

## Solo RM Y: % Letalidad acumulado---------
mod_letalidad_sign_rm <- glm.nb(covid_fallecidos ~ 
                               mp25 +
                               scale(`15-44`) +
                               scale(perc_puebloOrig) + scale(perc_rural) +
                               scale(dias_primerMuerte) +
                               scale(tasa_camas) +
                               scale(perc_vivHacMedio)+
                               scale(hr_anual) +
                               scale(heating_degree_15_winter) +
                               offset(log(casos_confirmados)),
                             data = df %>% filter(region=="M"),
                             na.action=na.omit)
summary(mod_letalidad_sign_rm)
nobs(mod_letalidad_sign_rm)
exp(summary(mod_letalidad_sign_rm)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_letalidad_sign_rm)
f_tableMRR(mod_letalidad_sign_rm) %>% 
print(preview="docx")
# print(preview="pptx")
f_figMRR(mod_letalidad_sign_rm)
f_savePlot(last_plot(), sprintf(file_name,"Y_letalidad_sign_rm"),dpi=150)
saveRDS(mod_letalidad_sign_rm, sprintf(file_mod,"Y_letalidad_sign_rm"))
rm(mod_letalidad_sign_rm)



### Grafico MRR Resumen modelos probados ------------

mod_nb <- read_rds(sprintf(file_mod,"sinRandomIntercept"))
mod_nb_sig <- read_rds(sprintf(file_mod,"sinRandomInterceptSign"))
mod_sinMP <- read_rds(sprintf(file_mod,"sin_MP"))
mod_sinRM <- read_rds(sprintf(file_mod,"sin_RM"))
mod_RM <- read_rds(sprintf(file_mod,"soloRM"))
mod_sinRM_sign <- read_rds(sprintf(file_mod,"sin_RM"))
mod_RM_sign <- read_rds(sprintf(file_mod,"sinRM_sign"))
mod_mp25winter <- read_rds(sprintf(file_mod,"soloRM_sign"))
modMP <- read_rds(sprintf(file_mod,"soloMP"))
mod_region <- read_rds(sprintf(file_mod,"randomRegion"))
modProv <- read_rds(sprintf(file_mod,"randomProvincia"))
mod_zona <- read_rds(sprintf(file_mod,"randomZonas"))
mod_zonaTermica <- read_rds(sprintf(file_mod,"randomZonaTermica"))
# mod_nb_cr2 <- read_rds(sprintf(file_mod,"CR2"))
# mod_nb_cr2_lena <- read_rds(sprintf(file_mod,"CR2_lena"))
# mod_nb_cr2_lena_cfr <- read_rds(sprintf(file_mod,"CR2_lena_cfr"))
mod_lena <- read_rds(sprintf(file_mod,"proxyLena"))
mod_65 <- read_rds(sprintf(file_mod,"fallecidos65"))
mod_cfr_0_20 <- read_rds(sprintf(file_mod,"CFR_0_20"))
mod_cfr_raw_0 <- read_rds(sprintf(file_mod,"CFR_raw_0"))
mod_letalidad <- read_rds(sprintf(file_mod,"Y_letalidad"))


## Cargo MRR del MP2.5 en mi Dataframe
df_mrr <- data.frame()
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Base"), f_MRR_mp25(mod_nb)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="MP2.5 Invierno"), 
                             f_MRR_mp25(mod_mp25winter,param="mp25_winter")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Sin RM"), 
                             f_MRR_mp25(mod_sinRM)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Solo RM"), 
                             f_MRR_mp25(mod_RM)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Sin RM Sign."), 
                             f_MRR_mp25(mod_sinRM_sign)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Solo RM Sign."), 
                             f_MRR_mp25(mod_RM_sign)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Region"), 
                             f_MRR_mp25(mod_region)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona"), 
                             f_MRR_mp25(mod_zona)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona Termica"), 
                             f_MRR_mp25(mod_zonaTermica)))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="Proxy Leña"), 
#                              f_MRR_mp25(mod_lena)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Fallecidos 65+"), 
                             f_MRR_mp25(mod_65)))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="CR2"), 
#                              f_MRR_mp25(mod_nb_cr2)))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="CR2 Leña"), 
#                              f_MRR_mp25(mod_nb_cr2_lena)))


## Figure MRR
df_mrr %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=reorder(method,desc(rowid)), y=RR))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(x="",y="MRR")+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  coord_flip()
f_savePlot(last_plot(), sprintf(file_name,"resumenMRR"),dpi=150)


## Figura resumen HDD 15° C Invierno ----
## Cargo MRR del MP2.5 en mi Dataframe
df_mrr <- data.frame()
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Base"), 
                             f_MRR_mp25(mod_nb, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Base Sign."), 
                             f_MRR_mp25(mod_nb_sig, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="MP2.5 Invierno"), 
                             f_MRR_mp25(mod_mp25winter,"scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Sin RM"), 
                             f_MRR_mp25(mod_sinRM, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Solo RM"), 
                             f_MRR_mp25(mod_RM, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Sin RM Sign."), 
                             f_MRR_mp25(mod_sinRM_sign, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Solo RM Sign."), 
                             f_MRR_mp25(mod_RM_sign, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Region"), 
                             f_MRR_mp25(mod_region, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona"), 
                             f_MRR_mp25(mod_zona, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona Termica"), 
                             f_MRR_mp25(mod_zonaTermica, "scale(heating_degree_15_winter)")))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="Proxy Leña"), 
#                              f_MRR_mp25(mod_lena)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Fallecidos 65+"), 
                             f_MRR_mp25(mod_65, "scale(heating_degree_15_winter)")))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="CR2"), 
#                              f_MRR_mp25(mod_nb_cr2)))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="CR2 Leña"), 
#                              f_MRR_mp25(mod_nb_cr2_lena)))

## Figure MRR
df_mrr %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=reorder(method,desc(rowid)), y=RR))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(x="",y="MRR")+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  coord_flip()
f_savePlot(last_plot(), sprintf(file_name,"resumenMRR_hdd15winter"),dpi=150)


## EoF