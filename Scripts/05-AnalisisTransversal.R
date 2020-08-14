### Analisis-COVID-MP2.5
## Analisis Transversal
## Fuente principal codigo: Exposure to air pollution and COVID-19 mortality in the United States A nationwide cross-sectional study
## https://github.com/wxwx1993/PM_COVID
## Fuente: Bates, D., Mächler, M., Bolker, B., & Walker, S. (2014). Fitting linear mixed-effects models using lme4. arXiv preprint arXiv:1406.5823.
## PBH Julio 2020

theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figuras/Analisis_transversal/%s.png"
source("Scripts/00-Funciones.R", encoding = "UTF-8")


library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)

# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
## Carga Datos a nivel de comuna-----

df_modelo %>% na.omit() %>% nrow() # dimension todas las variables


## MODELO INICIAL-----
# Varaibles se seleccionaron con ayuda de Step, pero con logica externa, 
# segun cuales podrian ser explicativas (y usadas en otros estudios)
df_modelo %>% names()
df <- df_modelo %>% 
  dplyr::select(nombre_comuna,region, poblacion,tasa_mortalidad,
                casos_fallecidos,mp25,
                densidad_pob, `65+`, `15-44`, perc_puebloOrig, perc_rural,
                dias_primerContagio, dias_cuarentena,tasa_camas,
                perc_lenaCocina,
                ingresoTotal_media,perc_menor_media,
                perc_fonasa_A, perc_fonasa_D, perc_isapre,
                tmed_summer, tmed_winter, 
                heating_degree_15_summer, heating_degree_15_winter,
                heating_degree_18_summer, heating_degree_18_winter)

df %>% na.omit() %>% dim() #dimension
df <- df %>% na.omit()

## Modelo  -----------
## Poblacion va a off_set, dado que se estima la tasa
# Buena explicacion: https://stats.stackexchange.com/questions/11182/when-to-use-an-offset-in-a-poisson-regression

mod <- glmer.nb(casos_fallecidos ~ 
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
# Summary
summary(mod)

## MRR para cada variable -------
# Se interpreta como el aumento relativo en la tasa de mortalidad covid por 1 ug/m3
# Dado que los confundentes estan estandarizados, su MRR se interepreta como 
# variacion relativa al aumento en 1 desviacion estandar de la variable confundente
# Fuente: https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
# est <- cbind(est=coef(mod), confint(mod))
# Coef of fixed effects
fixef(mod) %>% as.data.frame()
est <- summary(mod)$coefficients[,1:4] %>% as.data.frame() %>% 
  as_tibble(rownames = "parametro")
names(est) <- c("parametro","coef","sd","z_value","p_value")

## Add codes
foot_note <- "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"
est <- est %>% mutate(codes=case_when(
  p_value<0.001 ~ "***",
  p_value<0.01 ~ "**",
  p_value<0.05 ~ "*",
  p_value<0.1 ~ ".",
  T ~ ""))


## Tabla coeficientes
est %>% 
  mutate(parametro=parametro %>% 
           str_remove_all("scale|\\(|\\)|log") %>% 
           f_replaceVar()) %>% 
  rename(Parametro=parametro, `Coef.`=coef, `Desv.`=sd,
         `Valor-z`=z_value,`Valor-p`=p_value,`Sign.`=codes) %>% 
  flextable() %>% 
  colformat_num(big.mark=" ", digits=4, j=2:5,
                na_str="s/i") %>% 
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "left", part="all") %>% 
  footnote(j=6, value=as_paragraph(foot_note), part="header", inline=T)
  # print(preview="pptx")

## MRR
est <- est %>% mutate(low=coef-1.96*sd, high=coef+1.96*sd)
est_mrr <- est[-1,] %>% mutate(coef=exp(coef) %>% round(2), 
                    low=exp(low) %>% round(2), 
                    high=exp(high) %>% round(2),
                    ci=paste("(",low,", ",high,")",sep = ""),
                    p_value=round(p_value,4))

# Tabla MRR
est_mrr %>% 
  dplyr::select(parametro, coef, ci, p_value, codes) %>% 
  mutate(parametro=parametro %>% 
           str_remove_all("scale|\\(|\\)|log") %>% 
           f_replaceVar()) %>% 
  rename(Variable=parametro, MRR=coef, `95% I.C.`=ci,
         `Valor-p`=p_value,`Sign.`=codes) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "left", part="all") %>% 
  footnote(j=5, value=as_paragraph(foot_note), part="header", inline=T)
  # print(preview="pptx")

rm(foot_note)

## Figura MRR
est_mrr %>% 
  rowid_to_column() %>% 
  mutate(parametro=parametro %>% 
           str_remove_all("scale|\\(|\\)|log") %>% 
           f_replaceVar()) %>% 
  ggplot(aes(x=reorder(parametro,desc(rowid)), y=coef))+
  geom_point()+
  geom_errorbar(aes(ymin=low, ymax=high))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(x="",y="MRR")+
  coord_flip()+
  theme_bw(16)
f_savePlot(last_plot(), sprintf(file_name,"MRR"), dpi=100)

rm(est,est_mrr)


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

df %>% 
  mutate(y=predict(mod,type="response"),
         res=casos_fallecidos-y,
         residuals=residuals(mod, type="response")) %>% 
  dplyr::select(nombre_comuna,poblacion,casos_fallecidos,y,res,residuals,
                mp25,densidad_pob, tasa_mortalidad) %>% view()

# comparacion prediccion
df %>% 
  ggplot(aes(x = mp25, y = casos_fallecidos/poblacion*1e5)) +
  geom_point(col="green",alpha=.5) +
  geom_line(aes(y = predict(mod,type="response")/poblacion*1e5),
            size=1, col="red", alpha=.5)+
  labs(x="Concentración MP2.5 2016-2019 [ug/m3]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]")+
  theme_bw(16)




df %>% 
  mutate(rm=if_else(region=="M","RM","Resto Chile") %>% factor()) %>% 
  ggplot(aes(x = casos_fallecidos/poblacion*1e5)) +
  geom_point(aes(y = predict(mod,type="response")/poblacion*1e5),
             col="red", 
             size=2,
             alpha=.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  coord_cartesian(xlim=c(0,250),ylim=c(0,250), expand = T)+
  labs(x="Observada", 
       y="Predicción")+
  ggtitle("Tasa Mortalidad COVID [muertes/100mil hab]")+
  theme(panel.grid.minor = element_blank())


f_savePlot(last_plot(), sprintf(file_name,"Obs_vs_Pred"), dpi=100)

## Mapa comparativo
# detach("package:gamm4", unload = TRUE)
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")
df_map <- df %>%
  mutate(pred=predict(mod, type="response")/poblacion*1e5) %>% 
  left_join(codigos_territoriales) %>% 
  select(codigo_comuna, tasa_mortalidad, pred) %>% 
  rename(obs=tasa_mortalidad) %>% 
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
modelformula <- formula(casos_fallecidos ~ 
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
  dplyr::select(-casos_fallecidos, -poblacion,
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

glm_fit <- train(tasa_mortalidad ~ ., 
                 data=df,
                 # method="glm.nb",
                 method="glmStepAIC",
                 # method="glmnet",
                 # metric="RMSE",
                 na.action = na.omit)

glm_fit
summary(glm_fit)

## Otra prueba
df <-  df_modelo %>% 
  dplyr::select(-codigo_comuna, -nombre_comuna,-codigo_provincia,-nombre_provincia,
                -codigo_region,-nombre_region,-region,
                -tasa_mortalidad,-geometry,
                -tasa_contagios,-casos_confirmados,
                -pcr_region, -perc_letalidad,-defunciones,-tasa_mortalidad_all) %>% 
  na.omit()

# Debo cambiar la semilla, sino no encuentra solucion factible
set.seed(30, sample.kind="Rounding")
glm_fit <- train(casos_fallecidos ~ .+ offset(log(poblacion)), 
                 data=df,
                 method="glm.nb",
                 na.action = na.omit)

glm_fit
summary(glm_fit)


# cols <- df %>% 
#   cor() %>% 
#   findCorrelation()
# # Columnas fuera
# df[,cols] %>% names()
# 
# df <- df[,-cols]
# Debo cambiar la semilla, sino no encuentra solucion factible
# set.seed(64, sample.kind="Rounding")
# glm_fit <- train(casos_fallecidos ~ .+ offset(log(poblacion)), 
#                  data=df,
#                  method="glm.nb",
#                  na.action = na.omit)

# Debo cambiar la semilla, sino no encuentra solucion factible
# No convergio hasta 1890
# for (i in 1:10000){
#   cat(i, "\n")
#   set.seed(i, sample.kind="Rounding")
#   glm_fit <- tryCatch({
#     train(casos_fallecidos ~ .+ offset(log(poblacion)), 
#                    data=df,
#                    method="glm.nb",
#                    na.action = na.omit)
#   }, error = function(cond) return(NULL))
#   if (!is.null(glm_fit)) break
# }
# 
# glm_fit
# summary(glm_fit)



# eliminate variables with a low t-statistic
# rfe(df,df$tasa_mortalidad, rfeControl=rfeControl(functions=lmFuncs))





## MODELO SOLO MP2.5---------
modMP <- glmer.nb(casos_fallecidos ~ mp25 +
                  (1|region)+
                    offset(log(poblacion)),
                  data = df)

summary(modMP)
exp(summary(modMP)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5



