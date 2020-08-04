### Analisis-COVID-MP2.5
## Analisis Transversal
## Fuente principal codigo: Exposure to air pollution and COVID-19 mortality in the United States A nationwide cross-sectional study
## https://github.com/wxwx1993/PM_COVID
## Fuente: Bates, D., Mächler, M., Bolker, B., & Walker, S. (2014). Fitting linear mixed-effects models using lme4. arXiv preprint arXiv:1406.5823.
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
theme_set(theme_bw())

## Modelo basico-----
library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)

## Datos --------------
# Original
df <- df_modelo
# datos completados MP2.5
df <- df_modelo %>% left_join(df_avg %>% dplyr::select(codigo_comuna,avg)) %>% 
  select(-mp25) %>% rename(mp25=avg)


## Modelo Original -----------
## Poblacion va a off_set, dado que se estima la tasa
# Buena explicacion: https://stats.stackexchange.com/questions/11182/when-to-use-an-offset-in-a-poisson-regression
df %>% names()
# dimension
df %>% select(casos_fallecidos, mp25, densidad_pob, ingresoAutonomo_media,
              perc_ocupado, perc_menor_media, perc_isapre, tasa_camas,
              penetracion_lena, dias_cuarentena, dias_primerContagio,
              `65+`) %>% na.omit() %>% dim()

mod <- glmer.nb(casos_fallecidos ~ mp25 +
                  scale(densidad_pob) + 
                  scale(log(ingresoAutonomo_media)) + scale(perc_ocupado)+
                  scale(perc_menor_media) + scale(perc_isapre) +
                  scale(tasa_camas)+scale(penetracion_lena)+
                  scale(dias_cuarentena)+scale(dias_primerContagio)+
                  scale(`65+`)+
                  # scale(hr_summer)+scale(hr_winter)+
                  (1|region)+
                  offset(log(poblacion)), 
                data = df)

# Summary
summary(mod)
# Calculo MRR: Mortality rate ratios
# Se interpreta como el aumento relativo en la tasa de mortalidad covid por 1 ug/m3
exp(summary(mod)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
# I.C. M2.5
exp(summary(mod)[10]$coefficients[2,1] - 1.96*summary(mod)[10]$coefficients[2,2])
exp(summary(mod)[10]$coefficients[2,1] + 1.96*summary(mod)[10]$coefficients[2,2])
summary(mod)[10]$coefficients[2,4] # p-value MP2.5

## MRR para cada variable
# Dado que los confundentes estan estandarizados, su MRR se interepreta como 
# variacion relativa al aumento en 1 desviacion estandar de la variable confundente
# Fuente: https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
# est <- cbind(est=coef(mod), confint(mod))
# Coef of fixed effects
fixef(mod) %>% as.data.frame()

est <- summary(mod)$coefficients[,1:4] %>% as.data.frame() %>% 
  as_tibble(rownames = "parametro")
names(est) <- c("parametro","coef","sd","z_value","p_value")
est <- est %>% mutate(low=coef-1.96*sd, high=coef+1.96*sd)

## MRR
est %>% mutate(coef=exp(coef), low=exp(low), high=exp(high)) %>% 
  dplyr::select(parametro, coef, low, high)


## Varianza random effects -----
#  Si es muy cercana a cero un modelo si en este efecto es casi igual
print(vc <- VarCorr(mod), comp = c("Variance","Std.Dev."))
nobs(mod) # number of rows into model
ngrps(mod) # number of levels of the Subject grouping factor
sigma(mod) # residual standard deviation
formula(mod)
# Coeficients of grouping variable
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
df1 <- df %>% filter(!is.na(mp25)) %>% 
  mutate(prediccion=predict(mod,type="response"),
         residual=tasa_mortalidad-prediccion)

df1 %>% select(nombre_comuna, mp25, casos_fallecidos, poblacion, tasa_mortalidad,
               prediccion, residual) %>% view()


## Mapa comparativo
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")
p1 <- df1 %>% select(codigo_comuna, tasa_mortalidad, prediccion) %>% 
  right_join(mapa_comuna) %>% 
  gather(tipo, valor, tasa_mortalidad, prediccion) %>% 
  filter(tipo=="tasa_mortalidad") %>% 
  fig_mapaChile_facet(valor, limites=c(0,500))

p2 <- df1 %>% select(codigo_comuna, tasa_mortalidad, prediccion) %>% 
  right_join(mapa_comuna) %>% 
  gather(tipo, valor, tasa_mortalidad, prediccion) %>% 
  filter(tipo=="prediccion") %>% 
  fig_mapaChile_facet(valor,limites=c(0,500))

p1|p2




## Modelo solo MP2.5 ---------
modMP <- glmer.nb(casos_fallecidos ~ mp25 +
                  (1|region)+
                    offset(log(poblacion)),
                  data = df)

summary(modMP)
exp(summary(modMP)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
