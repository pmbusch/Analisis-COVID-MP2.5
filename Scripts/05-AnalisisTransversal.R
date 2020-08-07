### Analisis-COVID-MP2.5
## Analisis Transversal
## Fuente principal codigo: Exposure to air pollution and COVID-19 mortality in the United States A nationwide cross-sectional study
## https://github.com/wxwx1993/PM_COVID
## Fuente: Bates, D., Mächler, M., Bolker, B., & Walker, S. (2014). Fitting linear mixed-effects models using lme4. arXiv preprint arXiv:1406.5823.
## PBH Julio 2020

file_name <- "Figuras/Analisis_transversal/%s.png"

library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
theme_set(theme_bw())

## MODELO INICIAL-----

## Datos --------------
# Original
df <- df_modelo

## Modelo  -----------
## Poblacion va a off_set, dado que se estima la tasa
# Buena explicacion: https://stats.stackexchange.com/questions/11182/when-to-use-an-offset-in-a-poisson-regression
df %>% names()
# dimension
df %>% dplyr::select(casos_fallecidos, mp25, densidad_pob, ingresoAutonomo_media,
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

## Tabla coeficientes
est %>% 
  mutate(parametro=parametro %>% str_remove_all("scale|\\(|\\)|log")) %>% 
  flextable() %>% 
  colformat_num(big.mark=" ", digits=4, j=2:5,
                na_str="s/i") %>% 
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "left", part="all")
  # print(preview="pptx")

## MRR
est <- est %>% mutate(low=coef-1.96*sd, high=coef+1.96*sd)
est_mrr <- est[-1,] %>% mutate(coef=exp(coef) %>% round(2), 
                    low=exp(low) %>% round(2), 
                    high=exp(high) %>% round(2),
                    ci=paste("(",low,", ",high,")",sep = ""),
                    p_value=round(p_value,4)) %>% 
  dplyr::select(parametro, coef, ci, p_value)

est_mrr %>% 
  mutate(parametro=parametro %>% str_remove_all("scale|\\(|\\)|log")) %>% 
  rename(Variable=parametro, MRR=coef, `95% I.C.`=ci,`Valor-P`=p_value) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "left", part="all")
  # print(preview="pptx")


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


df %>% filter(!is.na(mp25)) %>% mutate(y=predict(mod,type="response")) %>% 
  dplyr::select(nombre_comuna,tasa_mortalidad,y,mp25,densidad_pob,
                perc_ocupado,penetracion_lena) %>% view()


# comparacion prediccion
df %>% filter(!is.na(mp25)) %>% 
ggplot(aes(x = mp25, y = tasa_mortalidad)) +
  geom_point(col="green",alpha=.5) +
  geom_line(aes(y = predict(mod,type="response")),size=1, col="red", alpha=.5)+
  labs(x="Concentración MP2.5 2016-2019 [ug/m3]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]")+
  theme_bw(16)


df %>% filter(!is.na(mp25)) %>% 
  ggplot(aes(x = tasa_mortalidad)) +
  geom_point(aes(y = predict(mod,type="response")), col="red", alpha=.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  labs(x="Observada", 
       y="Predicción")+
  theme_bw(16)

## Mapa comparativo
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")
df_map <- df %>%  filter(!is.na(mp25)) %>% 
  mutate(pred=predict(mod, type="response")) %>% 
  select(codigo_comuna, tasa_mortalidad, pred) %>% 
  rename(obs=tasa_mortalidad) %>% 
  right_join(mapa_comuna) %>% 
  gather(tipo,tasa,obs, pred)

df_map %>% 
  fig_mapaChile_facet(tasa, facets = ~tipo, limites=c(0,500),
                      titulo="Tasa Mortalidad COVID [muertes/100mil hab]")
f_savePlot(last_plot(), sprintf(file_name,"ModeloChile"))
rm(df_map)

## Comparacion con Poisson
# BinNeg vs Poisson
# https://stats.stackexchange.com/questions/311556/help-interpreting-count-data-glmm-using-lme4-glmer-and-glmer-nb-negative-binom
modelformula <- formula(casos_fallecidos ~ mp25 +
                          scale(densidad_pob) + 
                          scale(log(ingresoAutonomo_media)) + scale(perc_ocupado)+
                          scale(perc_menor_media) + scale(perc_isapre) +
                          scale(tasa_camas)+scale(penetracion_lena)+
                          scale(dias_cuarentena)+scale(dias_primerContagio)+
                          scale(`65+`)+
                          # scale(hr_summer)+scale(hr_winter)+
                          offset(log(poblacion)))
poismodel <- glm(modelformula, data = df, family = "poisson")   
nbmodel <- glm.nb(modelformula, data = df)
library(lmtest)
# Poisson vs Negative Binomial
lrtest(poismodel, nbmodel)
# Neg-Bin is a better fit

# Negative Binomial vs Mixed Model
lrtest(nbmodel, mod)
# Almost identical

rm(modelformula, poismodel, nbmodel)


## MODELO SOLO MP2.5---------
modMP <- glmer.nb(casos_fallecidos ~ mp25 +
                  (1|region)+
                    offset(log(poblacion)),
                  data = df)

summary(modMP)
exp(summary(modMP)[10]$coefficients[2,1]) # exponencial coeficiente MP2.5
