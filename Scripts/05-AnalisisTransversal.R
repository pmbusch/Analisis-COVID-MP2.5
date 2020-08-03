### Analisis-COVID-MP2.5
## Analisis Transversal
## Fuente principal codigo: Exposure to air pollution and COVID-19 mortality in the United States A nationwide cross-sectional study
## https://github.com/wxwx1993/PM_COVID
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
theme_set(theme_bw())

## Modelo basico-----
library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)


df_modelo %>% names()
mod <- glmer.nb(tasa_mortalidad ~ mp25 +
                  scale(densidad_pob) + 
                  scale(log(ingresoAutonomo_media)) + scale(perc_ocupado)+
                  scale(perc_menor_media) + scale(perc_isapre) +
                  scale(tasa_camas)+scale(penetracion_lena)+
                  scale(dias_cuarentena)+scale(dias_primerContagio)+
                  scale(`65+`)+
                  scale(hr_summer)+scale(hr_winter)+
                  (1|region)+
                  offset(log(poblacion)), 
                data = df_modelo)

# Summary
summary(mod)


## Modelo solo MP2.5
modMP <- glmer.nb(tasa_mortalidad ~ mp25 +
                  (1|region)+
                    offset(log(poblacion)),
                  data = df_modelo)

summary(modMP)
