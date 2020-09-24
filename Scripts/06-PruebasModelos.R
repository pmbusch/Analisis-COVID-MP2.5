### Analisis-COVID-MP2.5
## Pruebas Modelos: Analisis Transversal
## Modelo Binomial negativo con y sin efectos aleatorios
## PBH Septiembre 2020

# Load Data ------------
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/05-FuncionesAnalisisTransversal.R", encoding = "UTF-8")
load(".RData")
## df modelo es la tabla de datos con todo a nivel de comuna
## Columnas incluidas
df_modelo %>% names() %>% sort()

# Load library ----------
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)


## Modelo Prueba sin Efectos aleatorios--------
# Como voy a ajustar CFR como variable dependiente, debo crear un offset "auxiliar"
# de contagios
data_mod <- df_modelo %>% filter(cfr_raw_0_aplanados>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_raw_0_aplanados*100)
mod_nb <- glm.nb(covid_fallecidos ~ 
                         mp25 + rm + 
                         scale(densidad_pob) +
                         scale(`15-44`) + scale(`65+`) +
                         scale(perc_puebloOrig) + scale(perc_rural) +
                         scale(dias_primerMuerte) + 
                         scale(tasa_camas) + 
                         scale(perc_lenaCocina) + 
                         scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                         scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                         scale(tmed_summer) + scale(tmed_winter) + 
                         scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                         offset(log(contagios)),
                       data = data_mod,
                       na.action=na.omit)

# Analisis del modelo ajustado
summary(mod_nb) #Resumen genela
nobs(mod_nb) # Numero observaciones
f_tableCoef(mod_nb) # Tabla Coeficientes ajustados
f_tableMRR(mod_nb) # Tabla MRR
f_figMRR(mod_nb) # Figuras MRR

# Borra modelo ajustado
rm(mod_nb)

## Modelo Prueba con Efectos aleatorios (Region)--------
# En este ejemplo se ajusta la tasa de mortalidad (muertes/poblacion)
mod_nb_er <- glmer.nb(covid_fallecidos ~ 
                   mp25 +
                   scale(densidad_pob) +
                   scale(`15-44`) + scale(`65+`) +
                   scale(perc_puebloOrig) + scale(perc_rural) +
                   scale(dias_primerMuerte) + 
                   scale(tasa_camas) + 
                   scale(perc_lenaCocina) + 
                   scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(tmed_summer) + scale(tmed_winter) + 
                   scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                   (1|region)+
                   offset(log(poblacion)),
                 data = df_modelo,
                 na.action=na.omit)

# Analisis del modelo ajustado
summary(mod_nb_er) #Resumen genela
nobs(mod_nb_er) # Numero observaciones
ngrps(mod_nb_er) # number of levels of the Subject grouping factor
sigma(mod_nb_er) # residual standard deviation
ranef(mod_nb_er) # Coeficients of grouping variable, condicionados

f_tableCoef(mod_nb_er) # Tabla Coeficientes ajustados
f_tableMRR(mod_nb_er) # Tabla MRR
f_figMRR(mod_nb_er) # Figuras MRR

# Borra modelo ajustado
rm(mod_nb_er)

## EoF