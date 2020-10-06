### Analisis-COVID-MP2.5
## Analisis Transversal Mortalidad todas las Causas
## PBH Septiembre 2020

## Librerias ------
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
# file_name <- "Figuras/Analisis_transversal/%s.png"
file_mod <- "Data/Data_Modelo/Modelos/%s.rsd"
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/05-FuncionesAnalisisTransversal.R", encoding = "UTF-8")

library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)

## Seleccion variables ---------
# file_name <- "Figuras/Analisis_transversal/Otros_Modelos/%s.png"
df_modelo %>% names()
df <- df_modelo %>% 
  dplyr::select(nombre_comuna, region,nombre_provincia,
                zona, zona_termica,rm, pda,
                poblacion,
                def_total, def_allCauses, def_cardio, def_pulmonar, def_cardioPulmonar,
                mp25,mp25_winter,
                densidad_pob, quintil_dens_pob,densidad_pob_censal,
                `65+`, `75+`, `15-44`, perc_puebloOrig, perc_rural,
                dias_primerContagio, dias_primerMuerte, dias_cuarentena,tasa_camas,
                perc_lenaCocina, perc_lenaCalefaccion,
                ingresoTotal_media, ingresoAutonomo_media,perc_menor_media,
                perc_fonasa_A, perc_fonasa_D, perc_isapre,
                tmed_summer, tmed_winter, hr_winter,tmed_anual,
                heating_degree_15_summer, heating_degree_15_winter,
                heating_degree_18_summer, heating_degree_18_winter,
                hdd15_winter_lenaCalefaccion, cons_lena_urbana,
                perc_salud,perc_vivAntes2002,perc_vivHacMedio,movilidad,hr_anual,
                cfr_0_20, cfr_raw_0,cfr_0_20_aplanados, cfr_raw_0_aplanados,
                cfr_raw_10)

## Modelo Base. Y= Causas Cardiopulmonares -------------
mod_nb <- glm.nb(def_cardioPulmonar ~ 
                   mp25 +
                   rm +
                   scale(densidad_pob_censal) +
                   scale(`15-44`) + scale(`65+`) +
                   scale(perc_puebloOrig) +
                   scale(perc_rural) +
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
f_tableCoef(mod_nb)
f_tableMRR(mod_nb)
f_figMRR(mod_nb)
rm(mod_nb)


## Modelo Base Sign. Y= Causas Pulmonares -------------
mod_nb_sign <- glm.nb(def_pulmonar ~ 
                   mp25 +
                   scale(`65+`) +
                   scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                   scale(perc_vivHacMedio)+
                   offset(log(poblacion)), 
                 data = df,
                 na.action=na.omit)

summary(mod_nb_sign)
nobs(mod_nb_sign)
f_tableCoef(mod_nb_sign)
f_tableMRR(mod_nb_sign)
f_figMRR(mod_nb_sign)
rm(mod_nb_sign)


## Modelo Step sobre Y= Causas Cardiopulmonares ------------
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
glm_fit <- train(def_cardioPulmonar~ . +offset(log(poblacion)),
                 data=df,
                 method="glmStepAIC",
                 family="poisson",
                 link="log",
                 na.action = na.omit)
glm_fit
summary(glm_fit)
varImp(glm_fit)

