### Analisis-COVID-MP2.5
## Carga Informacion a COVID-19 nuevamente (para actualizaciones mas rapidas)
## PBH Julio 2020

## Funciona en base a informacion comunal ya cargada en el WS

## Data Covid -----------
source("Scripts/Aggregate_Data/covidMuertes_agg.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/covidCasos_agg.R", encoding = "UTF-8")
source("Scripts/Load_Data/covidPCR_load.R", encoding = "UTF-8")
source("Scripts/Load_Data/covidCuarentena_load.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/covid_CFR.R", encoding = "UTF-8")


## Join data to existing DF --------
df_modelo <- df_modelo %>% 
  select(-covid_fallecidos,-tasa_mortalidad_covid,-dias_primerMuerte) %>% 
  left_join(df_muertes,by=c("codigo_comuna")) %>% 
  select(-casos_confirmados,-tasa_contagios,-dias_primerContagio) %>% 
  left_join(df_casos ,by=c("codigo_comuna")) %>% 
  left_join(df_cuarentena, by=c("codigo_comuna")) %>% 
  select(-pcr_region) %>% 
  left_join(df_pcr, by=c("codigo_region"))
rm(df_muertes, df_casos, df_cuarentena, df_pcr)

## Feature data nuevamente ---------
source("Scripts/02-FeatureData.R", encoding = "UTF-8")

## EoF