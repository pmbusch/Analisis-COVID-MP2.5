### Analisis-COVID-MP2.5
## Consolida toda la informacion cargada a nivel comunal
## PBH Julio 2020
options(dplyr.summarise.inform=FALSE)

source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")

## Scripts para cargar datos
#carga datos incluida en los scripts de agg
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/censo_agg.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/minvu_agg.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/casen_agg.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/lena_agg.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/tasaMortalidad_agg.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/camas_agg.R", encoding = "UTF-8") 
source("Scripts/Load_Data/incidencia_load.R", encoding = "UTF-8") # tasa morbilidad
source("Scripts/Load_Data/covid_Movilidad.R", encoding = "UTF-8") # test


## Data ya cargada (o bajada)
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones.rsd")
# df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_20km.rsd")
df_meteo <- read_rds("Data/Data_Modelo/Datos_Meteorologia.rsd")

## Data Covid -----------
source("Scripts/Aggregate_Data/covidMuertes_agg.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/covidCasos_agg.R", encoding = "UTF-8")
source("Scripts/Load_Data/covidPCR_load.R", encoding = "UTF-8")
source("Scripts/Load_Data/covidCuarentena_load.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/covid_CFR.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/muertesDeis_agg.R", encoding = "UTF-8")


## Join all data ----------------
# Todas las comunas se unen por "codigo_comuna"
df_modelo <- df_poblacion %>% 
  left_join(df_muertes,by=c("codigo_comuna")) %>% 
  left_join(df_conc, by=c("codigo_comuna")) %>% 
  left_join(df_camas, by=c("codigo_comuna")) %>% 
  left_join(df_casos ,by=c("codigo_comuna")) %>% 
  left_join(cfr_comunas ,by=c("codigo_comuna")) %>%
  left_join(df_cuarentena, by=c("codigo_comuna")) %>% 
  left_join(df_meteo, by=c("codigo_comuna")) %>% 
  left_join(df_casen, by=c("codigo_comuna")) %>% 
  left_join(df_censo, by=c("codigo_comuna")) %>% 
  left_join(df_minvu, by=c("codigo_comuna")) %>% 
  left_join(df_pcr, by=c("codigo_region")) %>% 
  left_join(df_lena, by=c("codigo_comuna")) %>% 
  left_join(df_lena_urbana, by=c("codigo_comuna")) %>% 
  left_join(df_movilidad, by=c("codigo_comuna")) %>% 
  left_join(df_tasaMortalidad, by=c("codigo_comuna")) %>% 
  left_join(df_def, by=c("codigo_comuna")) %>%
  left_join(df_incidencia, by=c("codigo_comuna"))

rm(df_poblacion, df_muertes, df_conc, df_camas, df_casos, df_cuarentena,
   df_meteo, df_casen, df_censo,df_minvu, df_pcr, df_pcr_tiempo, df_lena, df_tasaMortalidad,
   df_movilidad, df_grupoEdad, cfr_comunas, df_lena_urbana, df_def, df_incidencia)

## EoF