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
source("Scripts/Load_Data/covid_Movilidad.R", encoding = "UTF-8") # test


## Data ya cargada (o bajada)
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones.rsd")
# df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_20km.rsd")
df_meteo <- read_rds("Data/Data_Modelo/Datos_Meteorologia.rsd")

## Data Covid -----------
# No agrego datos aca, los obtengo directo a nivel de comuna
source("Scripts/Aggregate_Data/covidMuertes_agg.R", encoding = "UTF-8")
source("Scripts/Load_Data/covidCasos_load.R", encoding = "UTF-8")
source("Scripts/Load_Data/covidPCR_load.R", encoding = "UTF-8")
source("Scripts/Load_Data/covidCuarentena_load.R", encoding = "UTF-8")


## Cargar nuevamente ------
# Solamente ejecutar para actualizar datos
# source("Scripts/Load Data/sinca_scrap.R", encoding = "UTF-8") # SINCA
# source("Scripts/Load Data/meteo_scrap.R", encoding = "UTF-8") # MeteoChile


## Join all data ----------------
# Todas las comunas se unen por "codigo_comuna"
df_modelo <- df_poblacion %>% 
  left_join(df_muertes,by=c("codigo_comuna")) %>% 
  left_join(df_conc, by=c("codigo_comuna")) %>% 
  left_join(df_camas, by=c("codigo_comuna")) %>% 
  left_join(df_casos ,by=c("codigo_comuna")) %>% 
  left_join(df_cuarentena, by=c("codigo_comuna")) %>% 
  left_join(df_meteo, by=c("codigo_comuna")) %>% 
  left_join(df_casen, by=c("codigo_comuna")) %>% 
  left_join(df_censo, by=c("codigo_comuna")) %>% 
  left_join(df_minvu, by=c("codigo_comuna")) %>% 
  left_join(df_pcr, by=c("codigo_region")) %>% 
  left_join(df_lena, by=c("codigo_comuna")) %>% 
  left_join(df_movilidad, by=c("codigo_comuna")) %>% 
  left_join(df_tasaMortalidad, by=c("codigo_comuna"))

rm(df_poblacion, df_muertes, df_conc, df_camas, df_casos, df_cuarentena,
   df_meteo, df_casen, df_censo,df_minvu, df_pcr, df_lena, df_tasaMortalidad,
   df_movilidad)

## EoF