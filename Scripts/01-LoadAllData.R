### Analisis-COVID-MP2.5
## Consolida toda la informacion cargada a nivel comunal
## PBH Julio 2020
options(dplyr.summarise.inform=FALSE)

source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")

## Scripts para cargar datos
#carga datos incluida en los scripts de agg
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/censo_agg.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/casen_agg.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/lena_agg.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/tasaMortalidad_agg.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/camas_agg.R", encoding = "UTF-8") 

## Data ya cargada (o bajada)
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones.rsd")
df_meteo <- read_rds("Data/Data_Modelo/Datos_Meteorologia.rsd")

## Data Covid -----------
# No agrego datos aca, los obtengo directo a nivel de comuna
source("Scripts/Load_Data/covidMuertes_load.R", encoding = "UTF-8")
source("Scripts/Load_Data/covidCasos_load.R", encoding = "UTF-8")
source("Scripts/Load_Data/covidPCR_load.R", encoding = "UTF-8")
source("Scripts/Load_Data/covidCuarentena_load.R", encoding = "UTF-8")


## Cargar nuevamente ------
# Solamente ejecutar para actualizar datos
# source("Scripts/Load Data/sinca_scrap.R", encoding = "UTF-8") # SINCA
# source("Scripts/Load Data/meteo_scrap.R", encoding = "UTF-8") # MeteoChile


## Join all data ----------------
# Todas las comunas se unen por "codigo_comuna"
df_comuna <- df_poblacion %>% 
  left_join(df_muertes,by=c("codigo_comuna")) %>% 
  left_join(df_conc, by=c("codigo_comuna")) %>% 
  left_join(df_camas, by=c("codigo_comuna")) %>% 
  left_join(df_casos ,by=c("codigo_comuna")) %>% 
  left_join(df_cuarentena, by=c("codigo_comuna")) %>% 
  left_join(df_meteo, by=c("codigo_comuna")) %>% 
  left_join(df_casen, by=c("codigo_comuna")) %>% 
  left_join(df_censo, by=c("codigo_comuna")) %>% 
  left_join(df_pcr, by=c("codigo_region")) %>% 
  left_join(df_lena, by=c("codigo_comuna")) %>% 
  left_join(df_tasaMortalidad, by=c("codigo_comuna"))

rm(df_poblacion, df_muertes, df_conc, df_camas, df_casos, df_cuarentena,
   df_meteo, df_casen, df_censo, df_pcr, df_lena, df_tasaMortalidad)

df_comuna %>% skim()

## Parametros -----------
# Superficie de m2 se pasa a km2
df_comuna <- df_comuna %>% 
  mutate(tasa_camas=camas/poblacion*1e5,
         dias_cuarentena=(fecha_muertes-fecha_cuarentena) %>% as.numeric(units="days"),
         densidad_pob=poblacion/superficie*1e6) %>% 
  select(-fecha_cuarentena, -camas)

# Sin NA
df_modelo <- df_comuna %>% filter(!is.na(mp25))
# df_comuna_modelo <- df_comuna %>% na.omit()


df_modelo %>% skim()
## Relleno NA ----------
# No hay completitud en: 
# dias cuarentena: No todas las comunas tienen cuaretena
# camas: No todas las comunas tienen camas? 
# meteorologia:
# tasa_mortalidadAll: no hubo muertes en el periodo temporal elegido
df_modelo <- df_modelo %>% 
  mutate(dias_cuarentena=if_else(is.na(dias_cuarentena),0, dias_cuarentena),
         tasa_camas=if_else(is.na(tasa_camas),0,tasa_camas),
         tasa_mortalidad_all=if_else(is.na(tasa_mortalidad_all),0,tasa_mortalidad_all))


## Guardar datos
cat('sep=; \n',file = "Data/Datos_Comuna.csv")
write.table(df_comuna,"Data/Datos_Comuna.csv",
            sep=';',row.names = F, append = T)

cat('sep=; \n',file = "Data/Datos_Modelo.csv")
write.table(df_modelo,"Data/Datos_Modelo.csv",
            sep=';',row.names = F, append = T)

## EoF