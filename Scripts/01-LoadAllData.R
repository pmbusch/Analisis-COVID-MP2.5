### Analisis-COVID-MP2.5
## Consolida toda la informacion cargada
## PBH Julio 2020
options(dplyr.summarise.inform=FALSE)

source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")


## Scripts para cargar datos
source("Scripts/Load Data/mapa_load.R", encoding = "UTF-8")
source("Scripts/Load Data/censo_load.R", encoding = "UTF-8")
source("Scripts/Load Data/casen_load.R", encoding = "UTF-8")
source("Scripts/Load Data/lena_load.R", encoding = "UTF-8")
source("Scripts/Load Data/tasaMortalidad_load.R", encoding = "UTF-8")
source("Scripts/Load Data/camas_load.R", encoding = "UTF-8")

## Data ya cargada (o bajada)
df_conc <- read_rds("Data/Data Modelo/Datos_Concentraciones.rsd")
df_meteo <- read_rds("Data/Data Modelo/Datos_Meteorologia.rsd")


## Data Covid -----------
source("Scripts/Load Data/covidMuertes_load.R", encoding = "UTF-8")
source("Scripts/Load Data/covidCasos_load.R", encoding = "UTF-8")
source("Scripts/Load Data/covidPCR_load.R", encoding = "UTF-8")
source("Scripts/Load Data/covidCuarentena_load.R", encoding = "UTF-8")


## Cargar nuevamente ------
# Solamente ejecutar para actualizar datos
source("Scripts/Load Data/sinca_scrap.R", encoding = "UTF-8") # SINCA
source("Scripts/Load Data/meteo_scrap.R", encoding = "UTF-8") # MeteoChile


## Join all data ----------------
# Todas las comunas se unen por "codigo_comuna"
df_comuna <- df_poblacion %>% 
  left_join(df_muertes %>% select(codigo_comuna, casos_fallecidos, tasa_mortalidad), 
            by=c("codigo_comuna")) %>% 
  left_join(df_conc, by=c("codigo_comuna")) %>% 
  left_join(df_camas %>% select(codigo_comuna, camas), by=c("codigo_comuna")) %>% 
  left_join(df_casos %>% select(codigo_comuna, casos_confirmados, tasa_contagios,
                                dia_contagioZero),by=c("codigo_comuna")) %>% 
  left_join(df_cuarentena, by=c("codigo_comuna")) %>% 
  left_join(df_meteo, by=c("codigo_comuna")) %>% 
  left_join(df_casen %>% select(-nombre_comuna,-codigo_provincia,
                                -nombre_provincia,-codigo_region,
                                -nombre_region), by=c("codigo_comuna")) %>% 
  left_join(df_censo %>% select(-nombre_comuna,-codigo_provincia,
                                -nombre_provincia,-codigo_region,
                                -nombre_region), by=c("codigo_comuna")) %>% 
  left_join(df_pcr %>% select(codigo_region, pcr_region), by=c("codigo_region")) %>% 
  left_join(df_lena, by=c("codigo_comuna")) %>% 
  left_join(df_tasaMortalidad %>% select(codigo_comuna, defunciones, 
                                         tasa_mortalidad) %>% 
              rename(tasa_mortalidad_all=tasa_mortalidad), by=c("codigo_comuna"))

df_comuna %>% skim()

# Sin NA
df_comuna_modelo <- df_comuna %>% filter(!is.na(mp25))
# df_comuna_modelo <- df_comuna %>% na.omit()

df_comuna_modelo %>% skim()

# No hay completitud en: fecha cuarentena, camas, meteorologia, perc_rural, tasa_mortalidad


## EoF