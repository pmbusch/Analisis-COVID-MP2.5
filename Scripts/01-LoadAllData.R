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


## Data ya cargada (o bajada)
df_conc <- read_delim("Data/Data Modelo/Datos_Concentraciones.csv", 
                      delim = ";", skip = 1, na = c("NA"),
                 col_types = "cddddcccccdddccc",
                 locale = locale(encoding = "windows-1252"))


df_meteo <- read_delim("Data/Data Modelo/Datos_Meteorologia.csv", 
                      delim = ";", skip = 1, na = c("NA"),
                      col_types = "dcdcccDc",
                      locale = locale(encoding = "windows-1252"))


## Cargar nuevamente ------
# Solamente ejecutar para actualizar datos
source("Scripts/Load Data/sinca_scrap.R", encoding = "UTF-8") # SINCA
source("Scripts/Load Data/meteo_scrap.R", encoding = "UTF-8") # MeteoChile

## EoF