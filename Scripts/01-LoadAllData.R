### Analisis-COVID-MP2.5
## Consolida toda la informacion cargada
## PBH Julio 2020


source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")

## Scripts para cargar datos
source("Scripts/Load Data/poblacion_load.R", encoding = "UTF-8")

## Data ya cargada
df_conc <- read_delim("Data/Data Modelo/Datos_Concentraciones.csv", 
                      delim = ";", skip = 1, na = c("NA"),
                 col_types = "cddddcccccdddccc",
                 locale = locale(encoding = "windows-1252"))

## Cargar nuevamente
source("Scripts/Load Data/sinca_scrap.R", encoding = "UTF-8") # SINCA


## EoF