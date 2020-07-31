### Analisis-COVID-MP2.5
## CASEN: Analisis datos
## PBH Julio 2020

theme_set(theme_bw())
file_name <- "Scripts/Analisis_Exploratorios/Figuras/casen/%s.png"
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")

# Carga datos brutos y Mapa --------
source("Scripts/Load_Data/casen_load.R", encoding = "UTF-8") 


## Mapa Mediana Ingresos --------
# Chile
df_ingreso %>% 
  right_join(mapa_comuna) %>% 
  fig_mapa(ingresoAutonomo_mediana, lwd=0.01, limites=c(0,1.2*1e6),
           titulo="Mediana Ingreso autonomo [CLP]",
           fileName = sprintf(file_name,"MapaChileIngresos"))
# Santiago
df_ingreso %>% 
  right_join(mapa_comuna) %>% 
  filter(codigo_provincia=="131" & codigo_comuna!="13115") %>% 
  fig_mapa(ingresoAutonomo_mediana, lwd=0.01, limites=c(0,1.2*1e6),
           titulo="Mediana Ingreso autonomo [CLP]",
           fileName = sprintf(file_name,"MapaSantiagoIngresos"))


## EoF