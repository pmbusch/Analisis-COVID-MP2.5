### Analisis-COVID-MP2.5
## Agrega infomacion del Censo a nivel comunal
## PBH Julio 2020

# Carga datos brutos --------
source("Scripts/Load_Data/censo_load.R", encoding = "UTF-8")

## Agrupo Todo ---------------
df_censo <- df_puebloOrig %>% 
  left_join(df_viviendas %>% select(codigo_comuna,viviendas)) %>% 
  left_join(df_rural %>% select(codigo_comuna, perc_rural)) %>% 
  left_join(df_material %>% select(codigo_comuna,perc_material_irrecuperable)) %>%
  select(codigo_comuna, perc_puebloOrig, viviendas, perc_rural, 
         perc_material_irrecuperable)
  
## Comunas con NA en porcentaje rural corresponde a O
df_censo <- df_censo %>% 
  mutate(perc_rural=if_else(is.na(perc_rural),0,perc_rural))

rm(df_puebloOrig,df_viviendas,df_material, df_rural)

## EoF