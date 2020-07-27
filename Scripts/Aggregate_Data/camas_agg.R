### Analisis-COVID-MP2.5
## Agrega datos de camas hospitalarias a nivel comunal
## PBH Julio 2020

# Carga datos brutos --------
source("Scripts/Load_Data/camas_load.R", encoding = "UTF-8")

## Agrupo por comuna ----------
df_camas <- df_camas %>% 
  group_by(codigo_comuna) %>% 
  summarise(camas=sum(total,na.rm=T)) %>% ungroup() %>% 
  left_join(codigos_territoriales) %>% 
  select(codigo_comuna,camas)

# Limpio WS
rm(df_camas_priv, df_camas_pub, df_establecimiento)

## EoF