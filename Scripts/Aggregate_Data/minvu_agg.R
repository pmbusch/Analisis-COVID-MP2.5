### Analisis-COVID-MP2.5
## Agrega infomacion de datos MINVU a nivel de comuna
## PBH Septiembre 2020


## HACINAMIENTO ---------------
# Carga datos brutos --------
source("Scripts/Load_Data/minvu_load.R", encoding = "UTF-8")


## Calculo porcentajes por comuna ---------------
df_minvu %>% names()
df_minvu <- df_minvu %>% 
  mutate(perc_vivSinHac=viv_sinHac_total/viv_total*100,
         perc_vivHacMedio=viv_HacMedio_total/viv_total*100,
         perc_vivHacCritico=viv_HacCritico_total/viv_total*100) %>% 
  select(codigo_comuna, perc_vivSinHac, perc_vivHacMedio, perc_vivHacCritico)


## PERMISOS EDIFICACION ----------------
# Sumo hasta el año 2017
df_permisos <- df_permisos %>% 
  filter(!(year %in% c("2018","2019"))) %>% 
  group_by(codigo_comuna) %>% 
  summarise(permisos=sum(permisos, na.rm = T)) %>% ungroup()
df_permisos$permisos %>% sum() ## Total viviendas


## Restar viviendas del censo, para obtener % construido antes del 2002
# source("Scripts/Aggregate_Data/censo_agg.R", encoding = "UTF-8")
df_permisos <- df_permisos %>% 
  left_join(df_censo %>% select(codigo_comuna, viviendas)) %>% 
  mutate(perc_vivAntes2002=(1-permisos/viviendas)*100) %>% 
  select(codigo_comuna, perc_vivAntes2002)

# Add to minvu
df_minvu <- df_minvu %>% left_join(df_permisos)
rm(df_permisos)

## EoF