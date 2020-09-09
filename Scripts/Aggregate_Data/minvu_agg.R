### Analisis-COVID-MP2.5
## Agrega infomacion de datos MINVU a nivel de comuna
## PBH Septiembre 2020

# Carga datos brutos --------
source("Scripts/Load_Data/minvu_load.R", encoding = "UTF-8")


## Calculo porcentajes por comuna ---------------
df_minvu %>% names()
df_minvu <- df_minvu %>% 
  mutate(perc_vivSinHac=viv_sinHac_total/viv_total*100,
         perc_vivHacMedio=viv_HacMedio_total/viv_total*100,
         perc_vivHacCritico=viv_HacCritico_total/viv_total*100) %>% 
  select(codigo_comuna, perc_vivSinHac, perc_vivHacMedio, perc_vivHacCritico)


## EoF