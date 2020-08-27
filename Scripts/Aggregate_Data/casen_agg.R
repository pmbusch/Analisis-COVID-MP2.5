### Analisis-COVID-MP2.5
## Agrega datos Casen a nivel comunal
## PBH Julio 2020

# Carga datos brutos --------
source("Scripts/Load_Data/casen_load.R", encoding = "UTF-8")

## Educacion -----------
## Agrupo para crear variable: Menor a eduacion media completa 
df_educacion$e6a %>% unique()
df_educacion <- df_educacion %>% 
  filter(e6a!=99) %>% # filtro respuesta no sabe
  mutate(menor_media=if_else(e6a<8,1,0)) %>% 
  group_by(codigo_comuna,menor_media) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% 
  filter(menor_media==1) %>% 
  select(codigo_comuna,perc) %>% 
  rename(perc_menor_media=perc)


## Salud prevision ---------
## Agrupo para crear variable: Porcentaje isapre
# lo hago distinto para incluir comunas sin nadie con isapre
df_prevision$s12 %>% unique()
df_prevision <-  df_prevision %>% 
  filter(s12!=99) %>% # filtro respuesta no sabe
  mutate(prev=case_when(
    s12==1 ~ "perc_fonasa_A",
    s12==2 ~ "perc_fonasa_B",
    s12==3 ~ "perc_fonasa_C",
    s12==4 ~ "perc_fonasa_D",
    s12==6 ~ "perc_FFAA",
    s12==7 ~ "perc_isapre",
    T~"otro")) %>% 
  group_by(codigo_comuna,prev) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% select(-hab) %>% 
  spread(prev, perc, fill=0) %>% select(-otro)


## Leña ---------
df_lena_casen <- df_lena_casen %>% 
  mutate(perc_lenaCocina=lena_cocina/hab*100,
         perc_lenaCalefaccion=lena_calefaccion/hab*100,
         perc_lenaAgua=lena_agua/hab*100) %>% 
  select(codigo_comuna, perc_lenaCocina, perc_lenaCalefaccion, perc_lenaAgua)



## AGRUPAR TODO -------
df_casen <- left_join(df_ingreso, 
                      df_ocupacion %>% select(codigo_comuna, perc_ocupado)) %>% 
  left_join(df_educacion) %>% 
  left_join(df_prevision) %>% 
  left_join(df_lena_casen) %>% 
  select(-nombre_comuna,-codigo_provincia,-nombre_provincia,
         -codigo_region,-nombre_region)


rm(df_codigoSalud, df_codigoEducacion, df_ingreso, df_prevision, df_educacion,
   df_ocupacion, df_lena_casen)


## EoF