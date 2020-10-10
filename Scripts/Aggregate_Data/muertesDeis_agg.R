### Analisis-COVID-MP2.5
## Agregacion a nivel Comunal de Datos de muertes totales en Chile
## PBH Septiembre 2020

# Carga datos ---------
# source("Scripts/Load_Data/covidMuertes_load.R", encoding = "UTF-8")
# source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")

## Explore data -------
df_deis_total %>% names()
df_deis_total$year %>% table() # Periodo de interes: 2017-2019

df_deis_total <- df_deis_total %>% filter(year %in% c(2017:2019))
df_deis_total %>% group_by(cap_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis_total %>% group_by(glosa_cap_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis_total %>% group_by(grupo_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis_total %>% group_by(glosa_grupo_diag1) %>% summarise(count=n()) %>% arrange(desc(count))

df_deis_total %>% group_by(cap_diag2) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis_total %>% group_by(glosa_cap_diag2) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis_total %>% group_by(grupo_diag2) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis_total %>% group_by(glosa_grupo_diag2) %>% summarise(count=n()) %>% arrange(desc(count))


## Grupos de interes
## All: Todas las filas
## All causes: Causas de morbilidad y mortalidad. is.na(cap_diag2)
## Cardio: Enfermedades del sistema circulatorio. cap_diag1==I00-I99
## Pulmonar: Enfermedades del sistema respiratorio. cap_diag1==J00-J99
## Cardiopulmonar: Suma cardio+pulmonar
## Cancer: Tumores [Neoplasias]. cap_diag1==C00-D48
## External causes: Suplemento de all causes

## Agg data ----------
## Muertes totales
df_all <- df_deis_total %>% 
  group_by(codigo_comuna, year) %>% 
  summarise(def_total=n()) %>% ungroup() %>% 
  replace_na(list(def_total=0))
df_all$def_total %>% sum()

## All Causes
df_allCauses <- df_deis_total %>% 
  filter(is.na(cap_diag2)) %>% 
  group_by(codigo_comuna, year) %>% 
  summarise(def_allCauses=n()) %>% ungroup() %>% 
  replace_na(list(def_allCauses=0))
df_allCauses$def_allCauses %>% sum()

## Cardio
df_cardio <- df_deis_total %>% 
  filter(cap_diag1=="I00-I99") %>% 
  group_by(codigo_comuna, year) %>% 
  summarise(def_cardio=n()) %>% ungroup() %>% 
  replace_na(list(def_cardio=0))
df_cardio$def_cardio %>% sum()

## Pulmonar
df_pulmonar <- df_deis_total %>% 
  filter(cap_diag1=="J00-J99") %>% 
  group_by(codigo_comuna, year) %>% 
  summarise(def_pulmonar=n()) %>% ungroup() %>% 
  replace_na(list(def_pulmonar=0))
df_pulmonar$def_pulmonar %>% sum()

## CardioPulmonar
df_cardioPulmonar <- df_deis_total %>% 
  filter(cap_diag1 %in% c("J00-J99","I00-I99")) %>% 
  group_by(codigo_comuna, year) %>% 
  summarise(def_cardioPulmonar=n()) %>% ungroup() %>% 
  replace_na(list(def_cardioPulmonar=0))
df_cardioPulmonar$def_cardioPulmonar %>% sum()

## Cancer
df_cancer <- df_deis_total %>% 
  filter(cap_diag1 %in% c("C00-D48")) %>% 
  group_by(codigo_comuna, year) %>% 
  summarise(def_cancer=n()) %>% ungroup() %>% 
  replace_na(list(def_cancer=0))
df_cancer$def_cancer %>% sum()

## Externas
df_extCauses <- df_deis_total %>% 
  filter(!is.na(cap_diag2)) %>% 
  group_by(codigo_comuna, year) %>% 
  summarise(def_extCauses=n()) %>% ungroup() %>% 
  replace_na(list(def_extCauses=0))
df_extCauses$def_extCauses %>% sum()

## Join All -------
# Base codigo comuna y año
df_def <- expand.grid(codigo_comuna=unique(df_poblacion$codigo_comuna),
                      year=2017:2019)
df_def <- df_def %>% 
  left_join(df_all) %>% 
  left_join(df_allCauses) %>% 
  left_join(df_cardio) %>% 
  left_join(df_pulmonar) %>% 
  left_join(df_cardioPulmonar) %>% 
  left_join(df_cancer) %>% 
  left_join(df_extCauses)



## Expand year to columns ----------
# Gather first, then spread
df_def <- df_def %>% 
  gather(cause, def,-codigo_comuna,-year) %>% 
  mutate(key=paste(cause,year,sep="_") %>% str_remove("_999")) %>% 
  select(-cause, -year) %>% 
  spread(key, def)

## Add average of years, ignoring NA
df_def %>% names()
df_def <- df_def %>% 
  group_by(codigo_comuna) %>% 
  mutate(def_total=mean(c(def_total_2017, def_total_2018,
                            def_total_2019), na.rm=T) %>% round(0),
         def_allCauses=mean(c(def_allCauses_2017, def_allCauses_2018,
                              def_allCauses_2019), na.rm=T) %>% round(0),
         def_cardio=mean(c(def_cardio_2017, def_cardio_2018,
                              def_cardio_2019), na.rm=T) %>% round(0),
         def_pulmonar=mean(c(def_pulmonar_2017, def_pulmonar_2018,
                              def_pulmonar_2019), na.rm=T) %>% round(0),
         def_cardioPulmonar=mean(c(def_cardioPulmonar_2017, def_cardioPulmonar_2018,
                              def_cardioPulmonar_2019), na.rm=T) %>% round(0),
         def_cancer=mean(c(def_cancer_2017, def_cancer_2018,
                           def_cancer_2019), na.rm=T) %>% round(0),
         def_extCauses=mean(c(def_extCauses_2017, def_extCauses_2018,
                                   def_extCauses_2019), na.rm=T) %>% round(0)) %>% 
  ungroup()


# Clean WS
rm(df_all, df_allCauses, df_cardio,df_pulmonar, df_cardioPulmonar, df_deis_total,
   df_cancer,df_extCauses)


## EoF