### Analisis-COVID-MP2.5
## Agregacion a nivel Comunal de Datos de muertes COVID
## Dato de Muertes COVID proviene del DEIS
## PBH Julio 2020

# Carga datos ---------
source("Scripts/Load_Data/covidMuertes_load.R", encoding = "UTF-8")
# source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")


## Compilo Muertes desde el DEIS -----------
df_muertes <- df_deis %>% 
  group_by(codigo_comuna, tipo) %>% 
  summarise(casos_fallecidos=n()) %>% 
  spread(tipo, casos_fallecidos) %>% 
  replace_na(list(confirmado=0, sospechoso=0)) %>% 
  rename(covid_fallecidos=confirmado, covid_fallecidos_sospechoso=sospechoso) %>% 
  mutate(fecha=fecha_deis %>% strptime("%d-%m-%Y") %>% as_date())

# Add Poblacion -----
df_muertes <- df_muertes %>%
  left_join(df_poblacion %>% select(codigo_comuna,poblacion))
df_muertes$poblacion %>% sum()

## Tasa Mortalidad --------
df_muertes <- df_muertes %>% 
  mutate(tasa_mortalidad_covid=covid_fallecidos/poblacion*1e5)

## Dias primer muerte confirmada DEIS ------ 
df_muerteZero <- df_deis %>% 
  filter(tipo=="confirmado") %>% 
  group_by(codigo_comuna, comuna) %>% 
  summarise(dia_muerteZero=min(date, na.rm=T)) %>% ungroup()

# Agrego este dato a df de muertes
df_muertes <- df_muertes %>% 
  left_join(df_muerteZero %>% select(-comuna)) %>% 
  mutate(dias_primerMuerte=(fecha-dia_muerteZero) %>% 
           as.numeric(units="days")) %>% 
  select(codigo_comuna, covid_fallecidos, tasa_mortalidad_covid,
         dias_primerMuerte)
rm(df_muerteZero)


## Muertes COVID DEIS ------------
## Muertes grupo etario 65
df_65 <- df_deis %>% 
  group_by(codigo_comuna,grupo_edad,tipo) %>% 
  summarise(covid_fallecidos_deis=n()) %>% ungroup()

df_65 <- df_65 %>% 
  filter(tipo=="confirmado" & grupo_edad=="65+") %>%
  select(-tipo, -grupo_edad) %>% 
  mutate(covid_fallecidos_65=as.numeric(covid_fallecidos_deis),
         covid_fallecidos_deis=NULL)

# Add to df_muertes
df_muertes <- df_muertes %>% 
  left_join(df_65, by=c("codigo_comuna"))
rm(df_65)

## Muertes grupo etario 75
df_75 <- df_deis %>% 
  filter(edad>=75) %>% 
  group_by(codigo_comuna,tipo) %>% 
  summarise(covid_fallecidos_deis=n()) %>% ungroup()

df_75 <- df_75 %>% 
  filter(tipo=="confirmado") %>% select(-tipo) %>% 
  mutate(covid_fallecidos_75=as.numeric(covid_fallecidos_deis),
         covid_fallecidos_deis=NULL)

# Add to df_muertes
df_muertes <- df_muertes %>% 
  left_join(df_75, by=c("codigo_comuna"))
rm(df_75)

## Replance NA in deaths
df_muertes <- df_muertes %>% 
  replace_na(list(covid_fallecidos_65=0, covid_fallecidos_75=0)) 


## Serie Temporal DEIS ------------
## Cumulate over Date
df_deis %>% names()
df_deis_tiempo <- df_deis %>%
  group_by(codigo_comuna,grupo_edad,sexo, tipo,date) %>%
  summarise(muertes=n()) %>% spread(tipo,muertes,fill = 0) %>%
  rename(muertes=confirmado, muertes_sospechoso=sospechoso)

df_deis_tiempo <- df_deis_tiempo %>%
  arrange(codigo_comuna,grupo_edad,sexo, date) %>%
  mutate(muertes_acc=cumsum(muertes),
         muertes_sospechoso_acc=cumsum(muertes_sospechoso)) %>% ungroup() %>%
  select(codigo_comuna,grupo_edad,sexo,date,muertes,muertes_acc,
         muertes_sospechoso,muertes_sospechoso_acc)

# Chequeo totales (fechas mas reciente)
df_aux <- df_deis_tiempo %>%
  group_by(codigo_comuna, grupo_edad,sexo) %>%
  mutate(fecha_muertes=max(date,na.rm=T),
         ultima=fecha_muertes==date) %>%
  filter(ultima==1)
df_aux$muertes_acc %>% sum(na.rm=T)
df_aux$muertes_sospechoso_acc %>% sum(na.rm=T)
df_deis %>% group_by(tipo) %>% summarise(count=n()) %>% arrange(desc(count))
rm(df_aux)

## Fill values on missing dates
# https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5
library(zoo)
df_deis_tiempo <- df_deis_tiempo %>%
  group_by(codigo_comuna, grupo_edad, sexo) %>%
  complete(date = seq.Date(min(df_deis_tiempo$date), ## Completa con todos los dias en mis fechas limites
                           max(df_deis_tiempo$date),
                           by = "day"),codigo_comuna) %>%
  arrange(codigo_comuna,grupo_edad,sexo, date) %>%
  fill(muertes_acc, muertes_sospechoso_acc) %>%  ## Completo con el acumulado anterior
  replace_na(list(muertes=0,muertes_sospechoso=0, ## NA es valor cero
                  muertes_acc=0,muertes_sospechoso_acc=0))

## Add region
df_deis_tiempo <- df_deis_tiempo %>%
  left_join(codigos_territoriales, by=c("codigo_comuna"))

## Add pob
df_deis_tiempo <- df_deis_tiempo %>%
  left_join(df_grupoEdad)
# rm(df_grupoEdad)

df_deis_tiempo <- df_deis_tiempo %>% 
  mutate(sexo=if_else(sexo %in% c("Mujer","mujer"),"mujer","hombre"))

## EoF