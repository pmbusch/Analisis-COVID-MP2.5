### Analisis-COVID-MP2.5
## Agregacion a nivel Comunal de Datos de muertes COVID
## PBH Julio 2020

# Carga datos ---------
source("Scripts/Load_Data/covidMuertes_load.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")


# Add Poblacion -----
df_muertes <- df_muertes %>% select(-poblacion) %>% 
  left_join(df_poblacion %>% select(codigo_comuna,poblacion))
df_muertes$poblacion %>% sum()

## Tasa Mortalidad --------
df_muertes <- df_muertes %>% 
  rename(covid_fallecidos=casos_fallecidos) %>% 
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

## Serie Temporal DEIS ------------

## Cumulate over Date
df_deis %>% names()
df_deis_tiempo <- df_deis %>%
  group_by(codigo_comuna,grupo_edad, tipo,date) %>%
  summarise(muertes=n()) %>% spread(tipo,muertes,fill = 0) %>%
  rename(muertes=confirmado, muertes_sospechoso=sospechoso)

df_deis_tiempo <- df_deis_tiempo %>%
  arrange(codigo_comuna,grupo_edad, date) %>%
  mutate(muertes_acc=cumsum(muertes),
         muertes_sospechoso_acc=cumsum(muertes_sospechoso)) %>% ungroup() %>%
  select(codigo_comuna,grupo_edad,date,muertes,muertes_acc,
         muertes_sospechoso,muertes_sospechoso_acc)

# Chequeo totales (fechas mas reciente)
df_aux <- df_deis_tiempo %>%
  group_by(codigo_comuna, grupo_edad) %>%
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
  group_by(codigo_comuna, grupo_edad) %>%
  complete(date = seq.Date(min(df_deis_tiempo$date), ## Completa con todos los dias en mis fechas limites
                           max(df_deis_tiempo$date),
                           by = "day"),codigo_comuna) %>%
  arrange(codigo_comuna,grupo_edad, date) %>%
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


## Serie tiempo nacional -----------
df_muertes_nacional <- df_deis_tiempo %>%
  group_by(date) %>%
  summarise(muertes_acc=sum(muertes_acc,na.rm=T),
            poblacion=sum(poblacion,na.rm=T),
            muertes=sum(muertes, na.rm=T),
            tasa_mortalidad_covid=muertes_acc/poblacion*1e5) %>% ungroup()
df_muertes_nacional$muertes %>% sum()


## Grafico Serie tiempo Region -----
df_muertes_nacional <- df_deis_tiempo %>%
  group_by(date, grupo_edad) %>%
  summarise(tasa_mortalidad_covid=sum(muertes_acc,na.rm=T)/
              sum(poblacion,na.rm=T)*1e5) %>% ungroup() %>%
  mutate(region="Nacional")

# Añado promedio nacional
df_muertes_region <- df_deis_tiempo %>%
  select(codigo_comuna, date,grupo_edad, muertes_acc, poblacion) %>%
  left_join(mapa_comuna) %>%
  group_by(region, date,grupo_edad) %>%
  summarise(tasa_mortalidad_covid=sum(muertes_acc,na.rm=T)/
              sum(poblacion,na.rm=T)*1e5) %>% ungroup() %>%
  rbind(df_muertes_nacional)

df_muertes_region %>%
  filter(!is.na(region)) %>%
  ggplot(aes(x=date, y=tasa_mortalidad_covid,col=grupo_edad))+
  geom_line()+
  # facet_grid(region~., scales = "free", space="free")
  facet_wrap(~region)+
  labs(x="", y="Tasa mortalidad COVID [por 100mil]")+
  theme(axis.text.x = element_text(angle = 90))
f_savePlot(last_plot(), sprintf(file_name,"SerieTiempoRegion"))


rm(df_muertes_region, df_muertes_nacional)


## EoF