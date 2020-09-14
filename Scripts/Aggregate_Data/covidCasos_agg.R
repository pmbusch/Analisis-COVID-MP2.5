### Analisis-COVID-MP2.5
## Agregacion a nivel Comunal de Datos de contagios COVID
## PBH Julio 2020

# Load data --------
source("Scripts/Load_Data/covidCasos_load.R", encoding = "UTF-8") 

# Expande serie de tiempo ----------------
# Interpola dado que reporte es cada 3 dias
library(zoo)
df_casos_tiempo <- df_casos_tiempo %>%
  select(fecha, codigo_comuna, casos_confirmados) %>% 
  rename(casos_acc=casos_confirmados) %>% 
  # mutate(original=1) %>%
  group_by(codigo_comuna) %>% 
  complete(fecha = seq.Date(min(df_casos_tiempo$fecha), 
                            max(df_casos_tiempo$fecha),
                            by = "day"),codigo_comuna) %>%
  arrange(codigo_comuna,fecha ) %>% ungroup() %>% 
  mutate(casos_acc=na.approx(casos_acc) %>% round(0))

# Calcula casos por dia (resta del acumulado)
df_casos_tiempo <- df_casos_tiempo %>% 
  arrange(codigo_comuna,fecha) %>% 
  group_by(codigo_comuna) %>%
  mutate(casos = casos_acc - lag(casos_acc,default = first(casos_acc))) %>% 
  ungroup()

## Nota: Existen comunas que disminuyen sus casos acumulados entre informes
## ERROR DE LOS DATOS
df_casos_tiempo <- df_casos_tiempo %>% 
  mutate(casos=if_else(casos<0, 0, casos))


## Nacional por Edad ----------------
# ggplot(df_casos_edad, aes(fecha, casos_acc, col=grupo_edad))+geom_line()+facet_wrap(~sexo)

# Interpola por dias sin reporte de casos
df_casos_edad <- df_casos_edad %>%
  # select(fecha, grupo_edad, casos_acc) %>% 
  mutate(original=1) %>%
  group_by(grupo_edad, sexo) %>% 
  complete(fecha = seq.Date(min(df_casos_edad$fecha), 
                            max(df_casos_edad$fecha),
                            by = "day"),grupo_edad, sexo) %>%
  arrange(grupo_edad,sexo,fecha) %>% ungroup() %>% 
  mutate(casos_acc=na.approx(casos_acc) %>% round(0))


# Calcula casos por dia (resta del acumulado)
df_casos_edad <- df_casos_edad %>% 
  arrange(fecha,grupo_edad, sexo) %>% 
  group_by(grupo_edad, sexo) %>%
  mutate(casos = casos_acc - lag(casos_acc,default = first(casos_acc))) %>% 
  ungroup()
# ggplot(df_casos_edad, aes(fecha, casos, col=grupo_edad))+geom_line()+facet_wrap(~sexo)

## EoF