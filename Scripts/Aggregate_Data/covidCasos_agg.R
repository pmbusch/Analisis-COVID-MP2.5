### Analisis-COVID-MP2.5
## Agregacion a nivel Comunal de Datos de contagios COVID
## PBH Julio 2020

# Load data --------
source("Scripts/Load_Data/covidCasos_load.R", encoding = "UTF-8") 
print_ggplot <- F

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


df_casos_nacional <- df_casos_tiempo %>% group_by(fecha) %>% 
  summarise(casos_acc=sum(casos_acc, na.rm=T),
            casos=sum(casos, na.rm = T)) %>% ungroup()

if (print_ggplot) ggplot(df_casos_nacional, aes(fecha, casos))+geom_line()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="Casos contagios COVID confirmados")

## Aplana Curva contagios extraña -----------------
# Entre el 16 y 19 de Junio existe un peak extraño de casos. 
# LAC sugiere distribuir el exceso de estos casos en 30 dias asi atras, de manera multiplicativa
# Esta distribucion se realiza a nivel de comuna

# Fechas ocurrencia suceso extraño
fecha_begin <- "2020-06-15" %>% strptime("%Y-%m-%d") %>% as_date()
fecha_end <- "2020-06-20" %>% strptime("%Y-%m-%d") %>% as_date()

# Peak
# df_casos_tiempo %>% filter(fecha>=fecha_begin & fecha<=fecha_end)

# Diferencia entre comienzo y fin de peak (normal). Linea Base sin este adicional
casos_begin <- df_casos_tiempo %>% filter(fecha==fecha_begin) %>% 
  select(codigo_comuna, casos) %>% rename(casos_begin=casos)
casos_end <- df_casos_tiempo %>% filter(fecha==fecha_end) %>% select(codigo_comuna, casos)
## pendiente de 4 dias para interlopar linealmente
m <- casos_begin %>% mutate(m=(casos_end$casos-casos_begin)/5, casos=NULL)

## Casos corregidos linea base
df_casos_tiempo <- df_casos_tiempo %>% left_join(m) ## add m
df_casos_tiempo <- df_casos_tiempo %>% left_join(casos_begin) ## add casos_begin
df_casos_tiempo <- df_casos_tiempo %>% 
  mutate(casos_lb=if_else(fecha>fecha_begin & fecha<fecha_end,
                                  round(casos_begin+m*(as.numeric(fecha-fecha_begin)),0),
                                  casos),
         casos_begin=NULL, m=NULL)

## Acumulado real vs acumulado linea base corregida
n_dias <- 30
df_aux <- df_casos_tiempo %>% filter(fecha>=fecha_end-n_dias & fecha<=fecha_end)

factor <- df_aux %>% group_by(codigo_comuna) %>% 
  summarise(casos=sum(casos),
            casos_lb=sum(casos_lb)) %>% ungroup() %>% 
  mutate(factor=if_else(casos_lb>0,casos/casos_lb,0))

## Casos aplanados
df_casos_tiempo <- df_casos_tiempo %>% 
  left_join(factor %>% select(codigo_comuna,factor)) ## Add factor
df_casos_tiempo <- df_casos_tiempo %>% 
  mutate(casos_aplanados=if_else(
    fecha>=fecha_end-n_dias & fecha<=fecha_end,
    round(casos_lb*factor,0),
    casos),
    casos_lb=NULL, factor=NULL)

## Cumulative casos aplanados
df_casos_tiempo <- df_casos_tiempo %>% 
  arrange(codigo_comuna, fecha) %>% 
  group_by(codigo_comuna) %>% 
  mutate(casos_aplanados_acc=cumsum(casos_aplanados)+min(casos_acc))

# Chequeo final sumas
df_casos_tiempo$casos %>% sum()
df_casos_tiempo$casos_aplanados %>% sum()

#Plot
if (print_ggplot) df_casos_tiempo %>% group_by(fecha) %>% 
  summarise(casos=sum(casos, na.rm = T),
            casos_aplanados=sum(casos_aplanados, na.rm = T)) %>% ungroup() %>%
  gather(tipo, casos, -fecha) %>% 
  ggplot(aes(fecha, casos))+geom_line()+
  facet_grid(tipo~.)+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="Casos contagios COVID confirmados")
rm(m, casos_begin, casos_end, df_aux, factor, fecha_begin, fecha_end,n_dias)


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
if (print_ggplot) ggplot(df_casos_edad, aes(fecha, casos, col=grupo_edad))+geom_line()+facet_wrap(~sexo)


## Aplana Curva contagios extraña Casos edad -----------------
# Fechas ocurrencia suceso extraño
fecha_begin <- "2020-06-15" %>% strptime("%Y-%m-%d") %>% as_date()
fecha_end <- "2020-06-20" %>% strptime("%Y-%m-%d") %>% as_date()

# Peak
df_casos_edad %>% filter(fecha>=fecha_begin & fecha<=fecha_end)

# Diferencia entre comienzo y fin de peak (normal). Linea Base sin este adicional
casos_begin <- df_casos_edad %>% filter(fecha==fecha_begin) %>% 
  select(grupo_edad,sexo, casos) %>% rename(casos_begin=casos)
casos_end <- df_casos_edad %>% filter(fecha==fecha_end) %>% 
  select(grupo_edad, sexo, casos)
## pendiente de 4 dias para interlopar linealmente
m <- casos_begin %>% mutate(m=(casos_end$casos-casos_begin)/5, casos=NULL)

## Casos corregidos linea base
df_casos_edad <- df_casos_edad %>% left_join(m) ## add m
df_casos_edad <- df_casos_edad %>% left_join(casos_begin) ## add casos_begin
df_casos_edad <- df_casos_edad %>% 
  mutate(casos_lb=if_else(fecha>fecha_begin & fecha<fecha_end,
                          round(casos_begin+m*(as.numeric(fecha-fecha_begin)),0),
                          casos),
         casos_begin=NULL, m=NULL)

## Acumulado real vs acumulado linea base corregida
n_dias <- 30
df_aux <- df_casos_edad %>% filter(fecha>=fecha_end-n_dias & fecha<=fecha_end)

factor <- df_aux %>% group_by(grupo_edad, sexo) %>% 
  summarise(casos=sum(casos),
            casos_lb=sum(casos_lb)) %>% ungroup() %>% 
  mutate(factor=if_else(casos_lb>0,casos/casos_lb,0))

## Casos aplanados
df_casos_edad <- df_casos_edad %>% 
  left_join(factor %>% select(grupo_edad, sexo,factor)) ## Add factor
df_casos_edad <- df_casos_edad %>% 
  mutate(casos_aplanados=if_else(
    fecha>=fecha_end-n_dias & fecha<=fecha_end,
    round(casos_lb*factor,0),
    casos),
    casos_lb=NULL, factor=NULL)

## Cumulative casos aplanados
df_casos_edad <- df_casos_edad %>% 
  arrange(grupo_edad, sexo, fecha) %>% 
  group_by(grupo_edad, sexo) %>% 
  mutate(casos_aplanados_acc=cumsum(casos_aplanados)+min(casos_acc))

# Chequeo final sumas
df_casos_edad$casos %>% sum()
df_casos_edad$casos_aplanados %>% sum()

rm(m, casos_begin, casos_end, df_aux, factor, fecha_begin, fecha_end,n_dias)
# 
if (print_ggplot) ggplot(df_casos_edad, aes(fecha, casos_aplanados, col=grupo_edad))+
  geom_line()+facet_wrap(~sexo)


## EoF