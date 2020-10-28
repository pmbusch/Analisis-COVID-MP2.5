### Analisis-COVID-MP2.5
## Load Tasas de Morbilidad
## PBH Octubre 2020

## Entregadas por JMV el 26 Oct 2020

# Fuente Datos: JMV.DEIS
df_incidencia <- read_excel("Data/Data_Original/TasasIncidencia.xlsx",
                            sheet = "BD")

## Feat ------
names(df_incidencia) <- c("C","nombre_comuna","grupo_edad","causa_morbilidad",
                          "tasa_egresos")

# Cruze con comunas
df_incidencia <- df_incidencia %>% 
  left_join(codigos_territoriales, by=c("nombre_comuna"))

## Causas + Edad
df_incidencia$causa_morbilidad %>% unique()
df_incidencia$grupo_edad %>% unique()

df_incidencia <- df_incidencia %>% 
  mutate(tasa=paste(
    "tasaMorbilidad",
    causa_morbilidad %>% str_remove(" \\(Disease\\)"),
    grupo_edad %>% str_replace("65\\+","65plus"),
    sep="_") %>% 
      str_remove("_Todos"))
df_incidencia$tasa %>% unique()

## Generar DF
df_incidencia <- df_incidencia %>% 
  select(codigo_comuna, tasa, tasa_egresos) %>% 
  spread(tasa,tasa_egresos)



## EoF