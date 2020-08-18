### Analisis-COVID-MP2.5
## Datos Covid. Fuente: https://github.com/MinCiencia/Datos-COVID19
## Producto 38: Casos fallecidos por comuna
## PBH Julio 2020


## Descarga de datos ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados
df_muertes <- read_csv(paste(url,"producto38","CasosFallecidosPorComuna_std.csv", sep="/"))
names(df_muertes) <- names(df_muertes) %>% str_to_lower() %>% str_replace_all(" ","_")
df_muertes <- df_muertes %>% na.omit() # limpio NA


## Dia primer muerte por comuna ---------------
# Limitado por fecha de primer reporte: 12-06-2020
df_muerteZero <- df_muertes %>% 
  filter(casos_fallecidos!=0) %>% #Filtro que haya casos
  group_by(codigo_comuna,comuna) %>% 
  summarise(dia_muerteZero=min(fecha, na.rm=T)) %>% ungroup()


## Muertes totales a la fecha -----------
# Filtro fechas mas reciente (dado que son muertes acumuladas)
fecha_muertes <- df_muertes$fecha %>% max()
df_muertes <- df_muertes %>% filter(fecha==fecha_muertes)

# total muertes
df_muertes$casos_fallecidos %>% sum()
df_muertes$poblacion %>% sum()

# Poblacion -----
## Utilizo poblacion de Chilemapas (censo 2017)
pob <- censo_2017_comunas %>% group_by(codigo_comuna) %>% 
  summarise(poblacion=sum(poblacion, na.rm=T)) %>% ungroup()

df_muertes <- df_muertes %>% select(-poblacion) %>% 
  left_join(pob)
df_muertes$poblacion %>% sum()
rm(pob)

## Parametros --------
df_muertes <- df_muertes %>% 
  mutate(tasa_mortalidad=casos_fallecidos/poblacion*1e5) %>% 
  left_join(df_muerteZero %>% select(-comuna)) %>% 
  mutate(dias_primerMuerte=(fecha-dia_muerteZero) %>% 
           as.numeric(units="days")) %>% 
  select(codigo_comuna, casos_fallecidos, tasa_mortalidad,
         dias_primerMuerte)

rm(url, df_muerteZero)

### Muertes DEIS -------------
# Descargar datos
# library(rvest)
# # # No logro encontrar el archivo mas reciente
# web <- read_html("https://deis.minsal.cl/#datosabiertos") %>% html_text()
# str_extract_all(web, ".rar")
# # No se descarga bien (debo hacerlo a mano)
# link <- "http://deis.minsal.cl/wp-content/uploads/2020/08/DEFUNCIONES_FUENTE_DEIS_2016_2020_06082020.rar"
# dest_file <- "Data/Data_Original/DEIS.rar"
# download.file(link, destfile = dest_file,
#               method = "libcurl", quiet = F)
# 
# 
# # NO LOGRO EXTRAER CORRECTAMENTE
# # extraer
# unzip("Data/Data_Original/DEIS.rar", 
#       exdir = "Data/Data_Original/DEIS")

# lectura
df_deis <- read_delim("Data/Data_Original/DEIS/DEFUNCIONES_FUENTE_DEIS_2016_2020_13082020.csv",
                 delim = ";",col_names = F,
                 col_types = "dDccccccccc",
                 locale = locale(encoding = "windows-1252"))
names(df_deis) <- c("year","date","sexo","edad","codigo_comuna","comuna","region",
               "causa_cie10","causa","cap_cie10","capitulo")

# Filtro COVID
df_deis %>% group_by(capitulo) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis <- df_deis %>% filter(capitulo=="COVID-19") %>% 
  mutate(tipo=str_extract(causa, "Confirmado|Sospechoso") %>% str_to_lower())

# Comparacion muertes
df_muertes$casos_fallecidos %>% sum()
df_deis %>% filter(tipo=="confirmado") %>% nrow()

# Cruzar con comunas
df_deis <- df_deis %>% filter(codigo_comuna!="99999") %>% 
  mutate(codigo_comuna=paste(
  if_else(str_length(codigo_comuna)==4,"0",""),codigo_comuna,sep=""))

# Factores
df_deis <- df_deis %>% mutate(sexo=factor(sexo),
                    edad=factor(edad),
                    causa_cie10=factor(causa_cie10),
                    cap_cie10=factor(cap_cie10),
                    tipo=factor(tipo))
nrow(df_deis) # muertes
# df_deis %>% skim()

df_deis$edad %>% unique()
df_deis <- df_deis %>% mutate(grupo_edad=case_when(
  edad %in% c("< 1","1 a 4","5 a 9","10 a 14") ~ "0-14",
  edad %in% c("15 a 19","20 a 24","25 a 29","30 a 34","35 a 39","40 a 44") ~ "15-44",
  edad %in% c("45 a 49","50 a 54","55 a 59","60 a 64") ~ "45-64",
  T ~ "65+"))

## Exportar para analisis
# cat('sep=; \n',file = "MuertesCovid_Deis.csv")
# write.table(df_deis,"MuertesCovid_Deis.csv",
#             sep=';',row.names = F, append = T)


# rm(df_deis)

## EoF