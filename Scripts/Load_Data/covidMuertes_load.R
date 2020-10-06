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

## Muertes totales a la fecha -----------
# Filtro fechas mas reciente (dado que son muertes acumuladas)
fecha_muertes <- df_muertes$fecha %>% max()
df_muertes <- df_muertes %>% filter(fecha==fecha_muertes)

# total muertes
df_muertes$casos_fallecidos %>% sum()
df_muertes$poblacion %>% sum()

rm(url)

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
fecha_deis <- "01-10-2020"
df_deis <- read_delim(paste(
  "Data/Data_Original/DEIS/DEFUNCIONES_FUENTE_DEIS_2016_2020_",
  fecha_deis %>% str_remove_all("-"),".csv",sep=""),
                 delim = ";",col_names = T,
                 col_types = "Dcddccccccccccccccccccccc",
                 locale = locale(encoding = "windows-1252",
                                 date_format = "%d-%m-%Y"))
spec(df_deis)
names(df_deis) <- c("date","sexo","edad_tipo","edad",
                    "codigo_comuna","comuna","region",
                    "diag1","cap_diag1","glosa_cap_diag1",
                    "grupo_diag1","glosa_grupo_diag1",
                    "categ_diag1","glosa_categ_diag1",
                    "subcateg_diag1","glosa_subcateg_diag1",
                    "diag2","cap_diag2","glosa_cap_diag2",
                    "grupo_diag2","glosa_grupo_diag2",
                    "categ_diag2","glosa_categ_diag2",
                    "subcateg_diag2","glosa_subcateg_diag2")
                    # "causa_cie10","causa","cap_cie10","capitulo")

## Add year: antes estaba y lo borarron, como cambian el formato semana a semana....
df_deis <- df_deis %>% mutate(year=year(date))

# Cruzar con comunas
df_deis <- df_deis %>% filter(codigo_comuna!="99999") %>% 
  mutate(codigo_comuna=paste(
  if_else(str_length(codigo_comuna)==4,"0",""),codigo_comuna,sep=""))

# Arreglo Edad
df_deis %>% group_by(edad_tipo) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis <- df_deis %>% mutate(edad=if_else(edad_tipo!=1,0,edad),
                              edad_tipo=1)

# Factores
df_deis <- df_deis %>% mutate(sexo=factor(sexo),
                    glosa_subcateg_diag1=factor(glosa_subcateg_diag1))

df_deis$edad %>% unique()
df_deis <- df_deis %>% mutate(grupo_edad=case_when(
  edad < 15 ~ "0-14",
  edad < 45 ~ "15-44",
  edad < 65 ~ "45-64",
  T ~ "65+"))

# Save for total deaths
df_deis_total <- df_deis

# Filtro COVID
df_deis %>% group_by(glosa_cap_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_grupo_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_categ_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_subcateg_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis <- df_deis %>% filter(str_detect(glosa_subcateg_diag1,"COVID-19")) %>% 
  mutate(tipo=str_extract(glosa_subcateg_diag1, "no identificado|identificado"),
         tipo=if_else(tipo=="no identificado","sospechoso","confirmado") %>% factor())
 
# Comparacion muertes
df_muertes$casos_fallecidos %>% sum()
df_deis %>% filter(tipo=="confirmado") %>% nrow()

nrow(df_deis) # muertes totales
df_deis$tipo %>% table() #por tipo
# df_deis %>% skim()



# df_deis %>% group_by(grupo_edad,edad) %>% 
#   summarise(count=n()) %>% arrange(desc(count)) %>% view()
# 
# df_deis <- df_deis %>% mutate(grupo_edad=case_when(
#   edad %in% c("< 1","1 a 4","5 a 9","10 a 14") ~ "0-14",
#   edad %in% c("15 a 19","20 a 24","25 a 29","30 a 34","35 a 39","40 a 44") ~ "15-44",
#   edad %in% c("45 a 49","50 a 54","55 a 59","60 a 64") ~ "45-64",
#   T ~ "65+"))

## Exportar para analisis
# cat('sep=; \n',file = "MuertesCovid_Deis.csv")
# write.table(df_deis,"MuertesCovid_Deis.csv",
#             sep=';',row.names = F, append = T)


# rm(df_deis)

## EoF