## Proyecto SINCA
## Descarga de datos de la web del SINCA
## PBH Octubre 2018
## Ultima atualizacion: PBH Jul 2020

# Parametros ----------
fecha_inicio <- "2016-01-01"
fecha_fin <- format(Sys.time(),'%Y-%m-%d')
contaminantes <- c("mp2.5", "co", "no2")


# Scripts necesarios ------------------
source('Scripts/00-Funciones.R')
source('Scripts/Load_Data/sinca_funcion_scrap.R')

## Carga Datos de las estaciones -------------
# Tipo de las variables: c character, d double, D date
cols_type <- "ccclccccdddccccDDccDccccclcccccc"
df_estaciones <- read_delim("Data/DatosEstacioneSINCA.csv", 
                            delim = ";", skip = 1, na = c("NA"),
                            col_types = cols_type,
                            locale = locale(encoding = "windows-1252"))
rm(cols_type)
spec(df_estaciones)

## FILTROS DESCARGA ---------------
# para descargar los datos, permite hacer filtro para no descargar todo
df_estaciones %>% names
df_estaciones$pollutant %>% unique()

# REGION, ESTACIONES, CONTAMINANTES Y METRICA
df_descarga <- df_estaciones %>%  
  filter(estacion_enlinea %in% c(T,F) &
           pollutant %in% contaminantes & 
           metrica=="Horario" )


# Si estaba al dia al momento de recolectar la info, actualizamos la fecha fin
df_descarga <- df_descarga %>% 
  mutate(contaminante_fechaFin=if_else(fecha_fin_actual, Sys.Date(), contaminante_fechaFin))


# Validez Fechas ingresadas
df_descarga<- df_descarga %>%  
  mutate(contaminante_fechaInicio = contaminante_fechaInicio %>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
         contaminante_fechaFin = contaminante_fechaFin %>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
         from = fecha_inicio %>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
         to = fecha_fin %>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
         inicio_valido = from>contaminante_fechaInicio,
         fin_valido = to<contaminante_fechaFin)

# Dejar solamente fechas validas, si no esta dentro del rango se deja el valor limite
df_descarga <- df_descarga %>% 
  mutate(from = if_else(inicio_valido, from, contaminante_fechaInicio)%>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
         to = if_else(fin_valido, to, contaminante_fechaFin)%>% 
           strptime(format='%Y-%m-%d') %>% as_date())

# Fechas en formato descarga (ej: pasar de 2015-12-31 a 151231)
df_descarga <- df_descarga %>% 
  mutate(from=paste(f_split_n(from,'-',1) %>% str_sub(3,4),
                                   f_split_n(from,'-',2),
                                   f_split_n(from,'-',3),sep=''),
         to=paste(f_split_n(to,'-',1) %>% str_sub(3,4),
                                 f_split_n(to,'-',2),
                                 f_split_n(to,'-',3),sep='')) 


# DESCARGA DE DATOS DE CONCENTRACION -----------------
# Reemplazable macro, date_from, date_to
url <-  'https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=%s&from=%s&to=%s&path=/usr/airviro/data/CONAMA/&lang=esp'

# Crear descarga con parametros establecidos
df_descarga <- df_descarga %>% mutate(url_descarga=sprintf(url,macro,from,to))


# Crear DF para almacenar informacion
df <- data.frame()

# Recorro los datos a descargar, almaceno la informacion en mi dataframe
for (d in 1:nrow(df_descarga)){
  tryCatch(
    {
    # Descarga concentraciones
    df_conc <- f_scrap_sinca(df_descarga$url_descarga[d],
                             file_name = paste(df_descarga$estacion[d], df_descarga$contaminante_cod[d],sep="_"),
                             remover_file = T)
    
    # Agregar info adicional
    df_conc <- df_conc %>% 
      mutate(site=df_descarga$estacion[d],
             region=df_descarga$region[d],
             provincia=df_descarga$provincia[d],
             comuna=df_descarga$comuna[d],
             coord_utm=df_descarga$coord_utm[d],
             huso=df_descarga$huso[d],
             longitud=df_descarga$longitud[d],
             latitud=df_descarga$latitud[d],
             tecnica=df_descarga$contaminante_tecnica[d],
             unidad=df_descarga$unidad[d],
             pollutant=df_descarga$pollutant[d])
    
    # Agregar a DF
    df <- rbind(df,df_conc)
    rm(df_conc)
    }, error = function(cond) return(NULL))
  }


## Feat Data ----------------

# Factores
df <- df %>% mutate(site=site %>% as.factor(),
                    region=region %>% as.factor(),
                    provincia=provincia %>% as.factor(),
                    comuna=comuna %>% as.factor(),
                    pollutant=pollutant %>% as.factor())


# Guardar como objeto de R
saveRDS(df, "Data/Data_Modelo/Datos_Concentraciones_raw.rsd")

rm(df, df_descarga, df_estaciones, contaminantes, d, fecha_fin, fecha_inicio, 
   url, f_scrap_sinca)

## EoF