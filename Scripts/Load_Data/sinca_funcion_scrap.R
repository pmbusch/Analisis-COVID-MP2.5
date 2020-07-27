### Analisis-COVID-MP2.5
## Descarga de datos de la web del SINCA
## Funcion para descargar datos del SINCA
## PBH Julio 2020


# Funcion para descargar y trabajar datos de concentracion a nivel diario
# Recibe el URL de la descarga y nombre del archivo
f_scrap_sinca <- function(url, file_name = "Descarga.csv", remover_file=T){
  
  # Descarga y Lectura de archivo  --------------
  destino <- paste('Data',"Temp",file_name,sep='/')
  download.file(url,destfile = destino, quiet = T)
  
  # csv2 uses sep=; and decimal mark=,
  df_conc <- read_delim(destino, delim=";", na = c("NA"), col_types="ccdddd",
                        locale = locale(decimal_mark = ","))
  spec(df_conc)
  df_conc$X6 <- NULL # columna adicional
  
  # If para asegurar que datos meteorologicos pasen igual como validados todos
  if (ncol(df_conc)<5){
    df_conc$X5 <- NA %>% as.numeric()
  }
  
  colnames(df_conc) <- c('fecha','hora','validados','preliminares','noValidados')
  
  if (remover_file){
    file.remove(destino)
  }
  
  # Jerarquia de datos: validados > preliminares > noValidados
  df_conc <- df_conc %>% mutate(tipo_dato = case_when(
    !is.na(validados) ~ "validados",
    !is.na(preliminares) ~ "preliminares",
    !is.na(noValidados) ~ "noValidados",
    TRUE ~ "NA"))
  df_conc %>% group_by(tipo_dato) %>% summarise(count=n())
  
  # remover valores NA 
  df_conc <- df_conc %>% filter(tipo_dato!="NA") 
  
  # Asigno el valor y borro columnas
  df_conc <- df_conc %>% 
    mutate(valor = case_when(
    tipo_dato=="validados" ~ validados,
    tipo_dato=="preliminares" ~ preliminares,
    tipo_dato=="noValidados" ~ noValidados),
    validados = NULL,
    preliminares = NULL,
    noValidados = NULL)
  
  # DATE
  df_conc<- df_conc %>% 
    mutate(date=paste(str_sub(fecha,5,6),
                      str_sub(fecha,3,4),
                      str_sub(fecha,1,2),sep='-'),
           hora=NULL,
           date=paste(date,sep=''))
  
  # Formato a date
  df_conc <- df_conc %>% 
    mutate(date = date %>% strptime(format='%d-%m-%y', tz="GMT") %>% as_date())
  
  # Borrar variables innecesarias
  df_conc <- df_conc %>% mutate(fecha=NULL)
  
  #  Util tener estos dates
  df_conc <- df_conc %>% mutate(year=date %>% year(),
                                month=date %>% month(),
                                day=date %>% day())
  
  
  df_conc <- df_conc %>% mutate(url=url)
  ## Retorno de datos -----------
  
  ## Codigo antiguo cuando se descargaban datos horarios
  # Dias con al menos 75% de los datos
  # df_conc_dia <- df_conc %>% group_by(url, year, month, day) %>% 
  #   summarise(valor=mean(valor,na.rm=T),count=n()) %>% 
  #   filter(count>=18)
  # 
  # df_conc_mes <- df_conc_dia %>% 
  #   group_by(url,year, month) %>% 
  #   summarise(valor=mean(valor,na.rm=T), 
  #             count=n()) %>% 
  #   ungroup()
  
  return(df_conc)
} 
## EoF