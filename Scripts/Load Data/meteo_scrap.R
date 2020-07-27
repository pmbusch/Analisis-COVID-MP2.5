### Analisis-COVID-MP2.5
## Descarga de datos meteorologicos de https://climatologia.meteochile.gob.cl/
## PBH Julio 2020

## Scrap MeteoChile
library(rvest)

#Establecer el año que se quiere scrapear
year_start <- 2016
year_end <- 2020

#Lee informacion de estaciones
estaciones = read_excel("Data/Estaciones_Frecuentes_MeteoChile.xlsx")

#Variables auxiliares
tipo_meteo <- c('temperaturaMediaAnual','temperaturaMinimaAnual',
               'temperaturaMaximaAnual','humedadAnual')
# Formato tipo, estacion, año
url <- 'https://climatologia.meteochile.gob.cl/application/anual/%s/%s/%s'


df_meteo <- data.frame()

# Por año
for (year in year_start:year_end){
  cat(year, "\n")
  # Por estacion
  for (i in 1:nrow(estaciones)){
    #Estación a scrapear
    est <- estaciones[i,] %>% pull(Nacional)
    
    # Por parametro
    for (tipo in tipo_meteo){
      tryCatch({
        #Rescata info de URL
        web <- read_html(sprintf(url, tipo, est, year))
        df <- html_node(web, "#excel > div > table:nth-child(1)") %>% 
          html_table(fill = T)
        
        # header
        names(df) <- c("day", 1:12)
        ## Limpio filas innecesarias
        df <- df[-1,] %>% tibble()
        df <- df[-32:-35,]
        
        ## Aplano la tabla
        df <- df %>% gather(month, valor, -day) %>% 
          mutate(valor=as.numeric(valor)) %>% na.omit()
  
        ## Datos de la estacion
        df <- df %>% mutate(tipo=tipo, 
                            estacion=est,
                            comuna=estaciones[i,] %>% pull(Comuna),
                            provincia=estaciones[i,] %>% pull(Provincia),
                            region=estaciones[i,] %>% pull(Region))
             
        ## Date
        df <- df %>% mutate(year=year,
                            date=paste(year,month,day,sep="-") %>% 
                              strptime(format='%Y-%m-%d') %>% as_date())
           
        #Junta todo en lista de dataframes
        df_meteo <- rbind(df_meteo, df)
      }        
      , error = function(cond) return(NULL) )
    }
  }
}


#Ultimos arreglos
df_meteo <- df_meteo %>% 
  select(-year,-month,-day) %>% 
  mutate(tipo=case_when(
    tipo=="temperaturaMediaAnual" ~ "tmed",
    tipo=="temperaturaMinimaAnual" ~ "tmin",
    tipo=="temperaturaMaximaAnual" ~ "tmax",
    tipo=="humedadAnual" ~ "hr",
    T ~ "s/i"),
    unidad="celsius")

## Agregar a nivel comunal
## NOTAS:
# Debo mejorar la agregacion al incluir otros contaminantes y ver bien 
# promedio de estaciones con distinta disponibilidad de datos
df <- df_meteo %>% 
  mutate(year=year(date)) %>% 
  filter(year %in% c(2016,2017,2018,2019) &
           tipo %in% c("tmed", "hr")) %>% 
  cutData(type="season", hemisphere = "southern") %>% 
  filter(season %in% c("winter (JJA)","summer (DJF)")) %>% # solo verano e invierno
  group_by(comuna, estacion, tipo, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% 
  ungroup() %>% group_by(comuna, tipo, season) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  mutate(season=season %>% str_remove_all(" \\(JJA\\)| \\(DJF\\)"),
         tipo=paste(tipo, season, sep="_"),
         season=NULL) %>% 
  spread(tipo,valor)

# Agregar codigos comunales
df <- df %>% 
  mutate(nombre_comuna=comuna %>% 
           str_replace_all("Viña del Mar","Vina del Mar")) %>%
  left_join(codigos_territoriales %>% select(codigo_comuna, nombre_comuna),
            by=c("nombre_comuna")) %>% 
  select(-comuna, -nombre_comuna)


# Guardar como objeto de R
saveRDS(df_meteo, "Data/Data Modelo/Datos_Meteorologia_raw.rsd")
saveRDS(df, "Data/Data Modelo/Datos_Meteorologia.rsd")

# Limpio WS
rm(estaciones, tipo_meteo, url, year_start, year_end,
   web, i, year, est, tipo, df)

## EoF