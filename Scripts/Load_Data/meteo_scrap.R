### Analisis-COVID-MP2.5
## Descarga de datos meteorologicos de https://climatologia.meteochile.gob.cl/
## PBH Julio 2020
# Nota: Columna "nombre" es el nombre original de la estacion para la descarga
# Columna "site" es el nombre de la estacion modificado para evitar repeticiones
# Columna codigo nacional es dada por la pagina, y no se repite


## Scrap MeteoChile
library(rvest)
source("Scripts/00-Funciones.R", encoding = "UTF-8")

#Establecer el año que se quiere scrapear
year_start <- 2000
year_end <- 2020

#Lee informacion de estaciones
# estaciones = read_excel("Data/Estaciones_Frecuentes_MeteoChile.xlsx")
estaciones = read_delim("Data/Data_Original/Estaciones_Meteo.csv",
                        delim = ";", skip = 1, na = c("NA"),
                        col_types = "dcccddddddcccc",
                        locale = locale(encoding = "windows-1252"))


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
    cat(i, " ")
    #Estación a scrapear
    est <- estaciones[i,] %>% pull(codigo_nacional)
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
                            site=estaciones[i,] %>% pull(site),
                            comuna=estaciones[i,] %>% pull(comuna),
                            provincia=estaciones[i,] %>% pull(provincia),
                            region=estaciones[i,] %>% pull(region),
                            nombre_estacion=estaciones[i,] %>% pull(nombre),
                            propietario=estaciones[i,] %>% pull(propietario),
                            latitud=estaciones[i,] %>% pull(latitud),
                            longitud=estaciones[i,] %>% pull(longitud),
                            altura_msnm=estaciones[i,] %>% pull(altura_metros))
             
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
    T ~ "s/i"))


## Features: Heating Degree -------
# https://en.wikipedia.org/wiki/Heating_degree_day
# 18 grados
df_hd18 <- df_meteo %>% 
  filter(tipo %in% c("tmin","tmax")) %>%
  spread(tipo, valor) %>% 
  mutate(heating_degree=18-(tmax-tmin)/2,
         heating_degree=if_else(heating_degree>0,heating_degree, 0),
         tipo="heating_degree_18") %>% 
  rename(valor=heating_degree) %>% 
  select(-tmin,-tmax)

# 15 grados
df_hd15 <- df_meteo %>% 
  filter(tipo %in% c("tmin","tmax")) %>%
  spread(tipo, valor) %>% 
  mutate(heating_degree=15-(tmax-tmin)/2,
         heating_degree=if_else(heating_degree>0,heating_degree, 0),
         tipo="heating_degree_15") %>% 
  rename(valor=heating_degree) %>% 
  select(-tmin,-tmax)

df_meteo <- df_meteo %>% rbind(df_hd18) %>% rbind(df_hd15)
rm(df_hd18,df_hd15)

# Season ---------
df_meteo <- df_meteo %>% filter(!is.na(date)) %>% 
  mutate(season=getSeason(date))

# Agregar codigos comunales ---------
df_meteo <- df_meteo %>% 
  mutate(nombre_comuna=comuna %>% 
           str_replace_all("Viña del Mar","Vina del Mar") %>% 
           str_replace_all("Purrangue","Purranque") %>% 
           str_replace_all("San Bernando","San Bernardo") %>% 
           str_replace_all("Cabo de Hornos \\(Ex-Navarino\\)","Cabo de Hornos") %>% 
           str_replace_all("Zapallar, V region","Zapallar") %>% 
           str_replace_all("Retiro, VII region","Retiro")) %>%
  left_join(codigos_territoriales %>% select(codigo_comuna, nombre_comuna),
            by=c("nombre_comuna")) %>% 
  select(-comuna, -nombre_comuna)


# Guardar como objeto de R ------
saveRDS(df_meteo, "Data/Data_Modelo/Datos_Meteorologia_raw.rsd")

# Limpio WS
rm(estaciones, tipo_meteo, url, year_start, year_end,
   web, i, year, est, tipo)

## EoF

## Ubicacion comunal de las estaciones -------
# DEPRECIADO
# 
# library(sp)
# df_sp <- SpatialPointsDataFrame(estaciones %>% select(longitud, latitud),
#                                 data=estaciones,
#                                 proj4string = CRS("+proj=longlat +ellps=GRS80 +no_defs"))
# plot(df_sp)
# 
# comunas <- mapa_comunas$geometry %>% as_Spatial()
# plot(comunas)
# 
# # indices
# df <- over(df_sp, comunas)
# comuna_aux <- mapa_comunas %>% rowid_to_column()
# df_est <- cbind(estaciones, df) %>% rename(rowid=df) %>% 
#   left_join(comuna_aux) %>% 
#   left_join(codigos_territoriales)
# df_est %>% names()
# df_est %>% select(codigo_nacional, nombre, latitud, longitud,
#                   region_meteo, nombre_comuna, nombre_region) %>% 
#   view()
# df_est %>% group_by(region_meteo, nombre_region) %>% 
#   summarise(count=n()) %>% arrange(desc(count)) %>% view()
# mapa_comunas[323,]

## EoF