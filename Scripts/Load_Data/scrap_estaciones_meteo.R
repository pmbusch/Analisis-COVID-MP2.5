### Analisis-COVID-MP2.5
## Descarga de datos meteorologicos de https://climatologia.meteochile.gob.cl/
## Permite generar un archivo csv con datos de las estaciones, monitoreo, y 
## links para su descarga
## PBH Julio 2020

library(rvest)

## Scrap MeteoChile
estaciones = read_excel("Data/Data_Original/Estaciones_MeteoChile_Original.xlsx",
                        na = c("Sólo datos en papel"))

## Solo estaciones con datos en linea
estaciones <- estaciones %>% filter(as.numeric(obs_meteorologicas)>0)

# Formato: codigo estacion
url <- "https://climatologia.meteochile.gob.cl/application/informacion/fichaDeEstacion/%s#inventario"


df_estaciones <- data.frame()
df_registros <- data.frame()
n <- 1

# Recorro cada estacion para obtener informacion
for (codigo in estaciones$codigo_nacional){
  cat(paste(codigo,n,"\n",sep=" "))
  n <- n+1
  web <- read_html(sprintf(url, codigo))
  
  # Datos geograficos
  comuna <- web %>% html_text() %>% str_extract("Comuna.*") %>% 
    str_remove("Comuna") %>% str_trim()
  provincia <- web %>% html_text() %>% str_extract("Provincia.*") %>% 
    str_remove("Provincia") %>% str_trim()
  region <- web %>% html_text() %>% str_extract("Región.*") %>% 
    str_remove("Región de") %>% str_trim()
  
  # Datos descarga
  elementos <- web %>% html_node("#excel > div:nth-child(5) > div.col-md-12 > div > div.panel-body > table") %>% 
    html_table(fill=T)
  names(elementos) <- c("id","param","desc","inicio","fin","n_registros")
  
  # Borro fila 1 y filtro por datos con año
  elementos <- elementos[-1,] %>% filter(inicio!=".")
  elementos <- elementos %>% mutate(id=id %>% str_remove_all(",") %>% as.numeric(),
                                    inicio=inicio %>% str_remove_all(",") %>% as.numeric(),
                                    fin=fin %>% str_remove_all(",") %>% as.numeric(),
                                    n_registros=n_registros %>% str_remove_all(",") %>% as.numeric())
  # Agrego codigo para despues juntar con right join
  elementos <- elementos %>% mutate(codigo_nacional=codigo)
  
  ## Consolido en data frame
  df_aux <- estaciones %>% filter(codigo_nacional==codigo)
  df_aux <- df_aux %>% mutate(comuna=comuna,
                              provincia=provincia,
                              region=region)
  
  ## Consolido en dos data frame
  df_estaciones <- rbind(df_estaciones, df_aux)
  df_registros <- rbind(df_registros, elementos)
  
  # limpio para evitar repeticiones
  rm(comuna, provincia, region, elementos, df_aux, web)
}

rm(n, url, codigo)

## Feat datos estaciones -------
source("Scripts/00-Funciones.R", encoding = "UTF-8")
df_estaciones <- df_estaciones %>% mutate(comuna=f_remover_acentos(comuna),
                                          provincia=f_remover_acentos(provincia),
                                          region=f_remover_acentos(region))

# Utilizo la region_meteo, obtenida de la descarga de excel de datos
df_estaciones$region %>% unique()
df_estaciones$region_meteo %>% unique()
levels_region <- c("XV","I","II","III","IV","V","M","VI","VII","VIII",
                   "IX","XIV","X","XI","XII")

df_estaciones <- df_estaciones %>% mutate(region=case_when(
  region_meteo=="Región de Arica y Parinacota" ~ "XV",
  region_meteo=="Región de Tarapacá" ~ "I",
  region_meteo=="Región de Antofagasta" ~ "II",
  region_meteo=="Región de Atacama" ~ "III",
  region_meteo=="Región de Coquimbo" ~ "IV",
  region_meteo=="Región de Valparaíso" ~ "V",
  region_meteo=="Región de Metropolitana de Santiago" ~ "M",
  region_meteo=="Región de Del Libertador Gral. Bernardo O'Higgins" ~ "VI",
  region_meteo=="Región de Del Maule" ~ "VII",
  region_meteo=="Región de De Ñuble" ~ "VII",
  region_meteo=="Región de Del Biobío" ~ "VII",
  region_meteo=="Región de De La Araucanía" ~ "IX",
  region_meteo=="Región de De Los Ríos" ~ "XIV",
  region_meteo=="Región de De Los Lagos" ~ "X",
  region_meteo=="Región de Aisén del Gral. Carlos Ibáñez del Campo" ~ "XI",
  region_meteo=="Región de Magallanes y de la Antártica Chilena" ~ "XII",
  T ~ "s/i") %>% factor(levels = levels_region))
df_estaciones$region %>% unique()
  
cat('sep=; \n',file = "Data/Estaciones_Meteo.csv")
write.table(df_estaciones,"Data/Estaciones_Meteo.csv",
            sep=';',row.names = F, append = T)


## Feat datos registros -------
df_registros$inicio %>% range()
df_registros$fin %>% range()
df_registros$param %>% unique()

# Right join
df_registros <- right_join(df_estaciones, df_registros, by=c("codigo_nacional"))

cat('sep=; \n',file = "Data/Estaciones_Meteo_Registros.csv")
write.table(df_estaciones,"Data/Estaciones_Meteo_Registros.csv",
            sep=';',row.names = F, append = T)


## EoF