### Analisis-COVID-MP2.5
## Funciones transversales al proyecto
## PBH Julio 2020

# https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

f_remover_acentos <- function(x){
  x %>% 
    str_replace_all("á","a") %>% 
    str_replace_all("é","e") %>% 
    str_replace_all("í","i") %>% 
    str_replace_all("ó","o") %>% 
    str_replace_all("ú","u") %>% 
    str_replace_all("ñ","n") %>% 
    str_replace_all("Ñ","N") %>% 
    str_replace_all("Á","A") %>% 
    str_replace_all("É","E") %>% 
    str_replace_all("Í","I") %>% 
    str_replace_all("Ó","O") %>% 
    str_replace_all("Ú","U")
}

f_split_n <- function(X,Sep,N){
  X %>% str_split(Sep) %>% sapply(function(x) x[N])
}

cuartilHora <- function(x){
  retVal = 'NA'
  if (x<7){
    retVal='Hora: 00-06'
  }
  else if (x<13){
    retVal='Hora: 06-12'
  }
  else if (x<19){
    retVal='Hora: 12-18'
  }
  else{retVal='Hora: 18-24'}
  return(retVal)
}

## Fuente: https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
# https://www.estacionesdelano.com/chile/
getSeason <- function(dat) {
  stopifnot(class(dat) == "Date")
  scalarCheck <- function(dat) {
    m <- month(dat)      
    d <- day(dat)        
    if ((m == 9 & d >= 23) | (m == 10) | (m == 11) | (m == 12 & d < 21)) {
      r <- 1
      } else if ((m == 12 & d >= 21) | (m == 1) | (m == 2) | (m == 3 & d < 21)) {
        r <- 2
        } else if ((m == 3 & d >= 21) | (m == 4) | (m == 5) | (m == 6 & d < 21))
          {
          r <- 3
          } else {
            r <- 4
            }
    r
    }
    res <- sapply(dat, scalarCheck)
    res <- ordered(res, labels=c("spring", "summer", "fall", "winter"))
    invisible(res)
}


## Save Plot ----------
f_savePlot <- function(p1, file_path, dpi=600){
  cat("Saving: ",file_path)
  ggsave(file_path, {{p1}},dpi=dpi,
         width = 14.87, height = 9.30, units = "in")
}

## Save CSV --------
f_saveCsv <- function(datos, file_path){
  cat("Saving: ",file_path)
  cat('sep=; \n',file = file_path)
  write.table(datos, file_path,
              sep=';',row.names = F, append = T)
}

## Rename Variables ----------
# Funcion para incorporar un nombre mas representativo a la variable
# Utilizada principalmente para resultados del modelo
f_replaceVar <- function(variable_orig){
  return(
    case_when(
      variable_orig== "Intercept" ~ "Intercepto",
      variable_orig== "poblacion" ~ "Poblacion",
      variable_orig== "tasa_mortalidad_covid" ~ "Tasa Mortalidad COVID [por 100mil]",
      variable_orig== "covid_fallecidos" ~ "Número fallecidos por COVID",
      variable_orig== "tasa_contagios" ~ "Tasa Contagios COVID [por 100mil]",
      variable_orig== "casos_confirmados" ~ "Número contagios por COVID",
      variable_orig== "perc_letalidad" ~ "% Letalidad COVID",
      variable_orig== "tasa_mortalidad_all" ~ "Tasa Mortalidad Total [por 100mil]",
      variable_orig== "defunciones" ~ "Número fallecidos todas las causas",
     variable_orig== "mp25" ~ "MP2.5 2017-2019 [ug/m3]",
     variable_orig== "mp25_fall" ~ "MP2.5 2017-2019 Otoño",
     variable_orig== "mp25_winter" ~ "MP2.5 2017-2019 Invierno",
     variable_orig== "mp25_spring" ~ "MP2.5 2017-2019 Primavera",
     variable_orig== "mp25_summer" ~ "MP2.5 2017-2019 Verano",
     variable_orig== "densidad_pob" ~ "Densidad [hab/km2]",
     variable_orig== "densidad_pob_censal" ~ "Densidad urbana",
     variable_orig== "quintil_dens_pob" ~ "Quintil densidad poblacion",
     variable_orig== "densidad_pob_manzana_mediana" ~ "Mediana densidad manzana",
     variable_orig== "densidad_pob_manzana_p90" ~ "Percentil 90 densidad manzana",
     variable_orig== "densidad_pob_manzana_media" ~ "Media densidad manzana",
     variable_orig== "`0-14`" ~ "% Edad 0-14",
     variable_orig== "0-14" ~ "% Edad 0-14",
     variable_orig== "`15-44`" ~ "% Edad 15-44",
     variable_orig== "15-44" ~ "% Edad 15-44",
     variable_orig== "`45-64`" ~ "% Edad 45-64",
     variable_orig== "45-64" ~ "% Edad 45-64",
     variable_orig== "`65+`"~ "% Edad 65+",
     variable_orig== "65+"~ "% Edad 65+",
     variable_orig== "superficie"~ "Superficie [m2]",
     variable_orig== "perimetro" ~ "Perimetro [m]",
     variable_orig== "superficie_censal" ~ "Superficie de zonas urbanas [m2]",
     variable_orig== "perc_mujer"~ "% Mujer",
     variable_orig== "perc_rural"~ "% Rural",
     variable_orig== "perc_puebloOrig"~ "% Pueblo Originario",
     variable_orig== "perc_material_irrecuperable"~ "% Vivienda con Material irrecuperable",
     variable_orig== "dias_primerContagio" ~ "Dias desde primer contagio",
     variable_orig== "dias_primerMuerte"~ "Dias desde primera muerte",
     variable_orig== "dias_cuarentena"~ "Dias desde cuarentena",
     variable_orig== "tasa_camas"~ "Camas hospitalarias [por 100mil]",
     variable_orig== "ingresoTotal_media"~ "Media Ingreso total mensual",
     variable_orig== "ingresoTotal_mediana"~ "Mediana Ingreso total mensual",
     variable_orig== "ingresoAutonomo_media" ~ "Media Ingreso autonomo mensual",
     variable_orig== "ingresoAutonomo_mediana" ~ "Mediana Ingreso autonomo mensual",
     variable_orig== "perc_isapre"~ "% Isapre",
     variable_orig== "perc_fonasa"~ "% Fonasa",
     variable_orig== "perc_fonasa_A"~ "% Fonasa-A",
     variable_orig== "perc_fonasa_B"~ "% Fonasa-B",
     variable_orig== "perc_fonasa_C"~ "% Fonasa-C",
     variable_orig== "perc_fonasa_D"~ "% Fonasa-D",
     variable_orig== "perc_FFAA" ~ "% Previsión Salud FF.AA.",
     variable_orig== "perc_menor_media"~ "% Educación menor a media",
     variable_orig== "perc_ocupado"~ "% Ocupado laboral",
     variable_orig=="consumo_lena_m3" ~" Consumo regional leña CDT",
     variable_orig== "cons_lena_calefactor_pp"~ "Consumo anual leña calefactor [kWh per cápita]",
     variable_orig== "cons_lena_cocina_pp"~ "Consumo anual leña cocina [kWh per cápita]",
     variable_orig== "perc_lenaCocina"~ "% Uso leña cocina",
     variable_orig== "perc_lenaCalefaccion" ~ "% Uso leña calefaccion",
     variable_orig== "perc_lenaAgua"~ "% Uso leña agua caliente",
     variable_orig== "hr_anual" ~ "Humedad relativa media [%]" ,
     variable_orig== "hr_summer" ~ "Humedad relativa media Verano [%]" ,
     variable_orig== "hr_winter"~ "Humedad relativa media Invierno [%]",
     variable_orig== "hr_fall" ~ "Humedad relativa media Otoño [%]" ,
     variable_orig== "hr_spring"~ "Humedad relativa media Primavera [%]",
     variable_orig== "tmed_anual"~ "Temperatura media [°C]",
     variable_orig== "tmed_summer"~ "Temperatura media Verano [°C]",
     variable_orig== "tmed_winter"~ "Temperatura media Invierno [°C]",
     variable_orig== "tmed_fall"~ "Temperatura media Otoño [°C]",
     variable_orig== "tmed_spring"~ "Temperatura media Primavera [°C]",
     variable_orig== "heating_degree_15_anual"~ "HDD 15°C [°C]",
     variable_orig== "heating_degree_15_summer"~ "HDD 15°C Verano [°C]",
     variable_orig== "heating_degree_15_winter"~ "HDD 15°C Invierno [°C]",
     variable_orig== "heating_degree_15_fall"~ "HDD 15°C Otoño [°C]",
     variable_orig== "heating_degree_15_spring"~ "HDD 15°C Primavera [°C]",
     variable_orig== "heating_degree_18_anual"~ "HDD 18°C [°C]",
     variable_orig== "heating_degree_18_summer"~ "HDD 18°C Verano [°C]",
     variable_orig== "heating_degree_18_winter"~ "HDD 18°C Invierno [°C]",
     variable_orig== "heating_degree_18_fall"~ "HDD 18°C Otoño [°C]",
     variable_orig== "heating_degree_18_spring"~ "HDD 18°C Primavera [°C]",
     variable_orig== "hdd15_winter_lenaCalefaccion"~ "HDD 15° Invierno * % Leña Calefacción ",
      T ~ variable_orig))
}

## Type of  Variables ----------
# Funcion para incorporar el tipo a la variable
# Utilizada principalmente para resultados del modelo
f_addTypeVar <- function(var){
  return(
    case_when(
      var %in% c("tasa_mortalidad_covid", "covid_fallecidos",
                   "tasa_contagios","casos_confirmados",
                   "dias_primerContagio","dias_primerMuerte","dias_cuarentena",
                   "perc_letalidad") ~ 
        "COVID-19",
      var %in% c("mp25","mp25_fall","mp25_winter",
                   "mp25_spring","mp25_summer")~ 
        "MP2.5",
      var %in% c("poblacion","densidad_pob","densidad_pob_censal",
                   "0-14","15-44","45-64","65+","perc_mujer",
                   "perc_rural","perc_puebloOrig",
                   "perc_material_irrecuperable","tasa_mortalidad_all") ~ 
        "Demografía",
      var %in% c("perc_menor_media","perc_ocupado",
                   "perc_isapre","perc_FFAA","perc_fonasa_A","perc_fonasa_B",
                   "perc_fonasa_C", "perc_fonasa_D","perc_fonasa",
                 "ingresoTotal_media", "ingresoAutonomo_media",
                 "ingresoTotal_mediana", "ingresoAutonomo_mediana",
                 "tasa_camas")  ~ 
        "Socioeconómico",
      var %in% c("cons_lena_cocina_pp","cons_lena_calefactor_pp",
                   "perc_lenaCocina","perc_lenaCalefaccion",
                   "perc_lenaAgua")  ~ 
        "Leña",
      var %in% c("hr_anual","hr_fall","hr_winter","hr_spring","hr_summer",
                 "tmed_anual","tmed_fall","tmed_winter","tmed_spring","tmed_summer",
                 "heating_degree_15_anual","heating_degree_15_fall","heating_degree_15_winter","heating_degree_15_spring","heating_degree_15_summer",
                 "heating_degree_18_anual","heating_degree_18_fall","heating_degree_18_winter","heating_degree_18_spring","heating_degree_18_summer") ~ 
        "Meteorología",
      T ~ "s/i") %>% 
      factor(levels=c("COVID-19","MP2.5","Leña","Demografía",
                          "Socioeconómico","Meteorología")))
}


## Fuente: https://stackoverflow.com/questions/7508229/how-to-create-a-column-with-a-quartile-rank
## Devuelve un vector con la clasificacion de cada valor en su quintil
qgroup = function(numvec, n = 5){
  qtile = quantile(numvec, probs = seq(0, 1, 1/n), na.rm=T)
  out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
  out=paste("Q",out,sep="") %>% factor(levels=paste("Q",1:n,sep=""))
  return(out)
}
