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
     variable_orig== "mp25" ~ "MP2.5 [ug/m3]",
     variable_orig== "densidad_pob" ~ "Densidad [hab/km2]",
     variable_orig== "densidad_pob_censal" ~ "Densidad censal [hab/km2]",
     variable_orig== "quintil_dens_pob" ~ "Quintil densidad poblacion",
     variable_orig== "`0-14`" ~ "% 0-14",
     variable_orig== "0-14" ~ "% 0-14",
     variable_orig== "`15-44`" ~ "% 15-44",
     variable_orig== "15-44" ~ "% 15-44",
     variable_orig== "`45-64`" ~ "% 45-64",
     variable_orig== "45-64" ~ "% 45-64",
     variable_orig== "`65+`"~ "% 65+",
     variable_orig== "65+"~ "% 65+",
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
     variable_orig== "heating_degree_15_anual"~ "Heating Degree 15°C [°C]",
     variable_orig== "heating_degree_15_summer"~ "Heating Degree 15°C Verano [°C]",
     variable_orig== "heating_degree_15_winter"~ "Heating Degree 15°C Invierno [°C]",
     variable_orig== "heating_degree_15_fall"~ "Heating Degree 15°C Otoño [°C]",
     variable_orig== "heating_degree_15_spring"~ "Heating Degree 15°C Primavera [°C]",
     variable_orig== "heating_degree_18_anual"~ "Heating Degree 18°C [°C]",
     variable_orig== "heating_degree_18_summer"~ "Heating Degree 18°C Verano [°C]",
     variable_orig== "heating_degree_18_winter"~ "Heating Degree 18°C Invierno [°C]",
     variable_orig== "heating_degree_18_fall"~ "Heating Degree 18°C Otoño [°C]",
     variable_orig== "heating_degree_18_spring"~ "Heating Degree 18°C Primavera [°C]",
      T ~ variable_orig))
}

## Fuente: https://stackoverflow.com/questions/7508229/how-to-create-a-column-with-a-quartile-rank
## Devuelve un vector con la clasificacion de cada valor en su quintil
qgroup = function(numvec, n = 5){
  qtile = quantile(numvec, probs = seq(0, 1, 1/n), na.rm=T)
  out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
  out=paste("Q",out,sep="") %>% factor(levels=paste("Q",1:n,sep=""))
  return(out)
}
