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
    str_replace_all("Ñ","N")
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
