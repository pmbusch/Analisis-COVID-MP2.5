### Analisis-COVID-MP2.5
## Resumen Modelos Analisis Transversal
## Resume modelos corridos y genera excel resumen
## PBH Octubre 2020

# Librerias -----
library(tools)
library(broom) # to get info of fitted models easily
source("Scripts/00-Funciones.R", encoding = "UTF-8")


# Ruta --------
# url <- "Data/Data_Modelo/Modelos/"
url <- "Data/Data_Modelo/Modelos_AllCauses/"

file_name <- "ResumenModelos/ResumenModelos_%s.csv"


# Modelos a cargar -------
(modelos_rsd <- list.files(url))

# Para guardar info
df_params <- data.frame()
df_coef <- data.frame()

## Loop for all models --------
# By the moment I am unable to get summary of models with random components (3 in total)
for (m in modelos_rsd){
  cat("Modelo: ",m,"\n",sep="")
  if(file_ext(m)=="rsd" & !str_detect(m,"random")){
    # load model
    modelo <- read_rds(paste(url, m, sep=""))
    # summary(modelo)
    
    ## Parametros modelo
    param_modelo <- modelo %>% glance()
    # param_modelo
    
    ## Coeficientes (we are getting the MRR directly, with exponentiate)
    coef_modelo <- modelo %>% tidy(exponentiate = T,  conf.int = T)
    # coef_modelo
    
    # Store data
    df_params <- rbind(df_params, param_modelo %>% mutate(modelo=m))
    df_coef <- rbind(df_coef, coef_modelo %>% mutate(modelo=m))
    
    rm(modelo,param_modelo, coef_modelo)
  }
}
rm(m)

# Only for models of all causes
df_params <- df_params %>% 
  mutate(causa=f_split_n(modelo,"_",1) %>% str_remove("\\.rsd"),
         grupo_edad=f_split_n(modelo,"_",2) %>% str_remove("\\.rsd")) %>% 
  replace_na(list(grupo_edad="todos"))
df_coef <- df_coef %>% 
  mutate(causa=f_split_n(modelo,"_",1) %>% str_remove("\\.rsd"),
         grupo_edad=f_split_n(modelo,"_",2) %>% str_remove("\\.rsd")) %>% 
  replace_na(list(grupo_edad="todos"))


## Save data in Excel
# file_path <- sprintf(file_name,"param")
file_path <- sprintf(file_name,"paramAllCauses")
cat('sep=; \n',file = file_path)
write.table(df_params,file_path, sep=';',row.names = F, append = T)

# file_path <- sprintf(file_name,"coef")
file_path <- sprintf(file_name,"coefAllCauses")
cat('sep=; \n',file = file_path)
write.table(df_coef,file_path, sep=';',row.names = F, append = T)

rm(file_path,url,file_name, modelos_rsd)


##EoF