### Analisis-COVID-MP2.5
## Analisis Regresion MP2.5
## PBH Octubre  2020

## Librerias ------
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figuras/Analisis_Transversal/Modelos_Mortalidad_All/%s.png"
file_mod <- "Data/Data_Modelo/Modelos_AllCauses/%s.rsd"
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/05-FuncionesAnalisisTransversal.R", encoding = "UTF-8")

## Variables adicionales ---------
file_name <- "Figuras/Analisis_transversal/Modelos_Mortalidad_All/%s.png"
df_modelo %>% names() %>% sort()
df <- df_modelo %>% 
  mutate(mp25_10um=mp25/10, # para ver aumento en RR por 10ug/m3
         mp10_minus25=mp10-mp25)


## Regresion Multiple. Y=MP2.5 -----------
mod_lm <- lm(log(mp25)~
               # mp10_minus25+
               scale(perc_lenaCalefaccion) +
               scale(hr_anual) +
               scale(heating_degree_15_winter) +
               scale(tmed_winter)+
               scale(cons_lena_kg),
             data=df,
             na.action=na.omit
               )

summary(mod_lm)
nobs(mod_lm)
f_tableCoef(mod_lm, preview = "none", highlight = T)
f_tableMRR(mod_lm, preview = "none", highlight = T)


## Modelo Step sobre Y= MP2.5 ------------
# https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection/20856#20856
library(caret)

## Creo df solo con variables numericas de interes (y fuera las COVID)
df_modelo %>% names() %>% sort()
df <-  df_modelo %>% 
  mutate(mp10_minus25=mp10-mp25) %>% 
  dplyr::select(
    mp25,
    poblacion,
    cons_lena_kg,  
    densidad_pob,densidad_pob_censal,
    densidad_pob_manzana_media, densidad_pob_manzana_mediana,
    densidad_pob_manzana_p90, 
    hdd15_winter_lenaCalefaccion, heating_degree_15_anual,
    heating_degree_15_fall,   heating_degree_15_spring,
    heating_degree_15_summer, heating_degree_15_winter,
    heating_degree_18_anual,  heating_degree_18_fall,
    heating_degree_18_spring, heating_degree_18_summer,
    heating_degree_18_winter, hr_anual,
    hr_fall, hr_spring, hr_summer, hr_winter,
    tmed_anual,  tmed_fall, tmed_spring, tmed_summer,tmed_winter,
    ingresoAutonomo_media,ingresoAutonomo_mediana,
    ingresoTotal_media,   ingresoTotal_mediana,
    mp10_minus25,
    perc_lenaAgua, perc_lenaCalefaccion, perc_lenaCocina, 
    perc_material_irrecuperable,perc_menor_media,
    perc_mujer, perc_puebloOrig, perc_rural,
    perc_ocupado, perc_vivAntes2002,perc_vivHacCritico,
    perc_vivHacMedio, perc_vivSinHac
  ) %>% 
  na.omit()
df %>% nrow() #Numero observaciones


# Columnas a remover dado que serian redundantes por su correlacion con otras variables
# identify and eliminate collinear variables
cols <- df %>% 
  cor() %>% 
  findCorrelation()
# Columnas fuera
df[,cols] %>% names() %>% sort()
## Keep mp2.5
cols <- cols[cols!=1]
# Columnas remanentes
df[,-cols] %>% names() %>% sort()

df <- df[,-cols]

## Prepare model with an offset
## Fuente: https://stackoverflow.com/questions/61104205/how-can-i-train-a-glmnet-model-poisson-family-with-an-offset-term-using-the-ca

# AIC estimates the relative amount of information lost by a given model: 
# the less information a model loses, the higher the quality of that model

X = model.matrix(mp25 ~ ., data=df)
Y = log(df$mp25)

glm_fit <- caret::train(
  x = X,
  y = Y,
  method = "glmStepAIC",
  preProcess = c("scale"),
  family = "gaussian",
  na.action = na.omit
)

glm_fit
summary(glm_fit)
varImp(glm_fit)
f_tableCoef(glm_fit$finalModel, preview="none", highlight = T)
f_tableMRR(glm_fit$finalModel, preview="none", highlight = T)
rm(glm_fit)



## EoF
