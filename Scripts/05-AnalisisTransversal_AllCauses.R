### Analisis-COVID-MP2.5
## Analisis Transversal Mortalidad todas las Causas
## PBH Septiembre 2020

## Librerias ------
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
# file_name <- "Figuras/Analisis_transversal/%s.png"
file_mod <- "Data/Data_Modelo/Modelos/%s.rsd"
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/05-FuncionesAnalisisTransversal.R", encoding = "UTF-8")

library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)

## Variables adicionales ---------
file_name <- "Figuras/Analisis_transversal/Modelos_Mortalidad_All/%s.png"
df_modelo %>% names() %>% sort()
df <- df_modelo %>% 
  mutate(mp25_10um=mp25/10, # para ver aumento en RR por 10ug/m3
         mp_10_minus25=mp10-mp25)

## Modelo Base. Y= Causas Cardiopulmonares -------------
# Notar que es poisson
mod_nb <- glm(def_cardioPulmonar ~ mp25_10um +mp_10_minus25+
                   scale(densidad_pob_censal) +
                   scale(`15-44`) + scale(`65+`) +
                   scale(perc_puebloOrig) +
                   scale(perc_rural) +
                   scale(tasa_camas) +
                   scale(perc_lenaCalefaccion) +
                   scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(perc_vivHacMedio)+
                   scale(hr_anual) +
                   scale(heating_degree_15_winter) +
                   offset(log(poblacion)), 
                 data = df,
                 na.action=na.omit,
              family = "poisson")

summary(mod_nb)
nobs(mod_nb)
f_tableMRR(mod_nb, preview = "none")
f_figMRR(mod_nb)
rm(mod_nb)

## ver step 
## densidad_poblacional: medida de hacinamiento/pobreza
## Leña calefaccion (composicion MP2.5)
## Ingreso
## Educacion
## Temperatura
## Pueblo originario

## Modelo Base Sign. Y= Causas CardioPulmonares -------------
mod_nb_sign <- glm(def_cardioPulmonar ~ 
                     mp25_10um +
                     scale(`15-44`) + scale(`65+`) +
                     scale(perc_rural) +
                     scale(perc_lenaCalefaccion) +
                     scale(log(ingresoAutonomo_media)) + 
                     scale(perc_menor_media) +
                     scale(perc_vivHacMedio)+
                     scale(tmed_anual) +
                     scale(heating_degree_15_winter) +
                     offset(log(poblacion)), 
                   data = df,
                   na.action=na.omit,
                   family="poisson")

summary(mod_nb_sign)
nobs(mod_nb_sign)
f_tableMRR(mod_nb_sign, preview = "none")
f_figMRR(mod_nb_sign)
rm(mod_nb_sign)

formula_base <- formula( def_cardioPulmonar ~ 
  mp25_10um +
  scale(`15-44`) + scale(`65+`) +
  scale(perc_rural) +
  scale(perc_lenaCalefaccion) +
  scale(log(ingresoAutonomo_media)) + 
  scale(perc_menor_media) +
  scale(perc_vivHacMedio)+
  scale(tmed_anual) +
  scale(heating_degree_15_winter) +
  offset(log(poblacion)))

reformulate(deparse(formula_base[[3]]), response = "def_cardio")

## MODELOS CON DISTINTA CAUSA DE DEFUNCION --------------
# Total deaths ----------
mod_total <- glm(reformulate(deparse(formula_base[[3]]), response = "def_total"), 
           data = df,
           na.action=na.omit,
           family="poisson")

summary(mod_total)
nobs(mod_total)
f_tableMRR(mod_total, preview = "none")
f_figMRR(mod_total)
rm(mod_total)

# All Cause deaths (no external) ----------
mod_allCauses <- glm(reformulate(deparse(formula_base[[3]]), response = "def_allCauses"), 
                 data = df,
                 na.action=na.omit,
                 family="poisson")

summary(mod_allCauses)
nobs(mod_allCauses)
f_tableMRR(mod_allCauses, preview = "none")
f_figMRR(mod_allCauses)
rm(mod_allCauses)


# External Cause deaths ----------
mod_extCauses <- glm(reformulate(deparse(formula_base[[3]]), response = "def_extCauses"), 
                    data = df,
                    na.action=na.omit,
                    family="poisson")

summary(mod_extCauses)
nobs(mod_extCauses)
f_tableMRR(mod_extCauses, preview = "none")
f_figMRR(mod_extCauses)
rm(mod_extCauses)

# Cardio ----------
mod_cardio <- glm(reformulate(deparse(formula_base[[3]]), response = "def_cardio"), 
                     data = df,
                     na.action=na.omit,
                     family="poisson")

summary(mod_cardio)
nobs(mod_cardio)
f_tableMRR(mod_cardio, preview = "none")
f_figMRR(mod_cardio)
rm(mod_cardio)

# Pulmonar ----------
mod_pulmonar <- glm(reformulate(deparse(formula_base[[3]]), response = "def_pulmonar"), 
                     data = df,
                     na.action=na.omit,
                     family="poisson")

summary(mod_pulmonar)
nobs(mod_pulmonar)
f_tableMRR(mod_pulmonar, preview = "none")
f_figMRR(mod_pulmonar)
rm(mod_pulmonar)

# Cancer ----------
mod_cancer <- glm(reformulate(deparse(formula_base[[3]]), response = "def_cancer"), 
                     data = df,
                     na.action=na.omit,
                     family="poisson")

summary(mod_cancer)
nobs(mod_cancer)
f_tableMRR(mod_cancer, preview = "none")
f_figMRR(mod_cancer)
rm(mod_cancer)



## Modelo Step sobre Y= Causas Cardiopulmonares ------------
# https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection/20856#20856
library(caret)

## Creo df solo con variables numericas de interes (y fuera las COVID)
df_modelo %>% names() %>% sort()
df <-  df_modelo %>% 
  mutate(mp_10_minus25=mp10-mp25) %>% 
  dplyr::select(
    poblacion,
    def_cardioPulmonar,
    `15-44`,`45-64`,`65-74`,`65+`,`75+`,
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
    mp25, mp_10_minus25,
    perc_FFAA, perc_fonasa_A, perc_fonasa_B, perc_fonasa_C,
    perc_fonasa_D, perc_isapre, perc_salud,
    perc_lenaAgua, perc_lenaCalefaccion, perc_lenaCocina, 
    perc_material_irrecuperable,perc_menor_media,
    perc_mujer, perc_puebloOrig, perc_rural,
    perc_ocupado, perc_vivAntes2002,perc_vivHacCritico,
    perc_vivHacMedio, perc_vivSinHac
  ) %>% 
  rename(e15_44=`15-44`,e45_64=`45-64`,e65_74=`65-74`,
         e65_plus=`65+`,e75_plus=`75+`) %>% 
  na.omit()
df %>% nrow() #Numero observaciones


# Columnas a remover dado que serian redundantes por su correlacion con otras variables
# identify and eliminate collinear variables
cols <- df %>% 
  cor() %>% 
  findCorrelation()
# Columnas fuera
df[,cols] %>% names() %>% sort()
## Keep def_cardiopulmonar and poblacion
cols <- cols[cols!=1 & cols!=2]
# Columnas remanentes
df[,-cols] %>% names() %>% sort()

df <- df[,-cols]


## Prepare model with an offset
## Fuente: https://stackoverflow.com/questions/61104205/how-can-i-train-a-glmnet-model-poisson-family-with-an-offset-term-using-the-ca

# AIC estimates the relative amount of information lost by a given model: 
# the less information a model loses, the higher the quality of that model
dat <- df %>% dplyr::select(-poblacion)
X = model.matrix(def_cardioPulmonar ~ ., data=dat)
Y = dat$def_cardioPulmonar
OFF = log(df$poblacion)

glm_fit <- caret::train(
  x = cbind(X,OFF),
  y = Y,
  method = "glmStepAIC",
  penalty = c(rep(1,ncol(X)),0), ##Penalty zero for the offset term
  preProcess = c("scale"),
  family = "poisson",
  link="log",
  na.action = na.omit
)

glm_fit
summary(glm_fit)
varImp(glm_fit)
f_tableMRR(glm_fit$finalModel, preview="none")
rm(glm_fit)

## Adjust same model
formula_step <- format(glm_fit$finalModel$formula) %>% 
  paste(collapse = "") %>% 
  str_replace(".outcome", "def_cardioPulmonar") %>% 
  str_replace_all("\\+",") + scale(") %>% 
  str_replace("~","~scale(") %>% 
  str_remove_all(" ") %>% 
  str_replace("scale\\(OFF", "offset(log(poblacion))") %>% 
  formula()
formula_step

mod <- glm(formula_step,
           data=df,
           family="poisson",
           na.action = na.omit)
summary(mod)
nobs(mod)
f_tableMRR(mod, preview = "none")
f_figMRR(mod)

rm(glm_fit, mod)

