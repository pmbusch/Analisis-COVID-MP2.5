### Analisis-COVID-MP2.5
## Analisis Modelo Transversal mediante Stepwise
## PBH Octubre 2020

##  Carga Librerias ------
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)


# Load Data (a nivel de comuna)  -----------
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/05-FuncionesAnalisisTransversal.R", encoding = "UTF-8")
load(".RData")
file_name <- "Figuras/Analisis_transversal/%s.png"
file_mod <- "Data/Data_Modelo/Modelos/%s.rsd"


cat("Fecha informe DEIS muertes COVID: ",fecha_deis, sep="")
df_modelo %>% dim()
df_modelo %>% na.omit() %>% nrow() # dimension todas las comunas con todos los datos

## STEPWISE ------------
# https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection/20856#20856
library(caret)

## Creo df solo con variables numericas de interes (y fuera las COVID)
df_modelo %>% names() %>% sort()
df <-  df_modelo %>% 
  mutate(mp_10_minus25=mp10-mp25) %>% 
  dplyr::select(
    poblacion,
    covid_fallecidos ,
    # covid_fallecidos_65,covid_fallecidos_75,
    `15-44`,`45-64`,`65-74`,`65+`,`75+`,
    cons_lena_kg,  
    densidad_pob,densidad_pob_censal,
    densidad_pob_manzana_media, densidad_pob_manzana_mediana,
    densidad_pob_manzana_p90, 
    dias_cuarentena, dias_primerContagio, dias_primerMuerte,
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
    movilidad,
    mp25, mp_10_minus25,
    perc_FFAA, perc_fonasa_A, perc_fonasa_B, perc_fonasa_C,
    perc_fonasa_D, perc_isapre, perc_salud,
    perc_lenaAgua, perc_lenaCalefaccion, perc_lenaCocina, 
    perc_material_irrecuperable,perc_menor_media,
    perc_mujer, perc_puebloOrig, perc_rural,
    perc_ocupado, perc_vivAntes2002,perc_vivHacCritico,
    perc_vivHacMedio, perc_vivSinHac,
    tasa_camas,
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
df[,cols] %>% names()
## Keep covid_fallecidos and poblacion
cols <- cols[cols!=1 & cols!=2]
# Columnas remanentes
df[,-cols] %>% names()

df <- df[,-cols]

# eliminate variables with a low t-statistic
# rfe(df,df$covid_fallecidos, rfeControl=rfeControl(functions=lmFuncs))


## Train with ML
getModelInfo("glmStepAIC")
modelLookup("glmStepAIC")
# modelLookup("glmnet")


## Prepare model with an offset
## Fuente: https://stackoverflow.com/questions/61104205/how-can-i-train-a-glmnet-model-poisson-family-with-an-offset-term-using-the-ca

# AIC estimates the relative amount of information lost by a given model: 
# the less information a model loses, the higher the quality of that model
dat <- df %>% dplyr::select(-poblacion)
X = model.matrix(covid_fallecidos ~ ., data=dat)
Y = dat$covid_fallecidos
OFF = log(df$poblacion)

glm_fit <- caret::train(
  x = cbind(X,OFF),
  y = Y,
  method = "glmStepAIC",
  family = "poisson",
  link="log",
  na.action = na.omit
)

glm_fit
summary(glm_fit)
varImp(glm_fit)
f_tableMRR(glm_fit$finalModel, preview="none")
rm(glm_fit)
# NOTAS: Algoritmo step funciona con familia Poisson, y sin efectos aleatorios


# Poisson GLM peude dar el valor inicial del theta
# Theta is usually interpreted as a measure of overdispersion with respect to the Poisson distribution
# https://stats.stackexchange.com/questions/10419/what-is-theta-in-a-negative-binomial-regression-fitted-with-r

initial_mod <- glm.nb(covid_fallecidos~ . +offset(OFF),
                      data=dat,
                      na.action=na.omit)
summary(initial_mod)
initial_mod$theta

glm_fit_nb <- caret::train(
  x = cbind(X,OFF),
  y = Y,
  method = "glmStepAIC",
  penalty = c(rep(1,ncol(X)),0), ##Penalty zero for the offset term
  preProcess = c("scale"),
  family=negative.binomial(theta=initial_mod$theta,link="log"),
  na.action = na.omit
)

glm_fit_nb
summary(glm_fit_nb)
varImp(glm_fit_nb)
f_tableMRR(glm_fit_nb$finalModel, preview="none")

## Adjust same model
formula_step <- format(glm_fit_nb$finalModel$formula) %>% 
  str_replace(".outcome", "covid_fallecidos") %>% 
  str_replace_all("\\+",") + scale(") %>% 
  str_replace("OFF", "offset(log(poblacion))") %>% 
  str_replace("~","~scale(") %>% 
  str_remove_all(" ") %>% 
  str_replace("scale\\(offset","offset") %>% 
  formula()
formula_step

mod <- glm.nb(formula_step,
              data=df,
              init.theta=initial_mod$theta,
              na.action = na.omit)
summary(mod)
nobs(mod)
f_tableMRR(mod, preview = "none")
f_figMRR(mod)
f_savePlot(last_plot(), sprintf(file_name,"step"),dpi=150)
saveRDS(mod, sprintf(file_mod,"step"))

rm(glm_fit_nb, initial_mod, mod)

