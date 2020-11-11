### Analisis-COVID-MP2.5
## Pruebas Modelos: Analisis Transversal
## Modelo Binomial negativo con y sin efectos aleatorios
## PBH Septiembre 2020


## NOTA:
# df_modelo tiene todos los datos conjuntamente!
# df_modelo <- read_rds("Data/Data_Modelo/Datos_Modelo.rsd")
# Los siguientes DF tienen los datos separados por datos de comuna (estaticos)
# datos COVID y datos de defunciones generales
# JOIN ES POR codigo_comuna, usar left_join(, by=c("codigo_comuna"))
# df_datosComuna <- read_rds("Data/Data_Modelo/Datos_Comuna.rsd")
# df_datosCovid <- read_rds("Data/Data_Modelo/Datos_Covid.rsd")
# df_datosMortalidad <- read_rds("Data/Data_Modelo/Datos_Mortalidad.rsd")


# Load library ----------
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)

# Load Data ------------
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/05-FuncionesAnalisisTransversal.R", encoding = "UTF-8")
load(".RData")

## df modelo es la tabla de datos con todo a nivel de comuna
## Columnas incluidas
df_modelo %>% names() %>% sort()



## Modelo Prueba sin Efectos aleatorios--------
# Como voy a ajustar CFR como variable dependiente, debo crear un offset "auxiliar"
# de contagios
data_mod <- df_modelo %>% filter(cfr_raw_0_aplanados>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_raw_0_aplanados*100)

mod_nb <- glm.nb(covid_fallecidos ~ 
                   mp25 +
                   rm +
                   scale(`65+`) +
                   scale(perc_vivHacMedio) +
                   scale(perc_salud) +
                   scale(perc_vivAntes2002) +
                   scale(hr_anual) + 
                   scale(movilidad) +
                   scale(hdd15_winter_lenaCalefaccion) +
                   scale(def_cardioPulmonar) +
                   offset(log(contagios)),
                 data = data_mod,
                 na.action=na.omit)

# Analisis del modelo ajustado
summary(mod_nb) #Resumen genela
nobs(mod_nb) # Numero observaciones
f_tableCoef(mod_nb) # Tabla Coeficientes ajustados


## MRR
# Se interpreta como el aumento relativo en la tasa de mortalidad covid por 1 ug/m3
# Dado que los confundentes estan estandarizados, su MRR se interepreta como 
# variacion relativa al aumento en 1 desviacion estandar de la variable confundente
# Fuente: https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
f_tableMRR(mod_nb, preview = "none",highlight = T) # Tabla MRR
f_figMRR(mod_nb) # Figuras MRR

# Borra modelo ajustado
rm(mod_nb)

## Modelo Prueba con Efectos aleatorios (Region)--------
# En este ejemplo se ajusta la tasa de mortalidad (muertes/poblacion)
mod_nb_er <- glmer.nb(covid_fallecidos ~ 
                   mp25 +
                   scale(densidad_pob) +
                   scale(`15-44`) + scale(`65+`) +
                   scale(perc_puebloOrig) + scale(perc_rural) +
                   scale(dias_primerMuerte) + 
                   scale(tasa_camas) + 
                   scale(perc_lenaCocina) + 
                   scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(tmed_summer) + scale(tmed_winter) + 
                   scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                   (1|region)+
                   offset(log(poblacion)),
                 data = df_modelo,
                 na.action=na.omit)

# Analisis del modelo ajustado
summary(mod_nb_er) #Resumen genela
# Coeficientes
coefficients(mod_nb_er)
confint(mod_nb_er, method="Wald", level=0.95) %>% exp() %>% round(2) # Intervalo de confianza

nobs(mod_nb_er) # Numero observaciones
ngrps(mod_nb_er) # number of levels of the Subject grouping factor
sigma(mod_nb_er) # residual standard deviation
formula(mod_nb_er)
ranef(mod_nb_er) # Coeficients of grouping variable, condicionados
#  Varianza random effects Si es muy cercana a cero un modelo si en este efecto es casi igual
print(vc <- VarCorr(mod), comp = c("Variance","Std.Dev."))

f_tableCoef(mod_nb_er) # Tabla Coeficientes ajustados
f_tableMRR(mod_nb_er) # Tabla MRR
f_figMRR(mod_nb_er) # Figuras MRR

## Residuales
residuals(mod_nb_er, scaled=T)
quantile(residuals(mod_nb_er, "pearson", scaled = TRUE))

# Diagnostic plots 
plot(mod_nb_er,type=c("p","smooth")) ## fitted vs residual
plot(mod_nb_er,sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth")) ## scale-location
lattice::qqmath(mod_nb_er,id=0.05) ## quantile-quantile

# Borra modelo ajustado
rm(mod_nb_er)

## Prediction ------
# type=responde https://stackoverflow.com/questions/47486589/what-is-the-difference-between-type-response-terms-and-link-in-predict-f
# type=link entrega el logaritmo natural de la tasa de mortalidad (variable dependiente)
# MODELO PREDICE CASOS FALLECIDOS!
# df %>% 
#   mutate(y=predict(mod_nb_er,type="response"),
#          res=covid_fallecidos-y,
#          residuals=residuals(mod_nb_er, type="response")) %>% 
#   dplyr::select(nombre_comuna,poblacion,covid_fallecidos,y,res,residuals,
#                 mp25,densidad_pob, tasa_mortalidad_covid) %>% view()

# comparacion prediccion
df %>% 
  ggplot(aes(x = mp25, y = covid_fallecidos/poblacion*1e5)) +
  geom_point(col="green",alpha=.5) +
  geom_line(aes(y = predict(mod_nb_er,type="response")/poblacion*1e5),
            size=1, col="red", alpha=.5)+
  labs(x="Concentración MP2.5 2016-2019 [ug/m3]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]")

df %>% 
  na.omit() %>% 
  mutate(rm=if_else(region=="M","RM","Resto Chile") %>% factor()) %>% 
  ggplot(aes(x = covid_fallecidos/poblacion*1e5)) +
  geom_point(aes(y = predict(mod_nb_er,type="response")/poblacion*1e5,
                 col=rm),
             # col="red", 
             size=4,
             alpha=.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  coord_cartesian(xlim=c(0,250),ylim=c(0,250), expand = T)+
  labs(x="Observada", y="Predicción", col="")+
  # ggtitle("Tasa Mortalidad COVID [muertes/100mil hab]")+
  theme(panel.grid.minor = element_blank())
f_savePlot(last_plot(), sprintf(file_name,"Obs_vs_Pred"), dpi=300)

## Mapa comparativo 
# detach("package:gamm4", unload = TRUE)
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")
df_map <- df %>%
  na.omit() %>% 
  mutate(pred=predict(mod_nb_er, type="response")/poblacion*1e5) %>% 
  left_join(codigos_territoriales) %>% 
  dplyr::select(codigo_comuna, covid_fallecidos,poblacion, pred) %>% 
  mutate(obs=covid_fallecidos/poblacion*1e5) %>% 
  right_join(mapa_comuna) %>% 
  gather(tipo,tasa,obs, pred)

df_map %>% 
  fig_mapaChile_facet(tasa, facets = ~tipo, limites=c(0,500),
                      titulo="Tasa Mortalidad COVID [muertes/100mil hab]")
f_savePlot(last_plot(), sprintf(file_name,"ModeloChile"))
rm(df_map)

## Comparacion con Poisson -----------
# BinNeg vs Poisson
# https://stats.stackexchange.com/questions/311556/help-interpreting-count-data-glmm-using-lme4-glmer-and-glmer-nb-negative-binom
modelformula <- formula(covid_fallecidos ~ 
                          mp25 +
                          scale(densidad_pob) + scale(`15-44`) + scale(`65+`) +
                          scale(perc_puebloOrig) + scale(perc_rural) +
                          scale(dias_primerContagio) +  scale(dias_cuarentena) + 
                          scale(tasa_camas) + 
                          scale(perc_lenaCalefaccion) + 
                          scale(log(ingresoTotal_media)) + scale(perc_menor_media) + 
                          scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                          scale(tmed_summer) + scale(tmed_winter) + 
                          scale(heating_degree_15_summer) + scale(heating_degree_15_winter) +
                          offset(log(poblacion)))
poismodel <- glm(modelformula, data = df, family = "poisson")   
nbmodel <- glm.nb(modelformula, data = df)
library(lmtest)
# Poisson vs Negative Binomial
lrtest(poismodel, nbmodel)
# Neg-Bin is a better fit

# Negative Binomial vs Mixed Model
lrtest(nbmodel, mod_nb_er)
# Mixed Model is a better fit

rm(modelformula, poismodel, nbmodel)

## EoF