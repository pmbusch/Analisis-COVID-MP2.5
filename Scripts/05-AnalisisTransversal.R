### Analisis-COVID-MP2.5
## Analisis Transversal
## Fuente principal codigo: Exposure to air pollution and COVID-19 mortality in the United States A nationwide cross-sectional study
## https://github.com/wxwx1993/PM_COVID
## Fuente: Bates, D., Mächler, M., Bolker, B., & Walker, S. (2014). Fitting linear mixed-effects models using lme4. arXiv preprint arXiv:1406.5823.
## PBH Julio 2020

## Load Data
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
df_modelo %>% na.omit() %>% nrow() # dimension todas las variables


## Y: TASA DE MORTALIDAD COVID (MUERTES/POBLACION) --------------
## MODELO DEFAULT: Binomial Negativo-----
# Variables se seleccionaron con ayuda de Step, pero con logica externa, 
# segun cuales podrian ser explicativas (y usadas en otros estudios)

## Poblacion va a off_set, dado que se estima la tasa
# Buena explicacion: https://stats.stackexchange.com/questions/11182/when-to-use-an-offset-in-a-poisson-regression
mod_nb <- glm.nb(covid_fallecidos ~ 
                   mp25 +
                   rm +
                   scale(densidad_pob_censal) +
                   scale(`15-44`) + scale(`65+`) +
                   scale(perc_mujer) +
                   scale(def_cardioPulmonar) +
                   scale(perc_puebloOrig) +
                   scale(perc_rural) +
                   scale(dias_primerMuerte) +
                   scale(tasa_camas) +
                   scale(perc_lenaCalefaccion) +
                   scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(perc_vivHacMedio)+
                   scale(hr_anual) +
                   scale(heating_degree_15_winter) +
                   offset(log(poblacion)), 
                 data = df_modelo,
                 na.action=na.omit)

summary(mod_nb)
nobs(mod_nb)
f_tableCoef(mod_nb)
f_tableMRR(mod_nb, preview = "none", highlight = T)
# f_tableMRR(mod_nb,preview = "docx")
# f_tableMRR(mod_nb,preview = "pptx")
f_figMRR(mod_nb)
f_savePlot(last_plot(), sprintf(file_name,"Base"),dpi=150)
saveRDS(mod_nb, sprintf(file_mod,"Base"))
rm(mod_nb)


file_name <- "Figuras/Analisis_transversal/Otros_Modelos/%s.png"

## Base Solo Significativas---------
mod_nb_sig <- glm.nb(covid_fallecidos ~ 
                       mp25 +
                       rm +
                       scale(`15-44`) +
                       scale(perc_puebloOrig) + scale(perc_rural) +
                       scale(dias_primerMuerte) +
                       scale(tasa_camas) +
                       scale(perc_vivHacMedio)+
                       scale(hr_anual) +
                       scale(heating_degree_15_winter) +
                       offset(log(poblacion)), 
                     data = df_modelo,
                     na.action=na.omit)
summary(mod_nb_sig)
nobs(mod_nb_sig)
f_tableCoef(mod_nb_sig)
f_tableMRR(mod_nb_sig, preview = "none", highlight = T) 
f_figMRR(mod_nb_sig)
f_savePlot(last_plot(), sprintf(file_name,"BaseSign"),dpi=150)
saveRDS(mod_nb_sig, sprintf(file_mod,"BaseSign"))
rm(mod_nb_sig)

## Sin MP2.5---------
mod_sinMP <- glm.nb(covid_fallecidos ~ 
                      rm +
                        scale(densidad_pob_censal) +
                        scale(`15-44`) + scale(`65+`) +
                        scale(perc_puebloOrig) + scale(perc_rural) +
                        scale(dias_primerMuerte) +
                        scale(tasa_camas) +
                        scale(perc_lenaCalefaccion) +
                        scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                        scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                        scale(perc_vivHacMedio)+
                        scale(hr_anual) +
                        scale(heating_degree_15_winter) +
                        offset(log(poblacion)), 
                data = df_modelo,
                na.action=na.omit)
summary(mod_sinMP)
nobs(mod_sinMP)
f_tableCoef(mod_sinMP)
f_tableMRR(mod_sinMP, preview = "none") 
f_figMRR(mod_sinMP)
f_savePlot(last_plot(), sprintf(file_name,"sinMP"),dpi=150)
saveRDS(mod_sinMP, sprintf(file_mod,"sin_MP"))
rm(mod_sinMP)

## Sin RM---------
mod_sinRM <- glm.nb(covid_fallecidos ~ 
                        mp25 +
                        scale(densidad_pob_censal) +
                        scale(`15-44`) + scale(`65+`) +
                        scale(perc_puebloOrig) + scale(perc_rural) +
                        scale(dias_primerMuerte) +
                        scale(tasa_camas) +
                        scale(perc_lenaCalefaccion) +
                        scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                        scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                        scale(perc_vivHacMedio)+
                        scale(hr_anual) +
                        scale(heating_degree_15_winter) +
                        offset(log(poblacion)), 
                      data = df_modelo %>% filter(region!="Metropolitana"),
                      na.action=na.omit)
summary(mod_sinRM)
nobs(mod_sinRM)
f_tableCoef(mod_sinRM)
f_tableMRR(mod_sinRM, preview = "none") 
f_figMRR(mod_sinRM)
f_savePlot(last_plot(), sprintf(file_name,"sinRM"),dpi=150)
saveRDS(mod_sinRM, sprintf(file_mod,"sin_RM"))
rm(mod_sinRM)

## Solo RM---------
mod_RM <- glm.nb(covid_fallecidos ~ 
                   mp25 +
                   scale(densidad_pob_censal) +
                   scale(`15-44`) + scale(`65+`) +
                   scale(perc_puebloOrig) + scale(perc_rural) +
                   scale(dias_primerMuerte) +
                   scale(tasa_camas) +
                   scale(perc_lenaCalefaccion) +
                   scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(perc_vivHacMedio)+
                   scale(hr_anual) +
                   scale(heating_degree_15_winter)+
                   offset(log(poblacion)),
                 data = df_modelo %>% filter(region=="Metropolitana"),
                      na.action=na.omit)
summary(mod_RM)
nobs(mod_RM)
exp(summary(mod_RM)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_RM)
f_tableMRR(mod_RM, preview = "none")
f_figMRR(mod_RM)
f_savePlot(last_plot(), sprintf(file_name,"soloRM"),dpi=150)
saveRDS(mod_RM, sprintf(file_mod,"soloRM"))
rm(mod_RM)


## Sin RM significativo---------
mod_sinRM_sign <- glm.nb(covid_fallecidos ~ 
                           mp25 +
                           scale(`15-44`) +
                           scale(perc_puebloOrig) + scale(perc_rural) +
                           scale(dias_primerMuerte) +
                           scale(tasa_camas) +
                           scale(perc_vivHacMedio)+
                           scale(hr_anual) +
                           scale(heating_degree_15_winter) +
                           offset(log(poblacion)), 
                         data = df_modelo %>% filter(region!="Metropolitana"),
                         na.action=na.omit)
summary(mod_sinRM_sign)
nobs(mod_sinRM_sign)
f_tableCoef(mod_sinRM_sign)
f_tableMRR(mod_sinRM_sign, preview = "none")
f_figMRR(mod_sinRM_sign)
f_savePlot(last_plot(), sprintf(file_name,"sinRM_sign"),dpi=150)
saveRDS(mod_sinRM_sign, sprintf(file_mod,"sinRM_sign"))
rm(mod_sinRM_sign)

## Solo RM significativo---------
mod_RM_sign <- glm.nb(covid_fallecidos ~ 
                        mp25 +
                        scale(`15-44`) +
                        scale(perc_puebloOrig) + scale(perc_rural) +
                        scale(dias_primerMuerte) +
                        scale(tasa_camas) +
                        scale(perc_vivHacMedio)+
                        scale(hr_anual) +
                        scale(heating_degree_15_winter) +
                        offset(log(poblacion)),
                      data = df_modelo %>% filter(region=="Metropolitana"),
                      na.action=na.omit)
summary(mod_RM_sign)
nobs(mod_RM_sign)
f_tableCoef(mod_RM_sign)
f_tableMRR(mod_RM_sign, preview = "none") 
f_figMRR(mod_RM_sign)
f_savePlot(last_plot(), sprintf(file_name,"soloRM_sign"),dpi=150)
saveRDS(mod_RM_sign, sprintf(file_mod,"soloRM_sign"))
rm(mod_RM_sign)

## MP2.5 Winter---------
mod_mp25winter<- glm.nb(covid_fallecidos ~ 
                          mp25_winter +
                          rm +
                          scale(densidad_pob_censal) +
                          scale(`15-44`) + scale(`65+`) +
                          scale(perc_puebloOrig) + scale(perc_rural) +
                          scale(dias_primerMuerte) +
                          scale(tasa_camas) +
                          scale(perc_lenaCalefaccion) +
                          scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                          scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                          scale(perc_vivHacMedio)+
                          scale(hr_anual) +
                          scale(heating_degree_15_winter) +
                          offset(log(poblacion)), 
                      data = df_modelo,
                      na.action=na.omit)
summary(mod_mp25winter)
nobs(mod_mp25winter)
f_tableCoef(mod_mp25winter)
f_tableMRR(mod_mp25winter, preview = "none")
f_figMRR(mod_mp25winter)
f_savePlot(last_plot(), sprintf(file_name,"MP25Winter"),dpi=150)
saveRDS(mod_mp25winter, sprintf(file_mod,"MP25Winter"))
rm(mod_mp25winter)

## Solo MP2.5---------
modMP <- glm.nb(covid_fallecidos ~ mp25 +
                    offset(log(poblacion)),
                  data = df_modelo,
                  na.action=na.omit)
summary(modMP)
nobs(modMP)
f_tableCoef(modMP)
f_tableMRR(modMP)
f_figMRR(modMP)
f_savePlot(last_plot(), sprintf(file_name,"soloMP"),dpi=150)
saveRDS(modMP, sprintf(file_mod,"soloMP"))
rm(modMP)

## MP2.5 2019 --------
mod_mp_2019 <- glm.nb(covid_fallecidos ~ 
                        mp25_2019 +
                        rm +
                        scale(densidad_pob_censal) +
                        scale(`15-44`) + scale(`65+`) +
                        scale(perc_puebloOrig) +
                        scale(perc_rural) +
                        scale(dias_primerMuerte) +
                        scale(tasa_camas) +
                        scale(perc_lenaCalefaccion) +
                        scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                        scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                        scale(perc_vivHacMedio)+
                        scale(hr_anual) +
                        scale(heating_degree_15_winter) +
                        offset(log(poblacion)), 
                      data = df_modelo,
                      na.action=na.omit)

summary(mod_mp_2019)
nobs(mod_mp_2019)
exp(summary(mod_mp_2019)$coefficients[2,1]) # exponencial coeficiente MP2.5
f_tableCoef(mod_mp_2019)
f_tableMRR(mod_mp_2019, preview = "none")
f_figMRR(mod_mp_2019)
f_savePlot(last_plot(), sprintf(file_name,"mp25_2019"),dpi=150)
saveRDS(mod_mp_2019, sprintf(file_mod,"mp25_2019"))
rm(mod_mp_2019)


## MP10 --------
mod_mp10 <- glm.nb(covid_fallecidos ~ 
                     mp10 +
                     rm +
                     scale(densidad_pob_censal) +
                     scale(`15-44`) + scale(`65+`) +
                     scale(perc_puebloOrig) +
                     scale(perc_rural) +
                     scale(dias_primerMuerte) +
                     scale(tasa_camas) +
                     scale(perc_lenaCalefaccion) +
                     scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                     scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                     scale(perc_vivHacMedio)+
                     scale(hr_anual) +
                     scale(heating_degree_15_winter) +
                     offset(log(poblacion)), 
                   data = df_modelo,
                   na.action=na.omit)

summary(mod_mp10)
nobs(mod_mp10)
f_tableCoef(mod_mp10)
f_tableMRR(mod_mp10, preview = "none")
f_figMRR(mod_mp10)
f_savePlot(last_plot(), sprintf(file_name,"mp10"),dpi=150)
saveRDS(mod_mp10, sprintf(file_mod,"mp10"))
rm(mod_mp10)

## Fallecidos 65+---------
mod_65 <- glm.nb(covid_fallecidos_65 ~ 
                     mp25 +
                     rm +
                     scale(densidad_pob_censal) +
                     scale(`15-44`) + 
                     scale(perc_puebloOrig) + scale(perc_rural) +
                     scale(dias_primerMuerte) +
                     scale(tasa_camas) +
                     scale(perc_lenaCalefaccion) +
                     scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                     scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                     scale(perc_vivHacMedio)+
                     scale(hr_anual) +
                     scale(heating_degree_15_winter) +
                     offset(log(pob65)),
                   data = df_modelo %>% mutate(pob65=poblacion*`65+`),
                   na.action=na.omit)
summary(mod_65)
f_tableCoef(mod_65)
f_tableMRR(mod_65, preview = "none") 
f_figMRR(mod_65)
f_savePlot(last_plot(), sprintf(file_name,"fallecidos65"),dpi=150)
saveRDS(mod_65, sprintf(file_mod,"fallecidos65"))
rm(mod_65)


## Proxy Leña---------
mod_lena <- glm.nb(covid_fallecidos ~ 
                     mp25 + rm +
                     scale(densidad_pob_censal) +
                     scale(`15-44`) + scale(`65+`) +
                     scale(perc_puebloOrig) + scale(perc_rural) +
                     scale(dias_primerMuerte) +
                     scale(tasa_camas) +
                     scale(perc_lenaCalefaccion) +
                     scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                     scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                     scale(perc_vivHacMedio)+
                     scale(hr_anual) +
                     scale(heating_degree_15_winter) +
                     scale(hdd15_winter_lenaCalefaccion) +
                     offset(log(poblacion)),
                   data = df_modelo,
                   na.action=na.omit)
summary(mod_lena)
nobs(mod_lena)
f_tableCoef(mod_lena)
f_tableMRR(mod_lena, preview = "none")
f_figMRR(mod_lena)
f_savePlot(last_plot(), sprintf(file_name,"proxyLena"),dpi=150)
saveRDS(mod_lena, sprintf(file_mod,"proxyLena"))
rm(mod_lena)


## PDA ---------
mod_pda <- glm.nb(covid_fallecidos ~ 
                    mp25 +
                    rm + pda +
                    scale(densidad_pob_censal) +
                    scale(`15-44`) + scale(`65+`) +
                    scale(perc_puebloOrig) + scale(perc_rural) +
                    scale(dias_primerMuerte) +
                    scale(tasa_camas) +
                    scale(perc_lenaCalefaccion) +
                    scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                    scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                    scale(perc_vivHacMedio)+
                    scale(hr_anual) +
                    scale(heating_degree_15_winter) +
                    offset(log(poblacion)),
                  data = df_modelo,
                  na.action=na.omit)
summary(mod_pda)
nobs(mod_pda)
f_tableCoef(mod_pda)
f_tableMRR(mod_pda, preview = "none")
f_figMRR(mod_pda)
f_savePlot(last_plot(), sprintf(file_name,"pda"),dpi=150)
saveRDS(mod_pda, sprintf(file_mod,"pda"))
rm(mod_pda)

## Consumo Urbano Leña (n=45) ---------
mod_lenaUrbano <- glm.nb(covid_fallecidos ~ 
                           mp25 +
                           scale(`15-44`) +
                           scale(perc_puebloOrig) + scale(perc_rural) +
                           scale(dias_primerMuerte) +
                           scale(tasa_camas) +
                           scale(perc_vivHacMedio)+
                           # scale(perc_lenaCalefaccion)+
                           scale(hr_anual) +
                           scale(heating_degree_15_winter) +
                           scale(cons_lena_urbana) +
                           offset(log(poblacion)),
                         data = df_modelo,
                         na.action=na.omit)
summary(mod_lenaUrbano)
nobs(mod_lenaUrbano)
f_tableCoef(mod_lenaUrbano)
f_tableMRR(mod_lenaUrbano, preview = "none")
f_figMRR(mod_lenaUrbano)
f_savePlot(last_plot(), sprintf(file_name,"lenaUrbano"),dpi=150)
saveRDS(mod_lenaUrbano, sprintf(file_mod,"lenaUrbano"))
rm(mod_lenaUrbano)

## Modelos con INTERCEPTO ALEATORIO ----------
## Random Region---------
mod_region <- glmer.nb(covid_fallecidos ~ 
                         mp25 +
                         scale(densidad_pob_censal) +
                         scale(`15-44`) + scale(`65+`) +
                         scale(perc_puebloOrig) + scale(perc_rural) +
                         scale(dias_primerMuerte) +
                         scale(tasa_camas) +
                         scale(perc_lenaCalefaccion) +
                         scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                         scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                         scale(perc_vivHacMedio)+
                         scale(hr_anual) +
                         scale(heating_degree_15_winter) +
                         (1|region)+
                         offset(log(poblacion)), 
                       data = df_modelo,
                       na.action=na.omit)
# Solucion no convergencia
# ss <- getME(mod,c("theta","fixef"))
# # mod2 <- update(mod,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))
# # summary(mod2)
# mod_region3 <- update(mod,start=ss,control=glmerControl(optimizer="bobyqa",
#                                                         optCtrl=list(maxfun=2e5)))
# summary(mod_region3)
# mod_region <- mod_region3

summary(mod_region)
ranef(mod_region)
f_tableCoef(mod_region)
f_tableMRR(mod_region, preview = "none")
f_figMRR(mod_region)
f_savePlot(last_plot(), sprintf(file_name,"randomRegion"),dpi=150)
saveRDS(mod_region, sprintf(file_mod,"randomRegion"))
rm(mod_region)

## Random por Provincia---------
modProv <- glmer.nb(covid_fallecidos ~ 
                      mp25 +
                      scale(densidad_pob_censal) +
                      scale(`15-44`) + scale(`65+`) +
                      scale(perc_puebloOrig) + scale(perc_rural) +
                      scale(dias_primerMuerte) +
                      scale(tasa_camas) +
                      scale(perc_lenaCalefaccion) +
                      scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                      scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                      scale(perc_vivHacMedio)+
                      scale(hr_anual) +
                      scale(heating_degree_15_winter) +
                      (1|nombre_provincia)+
                      offset(log(poblacion)),
                data = df_modelo,
                na.action=na.omit)
summary(modProv)
f_tableCoef(modProv)
f_tableMRR(modProv, preview = "none")
f_figMRR(modProv)
f_savePlot(last_plot(), sprintf(file_name,"randomProvincia"),dpi=150)
saveRDS(modProv, sprintf(file_mod,"randomProvincia"))
rm(modProv)


## Random Zonas---------
mod_zona <- glmer.nb(covid_fallecidos ~ 
                       mp25 +
                       scale(densidad_pob_censal) +
                       scale(`15-44`) + scale(`65+`) +
                       scale(perc_puebloOrig) + scale(perc_rural) +
                       scale(dias_primerMuerte) +
                       scale(tasa_camas) +
                       scale(perc_lenaCalefaccion) +
                       scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                       scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                       scale(perc_vivHacMedio)+
                       scale(hr_anual) +
                       scale(heating_degree_15_winter) +
                       (1|zona)+
                       offset(log(poblacion)), 
                     data = df_modelo,
                     na.action=na.omit)
summary(mod_zona)
ranef(mod_zona)
f_tableCoef(mod_zona)
f_tableMRR(mod_zona, preview = "none")
f_figMRR(mod_zona)
f_savePlot(last_plot(), sprintf(file_name,"randomZonas"),dpi=150)
saveRDS(mod_zona, sprintf(file_mod,"randomZonas"))
rm(mod_zona)

## Random Zona Termica---------
mod_zonaTermica <- glmer.nb(covid_fallecidos ~ 
                              mp25 +
                              scale(densidad_pob_censal) +
                              scale(`15-44`) + scale(`65+`) +
                              scale(perc_puebloOrig) + scale(perc_rural) +
                              scale(dias_primerMuerte) +
                              scale(tasa_camas) +
                              scale(perc_lenaCalefaccion) +
                              scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                              scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                              scale(perc_vivHacMedio)+
                              scale(hr_anual) +
                              scale(heating_degree_15_winter) +
                              (1|zona_termica)+
                              offset(log(poblacion)), 
                     data = df_modelo,
                     na.action=na.omit)
summary(mod_zonaTermica)
ranef(mod_zonaTermica)
f_tableCoef(mod_zonaTermica)
f_tableMRR(mod_zonaTermica, preview = "none")
f_figMRR(mod_zonaTermica)
f_savePlot(last_plot(), sprintf(file_name,"randomZonaTermica"),dpi=150)
saveRDS(mod_zonaTermica, sprintf(file_mod,"randomZonaTermica"))
rm(mod_zonaTermica)

## MODELOS CR2 ------------
## Modelo CR2---------
mod_nb_cr2 <- glm.nb(covid_fallecidos ~ 
                       mp25 + 
                       rm +
                       scale(`65+`) +
                       scale(perc_vivHacMedio) +
                       scale(perc_salud) +
                       scale(perc_vivAntes2002) +
                       scale(hr_anual) + 
                       scale(movilidad) +
                       offset(log(poblacion)), 
                     data = df_modelo,
                     na.action=na.omit)
summary(mod_nb_cr2)
nobs(mod_nb_cr2)
f_tableCoef(mod_nb_cr2)
f_tableMRR(mod_nb_cr2, preview = "none")  
f_figMRR(mod_nb_cr2)
f_savePlot(last_plot(), sprintf(file_name,"CR2"),dpi=150)
saveRDS(mod_nb_cr2, sprintf(file_mod,"CR2"))
rm(mod_nb_cr2)


## Modelo CR2 + Leña ---------
mod_nb_cr2_lena <- glm.nb(covid_fallecidos ~ 
                       mp25 +
                       rm +
                       scale(`65+`) +
                       scale(perc_vivHacMedio) +
                       scale(perc_salud) +
                       scale(perc_vivAntes2002) +
                       scale(hr_anual) + 
                       scale(movilidad) +
                       scale(hdd15_winter_lenaCalefaccion) +
                       offset(log(poblacion)), 
                     data = df_modelo,
                     na.action=na.omit)
summary(mod_nb_cr2_lena)
nobs(mod_nb_cr2_lena)
f_tableCoef(mod_nb_cr2_lena)
f_tableMRR(mod_nb_cr2_lena, preview = "none")
f_figMRR(mod_nb_cr2_lena)
f_savePlot(last_plot(), sprintf(file_name,"CR2_lena"),dpi=150)
saveRDS(mod_nb_cr2_lena, sprintf(file_mod,"CR2_lena"))
rm(mod_nb_cr2_lena)


## Y: CFR COVID (MUERTES/CASOS) --------------
# CFR se puede calcular de multiples maneras

## Y: % Letalidad acumulado---------
mod_letalidad <- glm.nb(covid_fallecidos ~ 
                          mp25 +
                          rm +
                          scale(densidad_pob_censal) +
                          scale(`15-44`) + scale(`65+`) +
                          scale(perc_puebloOrig) + scale(perc_rural) +
                          scale(dias_primerMuerte) +
                          scale(tasa_camas) +
                          scale(perc_lenaCalefaccion) +
                          scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                          scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                          scale(perc_vivHacMedio)+
                          scale(hr_anual) +
                          scale(heating_degree_15_winter) +
                          offset(log(casos_confirmados)),
                        data = df_modelo,
                        na.action=na.omit)
summary(mod_letalidad)
nobs(mod_letalidad)
f_tableCoef(mod_letalidad)
f_tableMRR(mod_letalidad, preview = "none")
f_figMRR(mod_letalidad)
f_savePlot(last_plot(), sprintf(file_name,"Y_letalidad"),dpi=150)
saveRDS(mod_letalidad, sprintf(file_mod,"Y_letalidad"))
rm(mod_letalidad)

## CFR Lag 0 Bruto---------
data_mod <- df_modelo %>% filter(cfr_raw_0>0 & covid_fallecidos>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_raw_0*100)
mod_cfr_raw_0 <- glm.nb(covid_fallecidos ~ 
                          mp25 + rm +
                          scale(densidad_pob_censal) +
                          scale(`15-44`) + scale(`65+`) +
                          scale(perc_puebloOrig) + scale(perc_rural) +
                          scale(dias_primerMuerte) +
                          scale(tasa_camas) +
                          scale(perc_lenaCalefaccion) +
                          scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                          scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                          scale(perc_vivHacMedio)+
                          scale(hr_anual) +
                          scale(heating_degree_15_winter) +
                          offset(log(contagios)),
                        data = data_mod,
                        na.action=na.omit)
summary(mod_cfr_raw_0)
nobs(mod_cfr_raw_0)
f_tableCoef(mod_cfr_raw_0)
f_tableMRR(mod_cfr_raw_0, preview = "none")
f_figMRR(mod_cfr_raw_0)
f_savePlot(last_plot(), sprintf(file_name,"CFR_raw_0"),dpi=150)
saveRDS(mod_cfr_raw_0, sprintf(file_mod,"CFR_raw_0"))
rm(mod_cfr_raw_0, data_mod)


## CFR Lag 0-20---------
data_mod <- df_modelo %>% filter(cfr_0_20>0 & covid_fallecidos>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_0_20*100)
mod_cfr_0_20 <- glm.nb(covid_fallecidos ~ 
                         mp25 + rm +
                         scale(densidad_pob_censal) +
                         scale(`15-44`) + scale(`65+`) +
                         scale(perc_puebloOrig) + scale(perc_rural) +
                         scale(dias_primerMuerte) +
                         scale(tasa_camas) +
                         scale(perc_lenaCalefaccion) +
                         scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                         scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                         scale(perc_vivHacMedio)+
                         scale(hr_anual) +
                         scale(heating_degree_15_winter) +
                         offset(log(contagios)),
                       data = data_mod,
                       na.action=na.omit)
summary(mod_cfr_0_20)
nobs(mod_cfr_0_20)
f_tableCoef(mod_cfr_0_20)
f_tableMRR(mod_cfr_0_20, preview = "none")
f_figMRR(mod_cfr_0_20)
f_savePlot(last_plot(), sprintf(file_name,"CFR_0_20"),dpi=150)
saveRDS(mod_cfr_0_20, sprintf(file_mod,"CFR_0_20"))
rm(mod_cfr_0_20, data_mod)


## Y: % Letalidad acumulado significativo---------
# Significativo segun el modelo base de nb (sin random intercept)
mod_letalidad_sign <- glm.nb(covid_fallecidos ~ 
                               # mp25 +
                               rm +
                               scale(`15-44`) +
                               scale(perc_puebloOrig) + scale(perc_rural) +
                               scale(dias_primerMuerte) +
                               scale(tasa_camas) +
                               scale(perc_vivHacMedio)+
                               scale(hr_anual) +
                               scale(heating_degree_15_winter) +
                               offset(log(casos_confirmados)),
                             data = df_modelo,
                             na.action=na.omit)
summary(mod_letalidad_sign)
nobs(mod_letalidad_sign)
f_tableCoef(mod_letalidad_sign)
f_tableMRR(mod_letalidad_sign, preview = "none")
f_figMRR(mod_letalidad_sign)
f_savePlot(last_plot(), sprintf(file_name,"Y_letalidad_sign"),dpi=150)
saveRDS(mod_letalidad_sign, sprintf(file_mod,"Y_letalidad_sign"))
rm(mod_letalidad_sign)


## CFR Lag 0 Bruto significativo ---------
# Significativo segun el modelo base de nb (sin random intercept)
data_mod <- df_modelo %>% filter(cfr_raw_0>0 & covid_fallecidos>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_raw_0*100)
mod_cfr_raw_0_sign <- glm.nb(covid_fallecidos ~ 
                               # mp25 +
                               rm +
                               scale(`15-44`) +
                               scale(perc_puebloOrig) + scale(perc_rural) +
                               scale(dias_primerMuerte) +
                               scale(tasa_camas) +
                               scale(perc_vivHacMedio)+
                               scale(hr_anual) +
                               scale(heating_degree_15_winter) +
                               offset(log(contagios)),
                             data = data_mod,
                             na.action=na.omit)
summary(mod_cfr_raw_0_sign)
nobs(mod_cfr_raw_0_sign)
f_tableCoef(mod_cfr_raw_0_sign)
f_tableMRR(mod_cfr_raw_0_sign, preview = "none")
f_figMRR(mod_cfr_raw_0_sign)
f_savePlot(last_plot(), sprintf(file_name,"CFR_raw_0_sign"),dpi=150)
saveRDS(mod_cfr_raw_0_sign, sprintf(file_mod,"CFR_raw_0_sign"))
rm(mod_cfr_raw_0_sign, data_mod)


## CFR Lag 0-20 significativo ---------
# Significativo segun el modelo base de nb (sin random intercept)
data_mod <- df_modelo %>% filter(cfr_0_20>0 & covid_fallecidos>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_0_20*100)
mod_cfr_0_20_sign <- glm.nb(covid_fallecidos ~ 
                              # mp25 +
                              rm +
                              scale(`15-44`) +
                              scale(perc_puebloOrig) + scale(perc_rural) +
                              scale(dias_primerMuerte) +
                              scale(tasa_camas) +
                              scale(perc_vivHacMedio)+
                              scale(hr_anual) +
                              scale(heating_degree_15_winter) +
                              offset(log(contagios)),
                            data = data_mod,
                            na.action=na.omit)
summary(mod_cfr_0_20_sign)
nobs(mod_cfr_0_20_sign)
f_tableCoef(mod_cfr_0_20_sign)
f_tableMRR(mod_cfr_0_20_sign, preview = "none")
f_figMRR(mod_cfr_0_20_sign)
f_savePlot(last_plot(), sprintf(file_name,"CFR_0_20_sign"),dpi=150)
saveRDS(mod_cfr_0_20_sign, sprintf(file_mod,"CFR_0_20_sign"))
rm(mod_cfr_0_20_sign, data_mod)


## Sin RM Y: % Letalidad acumulado---------
mod_letalidad_sign_Sinrm <- glm.nb(covid_fallecidos ~ 
                                     mp25 +
                                     scale(`15-44`) +
                                     scale(perc_puebloOrig) + scale(perc_rural) +
                                     scale(dias_primerMuerte) +
                                     scale(tasa_camas) +
                                     scale(perc_vivHacMedio)+
                                     scale(hr_anual) +
                                     scale(heating_degree_15_winter) +
                                     offset(log(casos_confirmados)),
                                   data = df_modelo %>% filter(region!="Metropolitana"),
                                   na.action=na.omit)
summary(mod_letalidad_sign_Sinrm)
nobs(mod_letalidad_sign_Sinrm)
f_tableCoef(mod_letalidad_sign_Sinrm)
f_tableMRR(mod_letalidad_sign_Sinrm, preview = "none")
f_figMRR(mod_letalidad_sign_Sinrm)
f_savePlot(last_plot(), sprintf(file_name,"Y_letalidad_sign_Sinrm"),dpi=150)
saveRDS(mod_letalidad_sign_Sinrm, sprintf(file_mod,"Y_letalidad_sign_Sinrm"))
rm(mod_letalidad_sign_Sinrm)

## Solo RM Y: % Letalidad acumulado---------
mod_letalidad_sign_rm <- glm.nb(covid_fallecidos ~ 
                                  mp25 +
                                  scale(`15-44`) +
                                  scale(perc_puebloOrig) + scale(perc_rural) +
                                  scale(dias_primerMuerte) +
                                  scale(tasa_camas) +
                                  scale(perc_vivHacMedio)+
                                  scale(hr_anual) +
                                  scale(heating_degree_15_winter) +
                                  offset(log(casos_confirmados)),
                                data = df_modelo %>% filter(region=="Metropolitana"),
                                na.action=na.omit)
summary(mod_letalidad_sign_rm)
nobs(mod_letalidad_sign_rm)
f_tableCoef(mod_letalidad_sign_rm)
f_tableMRR(mod_letalidad_sign_rm, preview = "none") 
f_figMRR(mod_letalidad_sign_rm)
f_savePlot(last_plot(), sprintf(file_name,"Y_letalidad_sign_rm"),dpi=150)
saveRDS(mod_letalidad_sign_rm, sprintf(file_mod,"Y_letalidad_sign_rm"))
rm(mod_letalidad_sign_rm)


## Consumo Urbano Leña Letalidad ---------
mod_lenaUrbano_letal <- glm.nb(covid_fallecidos ~ 
                                 mp25 +
                                 scale(`15-44`) +
                                 scale(perc_puebloOrig) + scale(perc_rural) +
                                 scale(dias_primerMuerte) +
                                 scale(tasa_camas) +
                                 scale(perc_vivHacMedio)+
                                 # scale(perc_lenaCalefaccion)+
                                 scale(hr_anual) +
                                 scale(heating_degree_15_winter) +
                                 scale(cons_lena_urbana) +
                                 offset(log(casos_confirmados)),
                               data = df_modelo,
                               na.action=na.omit)
summary(mod_lenaUrbano_letal)
nobs(mod_lenaUrbano_letal)
f_tableCoef(mod_lenaUrbano_letal)
f_tableMRR(mod_lenaUrbano_letal, preview = "none")
f_figMRR(mod_lenaUrbano_letal)
f_savePlot(last_plot(), sprintf(file_name,"lenaUrbano_letalidad"),dpi=150)
saveRDS(mod_lenaUrbano_letal, sprintf(file_mod,"lenaUrbano_letalidad"))
rm(mod_lenaUrbano_letal)


## Modelo CR2 + Leña con letalidad acumulada ---------
mod_nb_cr2_lena_let <- glm.nb(covid_fallecidos ~ 
                            mp25 +
                            rm +
                            scale(`65+`) +
                            scale(perc_vivHacMedio) +
                            scale(perc_salud) +
                            scale(perc_vivAntes2002) +
                            scale(hr_anual) + 
                            scale(movilidad) +
                            scale(hdd15_winter_lenaCalefaccion) +
                            offset(log(casos_confirmados)), 
                          data = df_modelo,
                          na.action=na.omit)
summary(mod_nb_cr2_lena_let)
nobs(mod_nb_cr2_lena_let)
f_tableCoef(mod_nb_cr2_lena_let)
f_tableMRR(mod_nb_cr2_lena_let, preview = "none")
f_figMRR(mod_nb_cr2_lena_let)
f_savePlot(last_plot(), sprintf(file_name,"CR2_lena_letalidad"),dpi=150)
saveRDS(mod_nb_cr2_lena_let, sprintf(file_mod,"CR2_lena_letalidads"))
rm(mod_nb_cr2_lena_let)

## Modelo CR2 + Leña con CFR ---------
data_mod <- df_modelo %>% filter(cfr_raw_0>0 & covid_fallecidos>0) %>% 
  mutate(contagios=covid_fallecidos/cfr_raw_0*100)
mod_nb_cr2_lena_cfr <- glm.nb(covid_fallecidos ~ 
                            mp25 +
                            rm +
                            scale(`65+`) +
                            scale(perc_vivHacMedio) +
                            scale(perc_salud) +
                            scale(perc_vivAntes2002) +
                            scale(hr_anual) + 
                            scale(movilidad) +
                            scale(hdd15_winter_lenaCalefaccion) +
                            offset(log(poblacion)), 
                          data = data_mod,
                          na.action=na.omit)
summary(mod_nb_cr2_lena_cfr)
nobs(mod_nb_cr2_lena_cfr)
f_tableCoef(mod_nb_cr2_lena_cfr)
f_tableMRR(mod_nb_cr2_lena_cfr, preview = "none")
f_figMRR(mod_nb_cr2_lena_cfr)
f_savePlot(last_plot(), sprintf(file_name,"CR2_lena_cfr"),dpi=150)
saveRDS(mod_nb_cr2_lena_cfr, sprintf(file_mod,"CR2_lena_cfr"))
rm(mod_nb_cr2_lena_cfr,data_mod)



## RESUMEN MODELOS ------------
### Grafico MRR MP2.5 Resumen modelos probados ------------

mod_nb <- read_rds(sprintf(file_mod,"Base"))
mod_nb_sig <- read_rds(sprintf(file_mod,"BaseSign"))
mod_sinMP <- read_rds(sprintf(file_mod,"sin_MP"))
mod_sinRM <- read_rds(sprintf(file_mod,"sin_RM"))
mod_RM <- read_rds(sprintf(file_mod,"soloRM"))
mod_sinRM_sign <- read_rds(sprintf(file_mod,"sin_RM"))
mod_RM_sign <- read_rds(sprintf(file_mod,"sinRM_sign"))
mod_mp25winter <- read_rds(sprintf(file_mod,"MP25Winter"))
modMP <- read_rds(sprintf(file_mod,"soloMP"))
mod_region <- read_rds(sprintf(file_mod,"randomRegion"))
modProv <- read_rds(sprintf(file_mod,"randomProvincia"))
mod_zona <- read_rds(sprintf(file_mod,"randomZonas"))
mod_zonaTermica <- read_rds(sprintf(file_mod,"randomZonaTermica"))
# mod_nb_cr2 <- read_rds(sprintf(file_mod,"CR2"))
# mod_nb_cr2_lena <- read_rds(sprintf(file_mod,"CR2_lena"))
# mod_nb_cr2_lena_cfr <- read_rds(sprintf(file_mod,"CR2_lena_cfr"))
mod_lena <- read_rds(sprintf(file_mod,"proxyLena"))
mod_65 <- read_rds(sprintf(file_mod,"fallecidos65"))
mod_cfr_0_20 <- read_rds(sprintf(file_mod,"CFR_0_20"))
mod_cfr_raw_0 <- read_rds(sprintf(file_mod,"CFR_raw_0"))
mod_letalidad <- read_rds(sprintf(file_mod,"Y_letalidad"))


## Cargo MRR del MP2.5 en mi Dataframe
df_mrr <- data.frame()
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Base"), f_MRR_mp25(mod_nb)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="MP2.5 Invierno"), 
                             f_MRR_mp25(mod_mp25winter,param="mp25_winter")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Sin RM"), 
                             f_MRR_mp25(mod_sinRM)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Solo RM"), 
                             f_MRR_mp25(mod_RM)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Sin RM Sign."), 
                             f_MRR_mp25(mod_sinRM_sign)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Solo RM Sign."), 
                             f_MRR_mp25(mod_RM_sign)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Region"), 
                             f_MRR_mp25(mod_region)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona"), 
                             f_MRR_mp25(mod_zona)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona Termica"), 
                             f_MRR_mp25(mod_zonaTermica)))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="Proxy Leña"), 
#                              f_MRR_mp25(mod_lena)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Fallecidos 65+"), 
                             f_MRR_mp25(mod_65)))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="CR2"), 
#                              f_MRR_mp25(mod_nb_cr2)))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="CR2 Leña"), 
#                              f_MRR_mp25(mod_nb_cr2_lena)))


## Figure MRR
df_mrr %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=reorder(method,desc(rowid)), y=RR))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(x="",y="MRR")+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  coord_flip()
f_savePlot(last_plot(), sprintf(file_name,"resumenMRR"),dpi=150)




## Figura resumen HDD 15° C Invierno ----
## Cargo MRR del MP2.5 en mi Dataframe
df_mrr <- data.frame()
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Base"), 
                             f_MRR_mp25(mod_nb, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Base Sign."), 
                             f_MRR_mp25(mod_nb_sig, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="MP2.5 Invierno"), 
                             f_MRR_mp25(mod_mp25winter,"scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Sin RM"), 
                             f_MRR_mp25(mod_sinRM, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Solo RM"), 
                             f_MRR_mp25(mod_RM, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Sin RM Sign."), 
                             f_MRR_mp25(mod_sinRM_sign, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Solo RM Sign."), 
                             f_MRR_mp25(mod_RM_sign, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Region"), 
                             f_MRR_mp25(mod_region, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona"), 
                             f_MRR_mp25(mod_zona, "scale(heating_degree_15_winter)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona Termica"), 
                             f_MRR_mp25(mod_zonaTermica, "scale(heating_degree_15_winter)")))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="Proxy Leña"), 
#                              f_MRR_mp25(mod_lena)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Fallecidos 65+"), 
                             f_MRR_mp25(mod_65, "scale(heating_degree_15_winter)")))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="CR2"), 
#                              f_MRR_mp25(mod_nb_cr2)))
# df_mrr <- rbind(df_mrr,cbind(data.frame(method="CR2 Leña"), 
#                              f_MRR_mp25(mod_nb_cr2_lena)))

## Figure MRR
df_mrr %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=reorder(method,desc(rowid)), y=RR))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  labs(x="",y="MRR")+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  coord_flip()
f_savePlot(last_plot(), sprintf(file_name,"resumenMRR_hdd15winter"),dpi=150)



## Prueba con Stargazer ---------
# Problemas:
# Solo puede generar latex usando knit o generar un html (no como en flextable)
# Calculo se complejiza para añadir CI y otras cosas.

# library(stargazer)
# 
# stargazer(df_modelo %>% dplyr::select(mp25,movilidad))
# 
# stargazer(mod_RM,mod_sinRM, out = 'table.html', 
#           digits = 2,
#           # apply.coef = exp,apply.ci = exp, apply.se = exp,
#           apply.coef=exp, t.auto=F, p.auto=F, report = "vct*",
#           column.labels = c("RM", "Sin RM"),
#           ci=T, ci.level = 0.95,
#           # single.row = T,
#           omit="Constant") 
# 
# f_tableMRR(mod_RM)

## EoF