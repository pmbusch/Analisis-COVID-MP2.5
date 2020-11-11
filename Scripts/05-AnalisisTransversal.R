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

mod_nb <- glm.nb(covid_fallecidos ~ 
                   mp25 +
                   rm +
                   scale(tasaMorbilidad_CPM) +
                   scale(densidad_pob_censal) +
                   scale(`15-44`) + scale(`65+`) +
                   scale(perc_vivHacMedio)+
                   scale(perc_mujer) +
                   scale(perc_puebloOrig) +
                   scale(perc_rural) +
                   scale(dias_primerMuerte) +
                   scale(tasa_camas) +
                   scale(movilidad) +
                   scale(perc_lenaCalefaccion) +
                   scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                   scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                   scale(hr_anual) +
                   scale(heating_degree_15_winter) +
                   offset(log(poblacion)), 
                 data = df_modelo,
                 na.action=na.omit)

summary(mod_nb)
nobs(mod_nb)
f_tableMRR(mod_nb, preview = "none", highlight = T)
rm(mod_nb)

## Base Solo Significativas---------
# positividad por region
# df_modelo <- df_modelo %>% 
#   left_join(df_modelo %>% group_by(region) %>% 
#               summarise(casos_confirmados=sum(casos_confirmados, na.rm=T),
#                         pcr_region=min(pcr_region, na.rm=T)) %>% 
#               ungroup() %>% mutate(positividad=casos_confirmados/pcr_region) %>% 
#               dplyr::select(region, positividad),
#             by=c("region"))

mod_base <- glm.nb(covid_fallecidos ~ 
                     mp25 +
                     rm +
                     # scale(pcr_region)+
                     scale(`15-44`) +  scale(`65+`) +
                     scale(perc_vivHacMedio)+
                     scale(perc_puebloOrig) +
                     scale(dias_primerMuerte) +
                     scale(perc_lenaCalefaccion) +
                     scale(perc_menor_media)+
                     scale(hr_anual) +
                     offset(log(poblacion)), 
                   data = df_modelo,
                   na.action=na.omit)
summary(mod_base)
nobs(mod_base)
f_tableMRR(mod_base, preview = "none", highlight = T) 
f_figMRR(mod_base)
rm(mod_base)


## Load data de muchos modelos: script loop
df_coef_params <- read.delim("ResumenModelos/Loop/coef.csv", sep=";",skip = 1)
df_coef_params %>% names()

## Tabla Resumen PPT modelos principales ----------
url <- "Data/Data_Modelo/Loop/"
## Pruebas concretas
modelo_mr <- read_rds(paste(url, "mod-MR-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep="")); summary(modelo_mr)
modelo_mr65 <- read_rds(paste(url, "mod-MR65-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep="")); summary(modelo_mr65)
modelo_mr75 <- read_rds(paste(url, "mod-MR75-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep="")); summary(modelo_mr75)
modelo_cfr <- read_rds(paste(url, "mod-CFR-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep="")); summary(modelo_cfr)

# MP2.5
df_mrr_mp <- data.frame()
df_mrr_mp <- rbind(df_mrr_mp,cbind(data.frame(Endpoint="TMR (all)"), 
                                   f_MRR_mp25(modelo_mr)))
df_mrr_mp <- rbind(df_mrr_mp,cbind(data.frame(Endpoint="TMR (65+)"), 
                                   f_MRR_mp25(modelo_mr65)))
df_mrr_mp <- rbind(df_mrr_mp,cbind(data.frame(Endpoint="TMR (75+)"), 
                                   f_MRR_mp25(modelo_mr75)))
df_mrr_mp <- rbind(df_mrr_mp,cbind(data.frame(Endpoint="CFR (all)"), 
                                   f_MRR_mp25(modelo_cfr)))
df_mrr_mp <- df_mrr_mp %>% mutate(Variable="MP2.5 2017-2019 [ug/m3]")

# Lena
df_mrr_lena <- data.frame()
df_mrr_lena <- rbind(df_mrr_lena,cbind(data.frame(Endpoint="TMR (all)"), 
                                       f_MRR_mp25(modelo_mr, "scale(perc_lenaCalefaccion)")))
df_mrr_lena <- rbind(df_mrr_lena,cbind(data.frame(Endpoint="TMR (65+)"), 
                                       f_MRR_mp25(modelo_mr65,"scale(perc_lenaCalefaccion)")))
df_mrr_lena <- rbind(df_mrr_lena,cbind(data.frame(Endpoint="TMR (75+)"), 
                                       f_MRR_mp25(modelo_mr75,"scale(perc_lenaCalefaccion)")))
df_mrr_lena <- rbind(df_mrr_lena,cbind(data.frame(Endpoint="CFR (all)"), 
                                       f_MRR_mp25(modelo_cfr,"scale(perc_lenaCalefaccion)")))
df_mrr_lena <- df_mrr_lena %>% mutate(Variable="% Leña Calefacción")

df_mrr <- rbind(df_mrr_mp, df_mrr_lena)

## Add sign codes
df_mrr <- df_mrr %>%
  mutate(`Sign.`=c("**","***","***"," ",
                   "*","*"," ","."))

foot_note <- "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

df_mrr %>% 
  mutate(`95% I.C.`= paste("(",round(lower_CI,2),", ",round(upper_CI,2),
                           ")", sep=""),
         MRR=round(RR,2)) %>% 
  dplyr::select(Variable,Endpoint, MRR, `95% I.C.`,`Sign.`) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% 
  bold(j=1, bold=T) %>%
  merge_v(j = 1) %>%   fix_border_issues(part = "all") %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>% 
  flextable::border(j=1, part="body",
                    border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  flextable::border(j=2:5, part="body",i=c(4),
                    border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  footnote(j=5, value=as_paragraph(foot_note), part="header", inline=T)
# print(preview="pptx")


rm(df_mrr, df_mrr_mp,df_mrr_lena, modelo_mr, modelo_mr65,modelo_mr75,modelo_cfr)



## Modelo Causas CardioPulmonares ----------
mod_valido <- glm.nb(def_cardioPulmonar ~ 
                       mp25 +
                       rm +
                       scale(`15-44`) +  scale(`65+`) +
                       scale(perc_vivHacMedio)+
                       scale(perc_puebloOrig) +
                       scale(perc_lenaCalefaccion) +
                       scale(perc_menor_media)+
                       scale(hr_anual) +
                       offset(log(poblacion)), 
                     data = df_modelo %>% mutate(mp25=mp25/10),
                     na.action=na.omit)
summary(mod_valido)
nobs(mod_valido)
f_tableMRR(mod_valido, preview = "none", highlight = T) 
f_figMRR(mod_valido)
rm(mod_valido)

## Modelo PDA Energia ----------
mod_pda <- glm.nb(covid_fallecidos ~ 
                    mp25 +
                    scale(`15-44`) +  scale(`65+`) +
                    scale(perc_vivHacMedio)+
                    scale(perc_puebloOrig) +
                    scale(dias_primerMuerte)+
                    scale(cons_lena_urbana) +
                    scale(perc_menor_media)+
                    scale(hr_anual) +
                    offset(log(poblacion)), 
                  data = df_modelo,
                  na.action=na.omit)
summary(mod_pda)
nobs(mod_pda)
f_tableMRR(mod_pda, preview = "none", highlight = T) 
f_figMRR(mod_pda)
rm(mod_pda)


## Modelo 65+ -------
url <- "Data/Data_Modelo/Loop/"
modelo_mr65 <- read_rds(paste(url, "mod-MR65-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep=""))
f_tableMRR(modelo_mr65, preview = "none", highlight = T); nobs(modelo_mr65)

## Modelo Full CFR -----------
mod_nb_cfr <- glm.nb(covid_fallecidos ~ 
                       mp25 +
                       rm +
                       scale(tasaMorbilidad_CPM) +
                       scale(densidad_pob_censal) +
                       scale(`15-44`) + scale(`65+`) +
                       scale(perc_vivHacMedio)+
                       scale(perc_mujer) +
                       scale(perc_puebloOrig) +
                       scale(perc_rural) +
                       scale(tasa_camas) +
                       scale(movilidad) +
                       scale(perc_lenaCalefaccion) +
                       scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                       scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                       scale(hr_anual) +
                       scale(heating_degree_15_winter) +
                       offset(log(casos_confirmados)), 
                     data = df_modelo,
                     na.action=na.omit)

summary(mod_nb_cfr)
nobs(mod_nb_cfr)
f_tableMRR(mod_nb_cfr, preview = "none", highlight = T)
rm(mod_nb_cfr)

#reducido
mod_nb_cfr <- glm.nb(covid_fallecidos ~ 
                       mp25 +
                       rm +
                       scale(`15-44`) + 
                       scale(`65+`) +
                       scale(perc_vivHacMedio)+
                       scale(perc_lenaCalefaccion) +
                       scale(perc_menor_media) +
                       scale(hr_anual) +
                       offset(log(casos_confirmados)), 
                     data = df_modelo,
                     na.action=na.omit)

summary(mod_nb_cfr)
nobs(mod_nb_cfr)
f_tableMRR(mod_nb_cfr, preview = "none", highlight = T)
rm(mod_nb_cfr)


## Modelo RM o no --------
## Base Solo Significativas
mod_rm <- glm.nb(covid_fallecidos ~ 
                   mp25 +
                   # pda +
                   # rm +
                   scale(`15-44`) +  scale(`65+`) +
                   scale(perc_vivHacMedio)+
                   scale(perc_puebloOrig) +
                   scale(dias_primerMuerte) +
                   scale(perc_lenaCalefaccion) +
                   scale(perc_menor_media)+
                   scale(hr_anual) +
                   offset(log(poblacion)),
                 # offset(log(casos_confirmados)),
                 data = df_modelo %>% filter(region=="Metropolitana"),
                 na.action=na.omit)
summary(mod_rm)
nobs(mod_rm)
f_tableMRR(mod_rm, preview = "none", highlight = T) 
f_figMRR(mod_rm)
rm(mod_rm)

mod_sinrm <- glm.nb(covid_fallecidos ~ 
                      mp25 +
                      # pda +
                      # rm +
                      scale(`15-44`) +  scale(`65+`) +
                      scale(perc_vivHacMedio)+
                      scale(perc_puebloOrig) +
                      scale(dias_primerMuerte) +
                      scale(perc_lenaCalefaccion) +
                      scale(perc_menor_media)+
                      scale(hr_anual) +
                      offset(log(poblacion)),
                    # offset(log(casos_confirmados)),
                    data = df_modelo %>% filter(region!="Metropolitana"),
                    na.action=na.omit)
summary(mod_sinrm)
nobs(mod_sinrm)
f_tableMRR(mod_sinrm, preview = "none", highlight = T) 
f_figMRR(mod_sinrm)
rm(mod_sinrm)


## Tasa Contagios -----------
## Correlacion
cor(df_modelo$casos_confirmados, df_modelo$covid_fallecidos,
    method="pearson", use = "complete.obs")
mod_base <- glm.nb(casos_confirmados ~ 
                     mp25 +
                     rm +
                     scale(`15-44`) +  scale(`65+`) +
                     scale(perc_vivHacMedio)+
                     scale(perc_puebloOrig) +
                     scale(dias_primerContagio) +
                     scale(perc_lenaCalefaccion) +
                     scale(perc_menor_media)+
                     scale(hr_anual) +
                     offset(log(poblacion)), 
                   data = df_modelo,
                   na.action=na.omit)
summary(mod_base)
nobs(mod_base)
f_tableMRR(mod_base, preview = "none", highlight = T) 
f_figMRR(mod_base)
rm(mod_base)


## Tablas informe ----------
url <- "Data/Data_Modelo/Loop/"
(modelos_rsd <- list.files(url))

df_coef_params$dependiente %>% unique()
df_coef_params$socioeconomico %>% unique()
df_coef_params$demografia %>% unique()
df_coef_params$lena %>% unique()
df_coef_params$meteorologia %>% unique()

## Pruebas concretas
modelo_mr <- read_rds(paste(url, "mod-MR-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep=""))
modelo_let <- read_rds(paste(url, "mod-CFR-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep=""))
modelo_cfr0 <- read_rds(paste(url, "mod-CFR_DL0-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep=""))
modelo_cfr0a20 <- read_rds(paste(url, "mod-CFR_DL0a20-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep=""))


f_tableMRR(modelo_mr, highlight = T); nobs(modelo_mr)
f_tableMRR(modelo_let, highlight = T, preview = "none"); nobs(modelo_let)
f_tableMRR(modelo_cfr0, highlight = T, preview = "none"); nobs(modelo_cfr0)
f_tableMRR(modelo_cfr0a20, highlight = T, preview = "none"); nobs(modelo_cfr0a20)
rm(modelo_mr, modelo_let, modelo_cfr0, modelo_cfr0a20)

## Figure resumen conclusiones distintos modelos ---------
## ----------------------
## De aca abajo todo manual
mod_mp25winter <- glm.nb(covid_fallecidos ~ 
                           mp25_winter +
                           # pda +
                           rm +
                           scale(`15-44`) +  scale(`65+`) +
                           scale(perc_vivHacMedio)+
                           scale(perc_puebloOrig) +
                           scale(dias_primerMuerte) +
                           scale(perc_lenaCalefaccion) +
                           scale(perc_menor_media)+
                           scale(hr_anual) +
                           offset(log(poblacion)),
                         # offset(log(casos_confirmados)),
                         data = df_modelo,
                         na.action=na.omit)


## Random Region---------
mod_region <- glmer.nb(covid_fallecidos ~ 
                         mp25 +
                         rm +
                         scale(`15-44`) +  scale(`65+`) +
                         scale(perc_vivHacMedio)+
                         scale(perc_puebloOrig) +
                         scale(dias_primerMuerte) +
                         scale(perc_lenaCalefaccion) +
                         scale(perc_menor_media)+
                         scale(hr_anual) +
                         (1|region)+
                         offset(log(poblacion)), 
                       data = df_modelo,
                       na.action=na.omit)

## Random Zonas---------
mod_zona <- glmer.nb(covid_fallecidos ~ 
                       mp25 +
                       rm +
                       scale(`15-44`) +  scale(`65+`) +
                       scale(perc_vivHacMedio)+
                       scale(perc_puebloOrig) +
                       scale(dias_primerMuerte) +
                       scale(perc_lenaCalefaccion) +
                       scale(perc_menor_media)+
                       scale(hr_anual) +
                       (1|zona)+
                       offset(log(poblacion)), 
                     data = df_modelo,
                     na.action=na.omit)

## Random Zona Termica---------
mod_zonaTermica <- glmer.nb(covid_fallecidos ~ 
                              mp25 +
                              rm +
                              scale(`15-44`) +  scale(`65+`) +
                              scale(perc_vivHacMedio)+
                              scale(perc_puebloOrig) +
                              scale(dias_primerMuerte) +
                              scale(perc_lenaCalefaccion) +
                              scale(perc_menor_media)+
                              scale(hr_anual) +
                              (1|zona_termica)+
                              offset(log(poblacion)), 
                            data = df_modelo,
                            na.action=na.omit)

## Modelos LET -----
mod_base_cfr <- glm.nb(covid_fallecidos ~ 
                         mp25 +
                         rm +
                         scale(`15-44`) +  scale(`65+`) +
                         scale(perc_vivHacMedio)+
                         scale(perc_puebloOrig) +
                         # scale(dias_primerMuerte) +
                         scale(perc_lenaCalefaccion) +
                         scale(perc_menor_media)+
                         scale(hr_anual) +
                         offset(log(casos_confirmados)), 
                       data = df_modelo,
                       na.action=na.omit)

mod_rm_cfr <- glm.nb(covid_fallecidos ~ 
                       mp25 +
                       scale(`15-44`) +  scale(`65+`) +
                       scale(perc_vivHacMedio)+
                       scale(perc_puebloOrig) +
                       # scale(dias_primerMuerte) +
                       scale(perc_lenaCalefaccion) +
                       scale(perc_menor_media)+
                       scale(hr_anual) +
                       offset(log(casos_confirmados)),
                     data = df_modelo %>% filter(region=="Metropolitana"),
                     na.action=na.omit)
# f_tableMRR(mod_rm_cfr, preview = "none", highlight = T); nobs(mod_rm_cfr)

mod_sinrm_cfr <- glm.nb(covid_fallecidos ~ 
                          mp25 +
                          scale(`15-44`) +  scale(`65+`) +
                          scale(perc_vivHacMedio)+
                          scale(perc_puebloOrig) +
                          # scale(dias_primerMuerte) +
                          scale(perc_lenaCalefaccion) +
                          scale(perc_menor_media)+
                          scale(hr_anual) +
                          offset(log(casos_confirmados)),
                        data = df_modelo %>% filter(region!="Metropolitana"),
                        na.action=na.omit)
# f_tableMRR(mod_sinrm_cfr, preview = "pptx", highlight = T); nobs(mod_sinrm_cfr)

mod_mp25winter_cfr <- glm.nb(covid_fallecidos ~ 
                               mp25_winter +
                               rm +
                               scale(`15-44`) +  scale(`65+`) +
                               scale(perc_vivHacMedio)+
                               scale(perc_puebloOrig) +
                               # scale(dias_primerMuerte) +
                               scale(perc_lenaCalefaccion) +
                               scale(perc_menor_media)+
                               scale(hr_anual) +
                               offset(log(casos_confirmados)),
                             data = df_modelo,
                             na.action=na.omit)

mod_region_cfr <- glmer.nb(covid_fallecidos ~ 
                             mp25 +
                             rm +
                             scale(`15-44`) +  scale(`65+`) +
                             scale(perc_vivHacMedio)+
                             scale(perc_puebloOrig) +
                             # scale(dias_primerMuerte) +
                             scale(perc_lenaCalefaccion) +
                             scale(perc_menor_media)+
                             scale(hr_anual) +
                             (1|region)+
                             offset(log(casos_confirmados)), 
                           data = df_modelo,
                           na.action=na.omit)

## Random Zonas
mod_zona_cfr <- glmer.nb(covid_fallecidos ~ 
                           mp25 +
                           rm +
                           scale(`15-44`) +  scale(`65+`) +
                           scale(perc_vivHacMedio)+
                           scale(perc_puebloOrig) +
                           # scale(dias_primerMuerte) +
                           scale(perc_lenaCalefaccion) +
                           scale(perc_menor_media)+
                           scale(hr_anual) +
                           (1|zona)+
                           offset(log(casos_confirmados)), 
                         data = df_modelo,
                         na.action=na.omit)

## Random Zona Termica
mod_zonaTermica_cfr <- glmer.nb(covid_fallecidos ~ 
                                  mp25 +
                                  rm +
                                  scale(`15-44`) +  scale(`65+`) +
                                  scale(perc_vivHacMedio)+
                                  scale(perc_puebloOrig) +
                                  # scale(dias_primerMuerte) +
                                  scale(perc_lenaCalefaccion) +
                                  scale(perc_menor_media)+
                                  scale(hr_anual) +
                                  (1|zona_termica)+
                                  offset(log(casos_confirmados)), 
                                data = df_modelo,
                                na.action=na.omit)


## Cargo MRR del MP2.5 en mi Dataframe
df_mrr <- data.frame()
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Base"), 
                             f_MRR_mp25(mod_base)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="MP2.5 Invierno"), 
                             f_MRR_mp25(mod_mp25winter,param="mp25_winter")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Sin RM"), 
                             f_MRR_mp25(mod_sinrm)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Solo RM"), 
                             f_MRR_mp25(mod_rm)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Region"), 
                             f_MRR_mp25(mod_region)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona"), 
                             f_MRR_mp25(mod_zona)))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona Termica"), 
                             f_MRR_mp25(mod_zonaTermica)))
## Add from excel
df_coef_params$dependiente %>% unique()
aux <- df_coef_params %>% 
  filter(dependiente=="MR65" & term=="mp25" &
           lena=="perc_lenaCalefaccion" & meteorologia=="hr_anual" &
           socioeconomico=="perc_menor_media" & demografia=="perc_puebloOrig")
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Fallecidos 65+",
                                        RR=aux$estimate,
                                        lower_CI=aux$conf.low,
                                        upper_CI=aux$conf.high)))
aux <- df_coef_params %>% 
  filter(dependiente=="MR75" & term=="mp25" &
           lena=="perc_lenaCalefaccion" & meteorologia=="hr_anual" &
           socioeconomico=="perc_menor_media" & demografia=="perc_puebloOrig")
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Fallecidos 75+",
                                        RR=aux$estimate,
                                        lower_CI=aux$conf.low,
                                        upper_CI=aux$conf.high)))

df_mrr <- df_mrr %>% mutate(endpoint="MR")


## CFR
df_mrr_cfr <- data.frame()
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Base"), 
                                     f_MRR_mp25(mod_base_cfr)))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="MP2.5 Invierno"), 
                                     f_MRR_mp25(mod_mp25winter_cfr,param="mp25_winter")))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Sin RM"), 
                                     f_MRR_mp25(mod_sinrm_cfr)))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Solo RM"), 
                                     f_MRR_mp25(mod_rm_cfr)))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Random Region"), 
                                     f_MRR_mp25(mod_region_cfr)))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Random Zona"), 
                                     f_MRR_mp25(mod_zona_cfr)))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Random Zona Termica"), 
                                     f_MRR_mp25(mod_zonaTermica_cfr)))
df_mrr_cfr <- df_mrr_cfr %>% mutate(endpoint="CFR")


df_mrr_all <- rbind(df_mrr,df_mrr_cfr) %>% 
  mutate(endpoint=factor(endpoint, levels=c("MR","CFR")),
         method=factor(method, levels=c("Base", "MP2.5 Invierno",
                                        "Solo RM","Sin RM","Random Region",
                                        "Random Zona","Random Zona Termica",
                                        "Fallecidos 65+", "Fallecidos 75+")))

## Figure MRR --------
df_mrr_all %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=fct_rev(method), y=RR))+
  geom_point(size=4, alpha=.5)+
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), size=0.8)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_wrap(~endpoint)+
  labs(x="",y="MRR MP2.5")+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  coord_flip()+
  theme_set(theme_bw(20)+theme(panel.grid.major = element_blank()))

f_savePlot(last_plot(), sprintf(file_name,"resumenMRR_mp25"),dpi=150)


## IDEM pero para variable de leña
## Cargo MRR del MP2.5 en mi Dataframe
df_mrr <- data.frame()
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Base"), 
                             f_MRR_mp25(mod_base,"scale(perc_lenaCalefaccion)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Sin RM"), 
                             f_MRR_mp25(mod_sinrm, "scale(perc_lenaCalefaccion)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Solo RM"), 
                             f_MRR_mp25(mod_rm, "scale(perc_lenaCalefaccion)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Region"), 
                             f_MRR_mp25(mod_region, "scale(perc_lenaCalefaccion)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona"), 
                             f_MRR_mp25(mod_zona, "scale(perc_lenaCalefaccion)")))
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Random Zona Termica"), 
                             f_MRR_mp25(mod_zonaTermica, "scale(perc_lenaCalefaccion)")))
## Add from excel
df_coef_params$dependiente %>% unique()
aux <- df_coef_params %>% 
  filter(dependiente=="MR65" & term=="scale(perc_lenaCalefaccion)" &
           lena=="perc_lenaCalefaccion" & meteorologia=="hr_anual" &
           socioeconomico=="perc_menor_media" & demografia=="perc_puebloOrig")
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Fallecidos 65+",
                                        RR=aux$estimate,
                                        lower_CI=aux$conf.low,
                                        upper_CI=aux$conf.high)))
aux <- df_coef_params %>% 
  filter(dependiente=="MR75" & term=="scale(perc_lenaCalefaccion)" &
           lena=="perc_lenaCalefaccion" & meteorologia=="hr_anual" &
           socioeconomico=="perc_menor_media" & demografia=="perc_puebloOrig")
df_mrr <- rbind(df_mrr,cbind(data.frame(method="Fallecidos 75+",
                                        RR=aux$estimate,
                                        lower_CI=aux$conf.low,
                                        upper_CI=aux$conf.high)))

df_mrr <- df_mrr %>% mutate(endpoint="MR")

## CFR
df_mrr_cfr <- data.frame()
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Base"), 
                                     f_MRR_mp25(mod_base_cfr, "scale(perc_lenaCalefaccion)")))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Sin RM"), 
                                     f_MRR_mp25(mod_sinrm_cfr, "scale(perc_lenaCalefaccion)")))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Solo RM"), 
                                     f_MRR_mp25(mod_rm_cfr, "scale(perc_lenaCalefaccion)")))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Random Region"), 
                                     f_MRR_mp25(mod_region_cfr, "scale(perc_lenaCalefaccion)")))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Random Zona"), 
                                     f_MRR_mp25(mod_zona_cfr, "scale(perc_lenaCalefaccion)")))
df_mrr_cfr <- rbind(df_mrr_cfr,cbind(data.frame(method="Random Zona Termica"), 
                                     f_MRR_mp25(mod_zonaTermica_cfr, "scale(perc_lenaCalefaccion)")))
df_mrr_cfr <- df_mrr_cfr %>% mutate(endpoint="CFR")


df_mrr_all <- rbind(df_mrr,df_mrr_cfr) %>% 
  mutate(endpoint=factor(endpoint, levels=c("MR","CFR")),
         method=factor(method, levels=c("Base",
                                        "Solo RM","Sin RM","Random Region",
                                        "Random Zona","Random Zona Termica",
                                        "Fallecidos 65+", "Fallecidos 75+")))

## Figure MRR --------
df_mrr_all %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=fct_rev(method), y=RR))+
  geom_point(size=4, alpha=.5)+
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI),size=.8)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_wrap(~endpoint)+
  labs(x="",y="% Uso leña como combustible principal en Calefacción")+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  coord_flip()+
  theme_set(theme_bw(20)+theme(panel.grid.major = element_blank()))
f_savePlot(last_plot(), sprintf(file_name,"resumenMRR_lena"),dpi=150)

## EoF