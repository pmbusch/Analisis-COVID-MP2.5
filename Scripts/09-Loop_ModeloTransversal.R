### Analisis-COVID-MP2.5
## Analisis Transversal
## Loop para generar y probar varios modelos
## PBH Octubre 2020

# source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)


# Load Data (a nivel de comuna)  -----------
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/05-FuncionesAnalisisTransversal.R", encoding = "UTF-8")
load(".RData")
file_name <- "Figuras/Analisis_transversal/Loop/%s.png"
file_mod <- "Data/Data_Modelo/Loop/%s.rsd"


cat("Fecha informe DEIS muertes COVID: ",fecha_deis, sep="")

## Y: TASA DE MORTALIDAD COVID (MUERTES/POBLACION) --------------
## MODELO DEFAULT: Binomial Negativo-----
# Variables se seleccionaron con ayuda de Step, pero con logica externa, 
# segun cuales podrian ser explicativas (y usadas en otros estudios)

mod_nb <- glm.nb(covid_fallecidos ~ 
                   mp25 +
                   rm +
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
mod_base <- glm.nb(covid_fallecidos ~ 
                     mp25 +
                     rm +
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

## Formula Base ---------
## Terminos para crear mi formula

df <- df_modelo %>% mutate(
  pob65=poblacion*`65+`, pob75=poblacion*`75+`,
  contagios_lag0=covid_fallecidos/cfr_raw_0*100,
  contagios_lag0_20=covid_fallecidos/cfr_0_20*100)


dependiente <- c("covid_fallecidos","covid_fallecidos_65","covid_fallecidos_75",
                 "covid_fallecidos","covid_fallecidos","covid_fallecidos")
off <- c("offset(log(poblacion))","offset(log(pob65))","offset(log(pob75))",
         "offset(log(casos_confirmados))","offset(log(contagios_lag0))",
         "offset(log(contagios_lag0_20))")
name_depend <- c("MR","MR65","MR75","LET","CFR0","CFR0a20")
                 
explicativas <- c("mp25","rm",
                  "scale(perc_vivHacMedio)",
                  "scale(perc_puebloOrig)",
                  "scale(dias_primerMuerte)",
                  "scale(perc_lenaCalefaccion)",
                  "scale(perc_menor_media)",
                  "scale(hr_anual)")

# Para sacarlas con tasa de mortalidad por edad
edad_explicativas <- c("scale(`15-44`)", "scale(`65+`)")


## Vectores a iterar ----------
#S
socioeconomico <- c("perc_menor_media","perc_fonasa_D",
                    "perc_isapre","log(ingresoAutonomo_media)")

#D
demografia <- c("perc_puebloOrig","perc_rural",
                "perc_mujer","densidad_pob_censal")

#L
lena <- c("perc_lenaCalefaccion","perc_lenaCocina",
          "cons_lena_kg")
#M
meteorologia <- c("hr_anual","tmed_anual","tmed_winter",
                  "heating_degree_15_winter")

## Total modelos a probar
length(socioeconomico)*length(demografia)*length(lena)*length(meteorologia)*
  length(dependiente)


## Loop total -----------------
## Codigo nombre
## Variable dependiente, tipo variable, variable explicativa
## mod_VARDEPENDIENTE_TIPOVAR_VAREXP
name_model <- "mod-%s-%s%s-%s%s-%s%s-%s%s"
sprintf(name_model,
        name_depend[1], 
        "S",socioeconomico[1],
        "D",demografia[1],
        "L",lena[1],
        "M",meteorologia[1])

i <- 1
for (z in 1:length(dependiente)){
  
  if (dependiente[z] %in% c("covid_fallecidos_65","covid_fallecidos_75")){
    explicativas_dep <- explicativas
  }else{
    explicativas_dep <- c(explicativas,edad_explicativas)
  }
  
  for (s in socioeconomico){
    # cambio la variable explicativa
    explicativas_s <- explicativas_dep %>% 
      str_replace("perc_menor_media",s)
    
    for (d in demografia){
      # cambio la variable explicativa
      explicativas_d <- explicativas_s%>% 
        str_replace("perc_puebloOrig",d)
      
      for (l in lena){
        # cambio la variable explicativa
        explicativas_l <- explicativas_d %>% 
          str_replace("perc_lenaCalefaccion",l)
        
        for (m in meteorologia){
          cat(i, "\n", sep=""); i <- i+1;
          # cambio la variable explicativa
          explicativas_m <- explicativas_l %>% 
            str_replace("hr_anual",m)
          
          ## genero la nueva formula
          formula_modelo <- reformulate(dependiente[z], 
                                        termlabels = c(explicativas_m, off[z]))
          
          ## Modelo
          mod_loop <- glm.nb(formula_modelo, 
                               data = df,
                               na.action=na.omit)
          
          ## Guardo modelo
          # nombre modelo
          mod_name <- sprintf(name_model,
                              name_depend[z], 
                              "S",s,
                              "D",d,
                              "L",l,
                              "M",m
                              )
          saveRDS(mod_loop, sprintf(file_mod,mod_name))
          rm(formula_modelo,mod_loop,mod_name,explicativas_m)
        }
        rm(explicativas_l)
      }
      rm(explicativas_d)
    }
      rm(explicativas_s)
  }
  rm(explicativas_dep)
}

rm(s,d,l,m,i,z)

## Lectura todos los modelos guardados ---------

library(tools)
library(broom) # to get info of fitted models easily

url <- "Data/Data_Modelo/Loop/"
(modelos_rsd <- list.files(url))


## Pruebas concretas
modelo1 <- read_rds(paste(url, modelos_rsd[1], sep=""));modelos_rsd[1];
modelo2 <- read_rds(paste(url, modelos_rsd[2], sep=""));modelos_rsd[2];

f_tableMRR(modelo1, highlight = T)
f_tableMRR(modelo2, highlight = T)

# Para guardar info
df_params <- data.frame()
df_coef <- data.frame()
i <- 1
## Loop para almacenar data
for (m in modelos_rsd){
  cat(i," Modelo: ",m,"\n",sep=""); i <- i+1;
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
rm(i,m)

## Save Data ----------
a <- df_coef; b <- df_params;

# Feat data
df_params <- df_params %>% 
  mutate(dependiente=f_split_n(modelo,"-",2),
         socioeconomico=f_split_n(modelo,"-",3) %>% str_remove("S"),
         demografia=f_split_n(modelo,"-",4) %>% str_remove("D"),
         lena=f_split_n(modelo,"-",5) %>% str_remove("L"),
         meteorologia=f_split_n(modelo,"-",6) %>% str_remove("M") %>% 
           str_remove("\\.rsd"))

# Join
df_coef_params <- df_coef %>% left_join(df_params, by = c("modelo"))

## Save data in Excel
file_path <- "ResumenModelos/Loop/params.csv"
cat('sep=; \n',file = file_path)
write.table(df_params,file_path, sep=';',row.names = F, append = T)


file_path <- "ResumenModelos/Loop/coef.csv"
cat('sep=; \n',file = file_path)
write.table(df_coef_params,file_path, sep=';',row.names = F, append = T)

rm(file_path,url,file_name, modelos_rsd, df_coef_params, df_coef, df_params)

## Figuras resumen -----------
df_coef_params <- read.delim("ResumenModelos/Loop/coef.csv", sep=";",skip = 1)


df_coef_params %>% names()


## Figura: MRR todos segun EndPoint
df_coef_params$term %>% unique()
df_coef_params %>% 
  rowid_to_column() %>% 
  filter(!(term %in% c("(Intercept)","rmRM"))) %>% 
  filter(lena=="perc_lenaCalefaccion" & meteorologia=="hr_anual" &
           socioeconomico=="perc_menor_media" & demografia=="perc_puebloOrig") %>% 
  mutate(term=term %>% str_remove_all("scale\\(|\\)") %>% f_replaceVar() %>% 
           factor(unique(.)),
         socioeconomico=socioeconomico %>% 
           str_remove_all("log\\(|\\)") %>% f_replaceVar(),
         demografia=f_replaceVar(demografia),
         lena=f_replaceVar(lena),
         meteorologia=f_replaceVar(meteorologia),
         dependiente=factor(dependiente,
                            levels = c("MR","MR65","MR75",
                                       "LET","CFR0","CFR0a20"))) %>% 
  ggplot(aes(x=fct_rev(term), y=estimate))+
  geom_point(size=2, alpha=.5)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_wrap(~dependiente)+
  coord_flip()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="MRR", 
       caption="MRR bajo distintos Endpoints considerados. \n 
  En columnas se presentan las variables explicativas de cada modelo. \n
  Cada cuadro representa un Endpoint (variable dependiente) distinto.")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))


## Figura: MRR MP2.5 segun endpoint, para socioeconomico y demografia
df_coef_params %>% 
  filter(term=="mp25") %>% 
  filter(lena=="perc_lenaCalefaccion" & meteorologia=="hr_anual") %>% 
  mutate(socioeconomico=socioeconomico %>% 
           str_remove_all("log\\(|\\)") %>% f_replaceVar(),
         demografia=f_replaceVar(demografia),
         dependiente=factor(dependiente,
                            levels = c("MR","MR65","MR75",
                                       "LET","CFR0","CFR0a20"))) %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=fct_rev(dependiente), y=estimate))+
  geom_point(size=2, alpha=.5)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_grid(demografia~socioeconomico)+
  coord_flip()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="MRR MP2.5 2017-2019 [ug/m3]", 
  caption="Coeficiente MP2.5 bajo distintas variables explicativas y endpoints. \n 
  En columnas se presentan variables socioeconomicas, en filas las variables demograficas y en cada cuadricula se diferencia por Endpoint. \n
  Formula base del modelo ajustado: {Endpoint} ~ MP2.5 + RM + DiasdesdePrimeraMuerteCOVID + {VariableDemografica} + {VariableSocioeconomica} + %LeñaCalefaccion + HumedadRelativa \n
  {}: Representa la variable modificada en cada análisis mostrado en la figura.")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))


## Figura: MRR MP2.5 segun endpoint, para leña y meteorologia
df_coef_params %>% 
  filter(term=="mp25") %>% 
  filter(socioeconomico=="perc_menor_media" & demografia=="perc_puebloOrig") %>% 
  mutate(lena=f_replaceVar(lena),
         meteorologia=f_replaceVar(meteorologia),
         dependiente=factor(dependiente,
                            levels = c("MR","MR65","MR75",
                                       "LET","CFR0","CFR0a20"))) %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=fct_rev(dependiente), y=estimate))+
  geom_point(size=2, alpha=.5)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_grid(lena~meteorologia)+
  coord_flip()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="MRR MP2.5 2017-2019 [ug/m3]", 
       caption="Coeficiente MP2.5 bajo distintas variables explicativas y endpoints. \n 
  En columnas se presentan variables socioeconomicas, en filas las variables demograficas y en cada cuadricula se diferencia por Endpoint. \n
  Formula base del modelo ajustado: {Endpoint} ~ MP2.5 + RM + DiasdesdePrimeraMuerteCOVID + {VariableDemografica} + {VariableSocioeconomica} + %LeñaCalefaccion + HumedadRelativa \n
  {}: Representa la variable modificada en cada análisis mostrado en la figura.")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))

## Figura: MRR Leña segun endpoint, para socioeconomico y meteorologia
df_coef_params %>% 
  filter(term=="scale(perc_lenaCalefaccion)") %>% 
  filter(demografia=="perc_puebloOrig" & lena=="perc_lenaCalefaccion") %>% 
  mutate(socioeconomico=socioeconomico %>% 
           str_remove_all("log\\(|\\)") %>% f_replaceVar(),
         meteorologia=f_replaceVar(meteorologia),
         dependiente=factor(dependiente,
                            levels = c("MR","MR65","MR75",
                                       "LET","CFR0","CFR0a20"))) %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=fct_rev(dependiente), y=estimate))+
  geom_point(size=2, alpha=.5)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_grid(meteorologia~socioeconomico)+
  coord_flip()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="MRR % Leña Calefacción", 
       caption="Coeficiente MP2.5 bajo distintas variables explicativas y endpoints. \n 
  En columnas se presentan variables socioeconomicas, en filas las variables demograficas y en cada cuadricula se diferencia por Endpoint. \n
  Formula base del modelo ajustado: {Endpoint} ~ MP2.5 + RM + DiasdesdePrimeraMuerteCOVID + {VariableDemografica} + {VariableSocioeconomica} + %LeñaCalefaccion + HumedadRelativa \n
  {}: Representa la variable modificada en cada análisis mostrado en la figura.")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))


## Figura: MRR HDD15 segun endpoint, para socioeconomico y leña
df_coef_params %>% 
  filter(term=="scale(heating_degree_15_winter)") %>% 
  filter(demografia=="perc_puebloOrig" & meteorologia=="heating_degree_15_winter") %>% 
  mutate(socioeconomico=socioeconomico %>% 
           str_remove_all("log\\(|\\)") %>% f_replaceVar(),
         lena=f_replaceVar(lena),
         dependiente=factor(dependiente,
                            levels = c("MR","MR65","MR75",
                                       "LET","CFR0","CFR0a20"))) %>% 
  rowid_to_column() %>% 
  ggplot(aes(x=fct_rev(dependiente), y=estimate))+
  geom_point(size=2, alpha=.5)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_grid(lena~socioeconomico)+
  coord_flip()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="MRR HDD 15°C Invierno", 
       caption="Coeficiente MP2.5 bajo distintas variables explicativas y endpoints. \n 
  En columnas se presentan variables socioeconomicas, en filas las variables demograficas y en cada cuadricula se diferencia por Endpoint. \n
  Formula base del modelo ajustado: {Endpoint} ~ MP2.5 + RM + DiasdesdePrimeraMuerteCOVID + {VariableDemografica} + {VariableSocioeconomica} + %LeñaCalefaccion + HumedadRelativa \n
  {}: Representa la variable modificada en cada análisis mostrado en la figura.")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))


## Modelo Causas CardioPulmonares ----------
mod_valido <- glm.nb(def_cardioPulmonar ~ 
                     mp25 +
                     rm +
                     scale(`15-44`) +  scale(`65+`) +
                     scale(perc_vivHacMedio)+
                     scale(perc_puebloOrig) +
                     scale(dias_primerMuerte) +
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


## Modelo RM o no --------
## Base Solo Significativas---------
mod_base <- glm.nb(covid_fallecidos ~ 
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
                   data = df_modelo %>% filter(region!="M"),
                   na.action=na.omit)
summary(mod_base)
nobs(mod_base)
f_tableMRR(mod_base, preview = "none", highlight = T) 
f_figMRR(mod_base)
rm(mod_base)



## Tasa Contagios -----------
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
modelo_let <- read_rds(paste(url, "mod-LET-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep=""))
modelo_cfr0 <- read_rds(paste(url, "mod-CFR0-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep=""))
modelo_cfr0a20 <- read_rds(paste(url, "mod-CFR0a20-Sperc_menor_media-Dperc_puebloOrig-Lperc_lenaCalefaccion-Mhr_anual.rsd", sep=""))


f_tableMRR(modelo_mr, highlight = T); nobs(modelo_mr)
f_tableMRR(modelo_let, highlight = T, preview = "docx"); nobs(modelo_let)
f_tableMRR(modelo_cfr0, highlight = T, preview = "docx"); nobs(modelo_cfr0)
f_tableMRR(modelo_cfr0a20, highlight = T, preview = "docx"); nobs(modelo_cfr0a20)


## EoF