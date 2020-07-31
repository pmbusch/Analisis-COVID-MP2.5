### Analisis-COVID-MP2.5
## Analisis Transversal
## Fuente principal codigo: Exposure to air pollution and COVID-19 mortality in the United States A nationwide cross-sectional study
## https://github.com/wxwx1993/PM_COVID
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 

## Scatter correlacion -----------
theme_set(theme_bw())

ggplot(df_modelo, aes(mp25, tasa_mortalidad))+
  geom_point()+
  labs(x="Concentración MP2.5 2016-2019 [ug/m3]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]")

last_plot()+
  geom_text_repel(aes(label=nombre_comuna))


## Matriz Correlacion ------------
library(corrplot)

df_modelo %>% na.omit() %>% 
  select_if(is.numeric) %>%
  # select(tasa_mortalidad, mp25, poblacion, `15-44`, `45-64`, `65+`,perc_mujer, 
  #        densidad_pob, perc_rural, perc_material_irrecuperable, 
  #        tasa_contagios, perc_letalidad) %>%
  cor() %>% 
  corrplot(method="color", order="hclust",
           diag=F, tl.cex = 0.7)

png("Correlaciones.png", width = 14.87, height = 9.30, units = "in", res=600)
df_modelo %>% 
  dplyr::select(tasa_mortalidad, mp25, `65+`,perc_mujer, 
         densidad_pob, perc_rural, 
         tasa_contagios,
         dias_primerContagio, dias_cuarentena, tasa_camas,
         ingresoAutonomo_media, perc_isapre, 
         perc_menor_media, perc_ocupado, penetracion_lena, cons_lena_calefactor_pp,
         cons_lena_cocina_pp,
         tmed_summer, tmed_winter, hr_summer, hr_winter) %>%
  na.omit() %>% 
  cor() %>% 
  corrplot(method="circle", 
           # order="hclust",
           # sig.level = 0.05, p.mat=p.mat,
           diag=F, tl.cex = 0.7)
dev.off()


## Funcion significacia
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram#:~:text=Correlogram%20is%20a%20graph%20of,degree%20of%20association%20between%20variables.
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(df_modelo %>% 
                     dplyr::select(tasa_mortalidad, mp25, `65+`,perc_mujer, 
                                   densidad_pob, perc_rural, 
                                   tasa_contagios,
                                   dias_primerContagio, dias_cuarentena, tasa_camas,
                                   ingresoAutonomo_media, perc_isapre, 
                                   perc_menor_media, perc_ocupado, penetracion_lena, cons_lena_calefactor_pp,
                                   cons_lena_cocina_pp,
                                   tmed_summer, tmed_winter, hr_summer, hr_winter) %>%
                     na.omit())



## Modelo-----

library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)

df_modelo %>% names()
mod <- glmer.nb(tasa_mortalidad ~ mp25 + densidad_pob + 
                  scale(log(ingresoAutonomo_mediana)) + scale(perc_ocupado)+
                  scale(perc_menor_media) + scale(perc_isapre) +
                  scale(tasa_camas)+scale(penetracion_lena)+
                  scale(dias_cuarentena)+scale(dias_primerContagio)+
                  scale(`65+`)+
                  (1|nombre_comuna)+
                  offset(log(poblacion)), 
                data = df_modelo)

# Summary
summary(mod)


## Modelo solo MP2.5
modMP <- glmer.nb(tasa_mortalidad ~ mp25 +
                  (1|nombre_comuna),data = df_modelo)

summary(modMP)
