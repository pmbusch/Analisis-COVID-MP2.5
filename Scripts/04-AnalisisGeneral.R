### Analisis-COVID-MP2.5
## Analisis generales de los datos comunales
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw())

file_name <- "Figuras/%s.png"


## Indicadores relevantes ------------

# Poblacion en comunas con datos MP2.5
total_pob <- df_modelo$poblacion %>% sum()
pob_mp25 <- df_modelo %>% filter(!is.na(mp25)) %>% pull(poblacion) %>% sum()
cat(round(pob_mp25/total_pob*100,1),
    "% de poblacion en comunas con monitoreo de MP2.5")


# Poblacion en comunas con muertes ---------
total_comunas <- nrow(df_modelo)
comunas_muerte <- df_modelo %>% filter(casos_fallecidos>0) %>% nrow()
cat(round(comunas_muerte/total_comunas*100,1),
    "% de comunas con muertes COVID")

rm(total_pob, pob_mp25, total_comunas, comunas_muerte)

## Scatter correlacion -----------
ggplot(df_modelo, aes(mp25, tasa_mortalidad, size=poblacion))+
  geom_point(col="red",alpha=.5)+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  labs(x="Concentración MP2.5 2016-2019 [ug/m3]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]")+
  theme_bw(16)
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_MP25"))

last_plot()+
  geom_text_repel(aes(label=nombre_comuna))
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_MP25_name"))


## Matriz Correlacion ------------
library(corrplot)

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


df_modelo %>% na.omit() %>% 
  select_if(is.numeric) %>%
  # select(tasa_mortalidad, mp25, poblacion, `15-44`, `45-64`, `65+`,perc_mujer, 
  #        densidad_pob, perc_rural, perc_material_irrecuperable, 
  #        tasa_contagios, perc_letalidad) %>%
  cor() %>% 
  corrplot(method="color", order="hclust",
           diag=F, tl.cex = 0.7)

png("Figuras/Correlaciones.png", width = 14.87, height = 9.30, units = "in", res=600)
df_modelo %>% 
  dplyr::select(tasa_mortalidad, mp25, `65+`,perc_mujer, 
                densidad_pob, perc_rural, 
                tasa_contagios,
                dias_primerContagio,dias_primerMuerte, dias_cuarentena, tasa_camas,
                ingresoAutonomo_media, perc_isapre, perc_fonasa_A,
                perc_fonasa_B, perc_fonasa_C, perc_fonasa_D,
                perc_menor_media, perc_ocupado, penetracion_lena, cons_lena_calefactor_pp,
                cons_lena_cocina_pp,
                tmed_summer, tmed_winter, hr_summer, hr_winter) %>%
  na.omit() %>% 
  cor() %>% 
  corrplot(method="circle", 
           # order="hclust",
           type = "upper",
           # sig.level = 0.05, p.mat=p.mat,
           diag=F, tl.cex = 0.7)
dev.off()


## Densidaddes  por variable ---------------
df_modelo %>% 
  select_if(is.numeric) %>% 
  gather(var, valor) %>% 
  ggplot(aes(valor))+
  geom_density(fill="green",alpha=.5)+
  facet_wrap(~var, scales = "free")

ggsave("Figuras/Densidades.png", last_plot(), dpi=900,
       width = 22.3, height = 14, units = "in")

## EoF