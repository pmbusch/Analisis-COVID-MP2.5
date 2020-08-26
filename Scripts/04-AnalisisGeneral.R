### Analisis-COVID-MP2.5
## Analisis generales de los datos comunales
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figuras/Analisis_general/%s.png"


## Indicadores relevantes ------------

# Poblacion en comunas con datos MP2.5
df_modelo %>% filter(!is.na(mp25)) %>% nrow()
total_pob <- df_modelo$poblacion %>% sum()
pob_mp25 <- df_modelo %>% filter(!is.na(mp25)) %>% pull(poblacion) %>% sum()
cat(round(pob_mp25/total_pob*100,1),
    "% de poblacion en comunas con monitoreo de MP2.5")

# Poblacion en comunas con muertes ---------
total_comunas <- nrow(df_modelo)
comunas_muerte <- df_modelo %>% filter(covid_fallecidos>0) %>% nrow()
cat(round(comunas_muerte/346*100,1),
    "% de comunas con muertes COVID")

rm(total_pob, pob_mp25, total_comunas, comunas_muerte)

## Scatter correlacion -----------
df_modelo %>% 
  mutate(rm=if_else(region=="M","RM","Resto Chile") %>% factor()) %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5,nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(mp25, tasa_mortalidad_covid, size=poblacion, col=rm))+
  geom_point(alpha=.5)+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  labs(x="Concentración MP2.5 2017-2019 [ug/m3]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]",
       size="Poblacion",
       color="")
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_MP25"), dpi=300)

# Interactive plot
# plotly::ggplotly(last_plot())

last_plot()+
  geom_text_repel(aes(label=nombre_comuna))
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_MP25_name"), dpi=150)


## CORRELACIONES ------------
library(corrplot)

## Con todas las variables posibles!
df_modelo %>% na.omit() %>% 
  select_if(is.numeric) %>%
  # select(tasa_mortalidad, mp25, poblacion, `15-44`, `45-64`, `65+`,perc_mujer, 
  #        densidad_pob, perc_rural, perc_material_irrecuperable, 
  #        tasa_contagios, perc_letalidad) %>%
  cor() %>% 
  corrplot(method="color", order="hclust",
           diag=F, tl.cex = 0.7)

# Con variables de interes
df_cor <- df_modelo %>% 
  dplyr::select(tasa_mortalidad_covid, mp25, `65+`,perc_mujer, 
                densidad_pob, perc_rural, 
                tasa_contagios,
                dias_primerContagio,dias_primerMuerte, dias_cuarentena, tasa_camas,
                ingresoAutonomo_media, perc_isapre, perc_fonasa_A,
                perc_fonasa_B, perc_fonasa_C, perc_fonasa_D,
                perc_menor_media, perc_ocupado, cons_lena_calefactor_pp,
                cons_lena_cocina_pp, perc_lenaCocina, perc_lenaCalefaccion,
                perc_lenaAgua,
                tmed_summer, tmed_winter, hr_summer, hr_winter,
                heating_degree_15_summer,heating_degree_15_winter)

# Pearson
png(sprintf(file_name,"Correlaciones"), width = 14.87, height = 9.30, units = "in", res=200)
df_cor %>% 
  cor(method = "pearson", use="complete.obs") %>% 
  corrplot(method="circle", 
           # order="hclust",
           type = "upper",
           # sig.level = 0.05, p.mat=p.mat,
           diag=F, tl.cex = 0.7)
dev.off()

# Number
png(sprintf(file_name,"CorrelacionesNumber"), width = 14.87, height = 9.30, units = "in", res=200)
df_cor %>% 
  cor(method = "pearson", use="complete.obs") %>% 
  corrplot(method="number", number.cex=0.5,
           # order="hclust",
           type = "upper",
           # sig.level = 0.05, p.mat=p.mat,
           diag=F, tl.cex = 0.7)
dev.off()

# Cluster
png(sprintf(file_name,"CorrelacionesCluster"), width = 14.87, height = 9.30, units = "in", res=600)
df_cor %>% 
  cor(method = "pearson", use="complete.obs") %>% 
  corrplot(method="circle", 
           order="hclust",
           type = "upper",
           # sig.level = 0.05, p.mat=p.mat,
           diag=F, tl.cex = 0.7)
dev.off()

# Spearmean
png(sprintf(file_name,"CorrelacionesSpearman"), width = 14.87, height = 9.30, units = "in", res=600)
df_cor %>% 
  cor(method = "spearman", use="complete.obs") %>% 
  corrplot(method="circle", 
           # order="hclust",
           type = "upper",
           # sig.level = 0.05, p.mat=p.mat,
           diag=F, tl.cex = 0.7)
dev.off()

# Significativas

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
p.mat <- cor.mtest(df_cor, method="pearson", na.action=na.omit)

png(sprintf(file_name,"CorrelacionesSign"), width = 14.87, height = 9.30, units = "in", res=600)
df_cor %>% 
  cor(method = "pearson", use="complete.obs") %>% 
  corrplot(method="circle", 
           # order="hclust",
           type = "upper",
           sig.level = 0.05, p.mat=p.mat,
           diag=F, tl.cex = 0.7)
dev.off()

rm(df_cor, p.mat, cor.mtest)

## Densidaddes  por variable ---------------
df_modelo %>% 
  select_if(is.numeric) %>% 
  gather(var, valor) %>% 
  ggplot(aes(valor))+
  geom_density(fill="green",alpha=.5)+
  facet_wrap(~var, scales = "free")

ggsave(sprintf(file_name,"Densidades"), last_plot(), dpi=900,
       width = 22.3, height = 14, units = "in")

df_modelo %>% 
  select_if(is.numeric) %>% 
  gather(var, valor) %>% 
  ggplot(aes(valor))+
  stat_ecdf()+
  facet_wrap(~var, scales = "free")

ggsave(sprintf(file_name,"Ecdf"), last_plot(), dpi=900,
       width = 22.3, height = 14, units = "in")

## Distribuciones por region --------
# Tomo promedio SIMPLE de las comunas dentro de la region
df_region <- df_modelo %>% group_by(codigo_region) %>% select_if(is.numeric) %>% 
  gather(var, valor,-codigo_region) %>% group_by(codigo_region, var) %>% 
  summarise(valor=mean(valor,na.rm=T))

# Densidad
df_region %>% 
  ggplot(aes(valor))+
  geom_density(fill="green",alpha=.5)+
  facet_wrap(~var, scales = "free")
ggsave(sprintf(file_name,"DensidadesRegion"), last_plot(), dpi=900,
       width = 22.3, height = 14, units = "in")

# ECDF
df_region %>% 
  ggplot(aes(valor))+
  stat_ecdf()+
  facet_wrap(~var, scales = "free")
ggsave(sprintf(file_name,"EcdfRegion"), last_plot(), dpi=900,
       width = 22.3, height = 14, units = "in")

rm(df_region)

## Coeficiente de variacion ----------
df_cv <- df_modelo %>% dplyr::select(-geometry) %>%  
  filter(!is.na(mp25)) %>% skim() %>% 
  mutate(cv=numeric.sd/numeric.mean) %>% 
  dplyr::select(skim_variable, cv) %>% na.omit()

df_cv %>% 
  filter(cv>0.5) %>% 
  ggplot(aes(x=reorder(skim_variable,cv),y=cv))+
  geom_col(fill="brown")+
  coord_flip(expand = F)+
  # coord_cartesian()+
  labs(x="Variable",y="Coeficiente Variacion",
       caption = "Se muestran variables con C.V. mayor a 0.5")+
  theme_bw(16)+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())
f_savePlot(last_plot(), sprintf(file_name,"coefVariacion"))

rm(df_cv)


## Jitter Comunas -------------
# Grafico Boxplot y Jitter de variables en percent (misma escala)
df_modelo %>% names()
df_box <- df_modelo %>% 
  mutate(tiene_mp=!is.na(mp25)) %>% 
  select(codigo_comuna,perc_letalidad, tiene_mp,
         `15-44`,`45-64`,`65+`,perc_mujer,perc_rural,
         perc_puebloOrig,perc_material_irrecuperable,
         perc_menor_media,perc_ocupado,
         perc_fonasa_A,perc_fonasa_B,perc_fonasa_C, 
         perc_fonasa_D,perc_isapre,perc_FFAA,
         perc_lenaCocina,perc_lenaCalefaccion,perc_lenaAgua)

# Aplano y genero columna para orden
df_box <- df_box %>% gather(var,value,-codigo_comuna,-tiene_mp) %>% filter(!is.na(value)) %>% 
  rowid_to_column()
df_box$value %>% range()

# Clasifico por tipo de variable
df_box <- df_box %>% 
  mutate(tipo=case_when(
    var=="perc_letalidad" ~ "COVID-19",
    var %in% c("15-44","45-64","65+","perc_mujer",
               "perc_rural","perc_puebloOrig",
               "perc_material_irrecuperable") ~ "Demografía",
    var %in% c("perc_menor_media","perc_ocupado",
               "perc_isapre","perc_FFAA","perc_fonasa_A","perc_fonasa_B",
               "perc_fonasa_C", "perc_fonasa_D")  ~ "Socioeconómico",
    var %in% c("perc_lenaCocina","perc_lenaCalefaccion",
               "perc_lenaAgua")  ~ "Leña",
    T ~ "s/i") %>% 
      factor(levels=c("COVID-19","Demografía","Socioeconómico","Leña")))
 
# Grafico jitter
df_box <- df_box %>% 
  mutate(var=var %>% f_replaceVar())
df_box %>% 
  filter(tiene_mp==T) %>% 
  ggplot(aes(x=reorder(var,desc(rowid)), y=value, col=tipo))+
  # geom_boxplot()+
  geom_jitter(data=filter(df_box, tiene_mp==F) , alpha=.5,col="gray",height= 0)+
  geom_jitter(alpha=.5,height= 0)+ #height 0: remove jitter on Y axis (value)
  coord_flip(expand = F)+
  # scale_color_viridis_d()+
  labs(x="",y="",col="", 
       caption = "Se muestran en color comunas con MP2.5, y en gris todas")
f_savePlot(last_plot(), sprintf(file_name,"jitter_perc"),dpi=300)

rm(df_box)

## Variables con escala numerica distinta --
df_box <- df_modelo %>% 
  mutate(tiene_mp=!is.na(mp25)) %>% 
  select(codigo_comuna,tiene_mp,
         tasa_mortalidad_covid, covid_fallecidos, 
         tasa_contagios,casos_confirmados,
         dias_primerContagio,dias_primerMuerte,dias_cuarentena,tasa_camas,
         mp25,
         poblacion, densidad_pob,densidad_pob_censal,
         ingresoTotal_media, ingresoAutonomo_media,
         tmed_anual, hr_anual, 
         heating_degree_15_anual, heating_degree_18_anual) %>% 
  mutate(poblacion=poblacion/1e3,
         ingresoTotal_media=ingresoTotal_media/1e3,
         ingresoAutonomo_media=ingresoAutonomo_media/1e3)

## Labels de promedios y desv estandar
df_mean <- df_box %>% filter(tiene_mp==T) %>% 
  mutate_if(is.numeric, mean, na.rm=T) %>% 
  select(-codigo_comuna, -tiene_mp) %>% head(1) %>% gather(var, mean)
df_sd <- df_box %>% filter(tiene_mp==T) %>% 
  mutate_if(is.numeric, sd, na.rm=T) %>% 
  select(-codigo_comuna,-tiene_mp) %>% head(1) %>% gather(var, sd)

df_label <- cbind(df_mean, df_sd %>% select(sd))
rm(df_mean,df_sd)

df_label <- df_label %>% 
  mutate(mean = round(mean,2),
         sd= round(sd,2),
         label=paste(mean," (",sd,")",sep="")) %>% rowid_to_column()

# Estandarizo
df_box <- df_box %>% mutate_if(is.numeric, scale)
df_box <- df_box %>% gather(var,value,-codigo_comuna,-tiene_mp) %>% 
  filter(!is.na(value)) %>% 
  rowid_to_column()
df_box$value %>% range()

df_box <- df_box %>% 
  mutate(tipo=case_when(
    var %in% c("tasa_mortalidad_covid", "covid_fallecidos",
               "tasa_contagios","casos_confirmados",
               "dias_primerContagio","dias_primerMuerte","dias_cuarentena",
               "tasa_camas") ~ "COVID-19",
    var == "mp25" ~ "MP2.5",
    var %in% c("poblacion","densidad_pob","densidad_pob_censal") ~ "Demografía",
    var %in% c("ingresoTotal_media", "ingresoAutonomo_media")  ~ "Socioeconómico",
    var %in% c("tmed_anual", "hr_anual", 
               "heating_degree_15_anual", "heating_degree_18_anual")  ~ "Meteorología",
    T ~ "s/i") %>% 
      factor(levels=c("COVID-19","MP2.5","Demografía","Socioeconómico","Meteorología")))

df_label <- df_label %>% left_join(df_box %>% group_by(var,tipo) %>% 
                                     summarise(count=n())) %>% 
  mutate(var=f_replaceVar(var))

# Grafico
df_box <- df_box %>% 
  mutate(var=f_replaceVar(var))
df_box %>% filter(tiene_mp==T) %>% 
  ggplot(aes(x=reorder(var,desc(rowid)), y=value, col=tipo))+
  geom_label(data = df_label,y=6, aes(label=label))+
  # geom_boxplot()+
  geom_jitter(data=filter(df_box, tiene_mp==T), alpha=0.5, col="gray", height= 0)+
  geom_jitter(alpha=.5, height= 0)+ #height 0: remove jitter on Y axis (value)
  geom_hline(yintercept = 0, linetype = "dashed")+
  coord_flip(expand = F)+
  # scale_color_viridis_d()+
  labs(x="",y="",col="",
       caption = "Se muestran en color comunas con MP2.5, y en gris todas.\n
       Variables estandarizadas. Mean (sd)")
f_savePlot(last_plot(), sprintf(file_name,"jitter_scale"),dpi=300)

rm(df_box)

## EoF