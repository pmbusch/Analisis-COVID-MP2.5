### Analisis-COVID-MP2.5
## Analisis generales de los datos comunales
## Correlaciones
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_General/Figuras/%s.png"


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
  dplyr::select(tasa_mortalidad_covid,cfr_raw_0,cfr_raw_0_aplanados,
                cfr_0_20,cfr_0_20_aplanados,
                mp25, `65+`,`75+`,perc_mujer, 
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


## GGPAIRS ------
df_pairs <- df_modelo %>% 
  mutate(rm=if_else(region=="M","RM","Resto Chile") %>% factor()) %>% 
  select(rm,tasa_mortalidad_covid, cfr_raw_0, mp25, densidad_pob_censal,
         perc_fonasa_B, perc_lenaCalefaccion, tmed_winter)
names(df_pairs) <- names(df_pairs) %>% f_replaceVar()
p <- GGally::ggpairs(df_pairs, aes(col=rm, alpha=0.5),
                     columns = 2:ncol(df_pairs))
p
f_savePlot(p, sprintf(file_name, "ggpairs"), dpi=300)
rm(df_pairs,p)

# Correlacion con grouping effect -----------
# Pairwise remuve NA para cada par de vectores comparados, no para la totalidad
df_cor %>% 
  cor(method = "pearson",use="pairwise.complete.obs")

## Correlacion a nivel Nacional
# df_modelo <- df_modelo %>% mutate(mp25=mp25_winter)
n <- df_modelo %>%
  select(tasa_mortalidad_covid, mp25) %>% 
  na.omit() %>% nrow()
corr <- df_modelo %>% 
  select(tasa_mortalidad_covid, mp25) %>% 
  na.omit() %>% 
  {cor(.$tasa_mortalidad_covid,.$mp25,use="complete.obs")}
df_cor <- data.frame(
  region="Nacional",
  count=n,
  corr_mp25=corr)
rm(corr,n)

## Correlaciones por region
cor_region <- df_modelo %>% 
  select(region, tasa_mortalidad_covid, mp25) %>% 
  na.omit() %>% 
  group_by(region) %>% 
  summarise(count=n(),
            corr_mp25=cor(tasa_mortalidad_covid,mp25,use="complete.obs")) %>% 
  rbind(df_cor)
# Tabla resumen
cor_region %>% 
  rename(Region=region, `N° Obs`=count, 
         `Correlación`=corr_mp25) %>% 
  flextable() %>% 
  colformat_num(big.mark=" ", digits=0, j=2,
                na_str="NA") %>%
  colformat_num(big.mark=" ", digits=2, j=3,
                na_str="NA") %>%
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "center", part="all") %>% 
  bold(bold = T, part="body", i=16) %>% 
  flextable::color(color="blue", i = ~ `Correlación`> 0.25, j=3) %>% 
  flextable::color(color="red", i = ~ `Correlación`< -0.25, j=3)
# print(preview="pptx")
rm(cor_region)


## Correlaciones por zona
cor_zona <- df_modelo %>% 
  select(zona, tasa_mortalidad_covid, mp25) %>% 
  na.omit() %>% 
  group_by(zona) %>% 
  summarise(count=n(),
            corr_mp25=cor(tasa_mortalidad_covid,mp25,use="complete.obs")) %>% 
  rbind(df_cor %>% rename(zona=region))
# Tabla resumen
cor_zona %>% 
  rename(Zona=zona, `N° Obs`=count, 
         `Correlación`=corr_mp25) %>% 
  flextable() %>% 
  colformat_num(big.mark=" ", digits=0, j=2,
                na_str="NA") %>%
  colformat_num(big.mark=" ", digits=2, j=3,
                na_str="NA") %>%
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "center", part="all") %>% 
  bold(bold = T, part="body", i=6) %>% 
  flextable::color(color="blue", i = ~ `Correlación`> 0.25, j=3) %>% 
  flextable::color(color="red", i = ~ `Correlación`< -0.25, j=3)
# print(preview="pptx")
rm(cor_zona)

## Correlaciones por zona termica
cor_zona <- df_modelo %>% 
  select(zona_termica, tasa_mortalidad_covid, mp25) %>% 
  na.omit() %>% 
  group_by(zona_termica) %>% 
  summarise(count=n(),
            corr_mp25=cor(tasa_mortalidad_covid,mp25,use="complete.obs")) %>% 
  rbind(df_cor %>% rename(zona_termica=region))
# Tabla resumen
cor_zona %>% 
  rename(`Zona Termica`=zona_termica, `N° Obs`=count, 
         `Correlación`=corr_mp25) %>% 
  flextable() %>% 
  colformat_num(big.mark=" ", digits=0, j=2,
                na_str="NA") %>%
  colformat_num(big.mark=" ", digits=2, j=3,
                na_str="NA") %>%
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "center", part="all") %>% 
  bold(bold = T, part="body", i=8) %>% 
  flextable::color(color="blue", i = ~ `Correlación`> 0.25, j=3) %>% 
  flextable::color(color="red", i = ~ `Correlación`< -0.25, j=3) 
# print(preview="pptx")
rm(cor_zona, df_cor)



## EoF