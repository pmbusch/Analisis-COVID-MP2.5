### Analisis-COVID-MP2.5
## Analisis generales de los datos comunales
##  Densidades y ECDF
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_General/Figuras/%s.png"


## Densidades  por variable ---------------
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


## EoF