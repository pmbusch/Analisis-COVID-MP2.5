### Analisis-COVID-MP2.5
## Analisis generales de los datos comunales
## Coeficiente Variacion sd/mean
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_General/Figuras/%s.png"




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

## EoF