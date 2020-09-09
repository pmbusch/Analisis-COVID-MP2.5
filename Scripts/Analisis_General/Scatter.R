### Analisis-COVID-MP2.5
## Analisis generales de los datos comunales
## Scatter 
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_General/Figuras/%s.png"

## Scatter correlacion -----------
p1 <- df_modelo %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5,nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(mp25, tasa_mortalidad_covid, size=poblacion, col=rm))+
  geom_point(alpha=.5)+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  labs(x="Concentración MP2.5 2017-2019 [ug/m3]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]",
       size="Poblacion",
       color="")
p1
f_savePlot(p1,
           sprintf(file_name, "Muertes_vs_MP25"), dpi=300)

## Analisis del grouping effect ----
p1+geom_smooth(method = "lm", col="black")
p1+geom_smooth(method = "lm")
p1+geom_smooth(method = "lm",se=F, aes(col=region), data=df_modelo)
p1+geom_smooth(method = "lm",se=F, aes(col=rm), 
               data=df_modelo %>% mutate(rm=if_else(region=="M","RM","Resto Chile") %>% factor()))


# Interactive plot
# plotly::ggplotly(last_plot())
p1+
  geom_text_repel(aes(label=nombre_comuna))
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_MP25_name"), dpi=150)
rm(p1)

## EoF