### Analisis-COVID-MP2.5
## Analisis generales de los datos comunales
## Boxplot Cuartiles
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_General/Figuras/%s.png"


## Boxplot MP2.5 Quintiles ------------
# Nota: Grafico inspirado en presentacion CR2: http://www.cr2.cl/contaminacion
df_cuartil <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  mutate(cuartil_mp25=qgroup(mp25, 4))

label_cuartil <- df_cuartil %>% group_by(cuartil_mp25) %>% 
  summarise(count=n(), mean_mp25=mean(mp25,na.rm=T),
            min_mp=min(mp25,na.rm=T) %>% round(1),
            max_mp=max(mp25,na.rm=T) %>% round(1)) %>% 
  ungroup() %>% 
  mutate(cuartil_label=paste(cuartil_mp25,"\n [",min_mp," - ",max_mp,"]",sep=""))

df_cuartil <- df_cuartil %>% left_join(label_cuartil, by=c("cuartil_mp25"))
rm(label_cuartil)

# Label de comunas incluidas en cada cuartil,ordenadas por contaminacion
comuna_label <- df_cuartil %>% group_by(cuartil_mp25, cuartil_label) %>% 
  arrange(mp25) %>% 
  summarise(comunas=toString(nombre_comuna)) %>% ungroup() %>% 
  mutate(comunas=paste(cuartil_mp25,": ", comunas,"\n", sep=""))

df_cuartil %>% 
  ggplot(aes(cuartil_label, tasa_mortalidad_covid))+
  geom_boxplot()+
  geom_point(data=comuna_label,y=0, aes(col=str_wrap(comunas,40)),alpha=0)+ ##labels as legend
  labs(y="Tasa Mortalidad COVID-19 [por 100mil]", x="Cuartil MP2.5 [ug/m3]", 
       col="", caption="Comunas en leyenda ordenadas de menor a mayor concentración")+
  coord_cartesian(expand = T)+
  theme(legend.text = element_text(size=8),
        legend.key.height = unit(6, 'lines'))
f_savePlot(last_plot(), sprintf(file_name,"Boxplot_Cuartil"))
rm(df_cuartil, comuna_label)


## EoF