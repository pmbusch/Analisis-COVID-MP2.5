### Analisis-COVID-MP2.5
## Analisis generales de los datos comunales
## Scatter 
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_General/Figuras/%s.png"

df_modelo %>% group_by(zona) %>% summarise(count=n())
df_modelo %>% group_by(zona,region) %>% summarise(count=n())

df_modelo <- df_modelo %>% 
  mutate(Zona=case_when(
    zona=="Norte" ~ "Norte: Arica a Coquimbo",
    zona=="Centro" ~ "Centro: Valparaiso a Maule",
    zona=="RM"~ "Region Metropolitana",
    zona=="Sur"~"Sur: Biobio a Los Rios",
    zona=="Austral"~"Austral: Los Lagos a Magallanes",
    T ~ "s/i") %>% 
      factor(levels = c("Norte: Arica a Coquimbo","Region Metropolitana",
                        "Centro: Valparaiso a Maule","Sur: Biobio a Los Rios",
                        "Austral: Los Lagos a Magallanes")))


## Scatter correlacion -----------
p1 <- df_modelo %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|tasa_mortalidad_covid>200|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(mp25, tasa_mortalidad_covid, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x="Concentración MP2.5 2017-2019 [ug/m3]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]",
       size="Poblacion",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
p1
f_savePlot(p1,
           sprintf(file_name, "Muertes_vs_MP25"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_MP25_name"), dpi=150)

## Analisis del grouping effect ----
p1+geom_smooth(method = "lm", col="black")
p1+geom_smooth(method = "lm")
p1+geom_smooth(method = "lm",se=F, aes(col=region), data=df_modelo)
p1+geom_smooth(method = "lm",se=F, aes(col=rm), 
               data=df_modelo)

# Interactive plot
# plotly::ggplotly(last_plot())
rm(p1)

## Scatter correlacion  CFR-----------
df_plot <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  select(poblacion,nombre_comuna, rm, mp25, 
         cfr_0_20, cfr_0_20_aplanados, cfr_0_30, cfr_0_30_aplanados,
         cfr_10_20, cfr_10_20_aplanados, cfr_raw_0, cfr_raw_0_aplanados,
         cfr_raw_10, cfr_raw_10_aplanados, cfr_raw_20, cfr_raw_20_aplanados) %>% 
  gather(key,cfr,-poblacion,-nombre_comuna, -rm, -mp25)


p1 <- df_plot %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5,nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(mp25, cfr, size=poblacion, col=rm))+
  geom_point(alpha=.5)+
  facet_wrap(~key)+
  ylim(c(0,NA))+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  labs(x="Concentración MP2.5 2017-2019 [ug/m3]", 
       y="% CFR",
       size="Poblacion",
       color="")
p1
f_savePlot(p1,sprintf(file_name, "CFR_vs_MP25"), dpi=300)
# 
# p1+geom_text_repel(aes(label=nombre_comuna))
# f_savePlot(last_plot(),sprintf(file_name, "CFR_vs_MP25_name"), dpi=300)
rm(p1)

## Correlacion Leña ----------
p1 <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|tasa_mortalidad_covid>200|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(perc_lenaCalefaccion, tasa_mortalidad_covid, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x="% Uso leña como combustible principal en Calefacción", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]",
       size="Poblacion",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
p1
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_Lena"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_Lena_name"), dpi=300)

# Consumo lena
p1 <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|tasa_mortalidad_covid>200|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(cons_lena_kg, tasa_mortalidad_covid, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x="Consumo anual leña Casen 2013 [kg/hogar]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]",
       size="Poblacion",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_LenaCons"), dpi=150)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_LenaCons_name"), dpi=150)

## Correlacion ISAPRE ----------
p1 <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|tasa_mortalidad_covid>200|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(perc_isapre, tasa_mortalidad_covid, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x="% Población en previsión de salud Isapre", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]",
       size="Poblacion",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_Isapre"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_Isapre_name"), dpi=300)


## Correlacion Ingreso ----------
p1 <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|tasa_mortalidad_covid>200|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(ingresoAutonomo_media/1e3, tasa_mortalidad_covid, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x="Media ingreso autonomo [miles CLP/mes per capita]", 
       y="Tasa Mortalidad COVID [muertes/100mil hab]",
       size="Poblacion",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_Ingreso"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
f_savePlot(last_plot(),
           sprintf(file_name, "Muertes_vs_Ingreso_name"), dpi=300)

### Mismos graficos pero con CFR ----------

p1 <- df_modelo %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(mp25, perc_letalidad, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  ylim(0,6)+
  labs(x="Concentración MP2.5 2017-2019 [ug/m3]", 
       y="% CFR [muertes/casos]",
       size="Poblacion",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
p1
f_savePlot(p1,
           sprintf(file_name, "CFR_vs_MP25"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
f_savePlot(last_plot(),
           sprintf(file_name, "CFR_vs_MP25_name"), dpi=150)



## Correlacion Leña ----------
p1 <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(perc_lenaCalefaccion, perc_letalidad, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  ylim(0,6)+
  labs(x="% Uso leña como combustible principal en Calefacción", 
       y="% CFR [muertes/casos]",
       size="Poblacion",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
p1
f_savePlot(last_plot(),
           sprintf(file_name, "CFR_vs_Lena"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
f_savePlot(last_plot(),
           sprintf(file_name, "CFR_vs_Lena_name"), dpi=300)

# Consumo lena
p1 <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(cons_lena_kg, perc_letalidad, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  ylim(0,6)+
  labs(x="Consumo anual leña Casen 2013 [kg/hogar]", 
       y="% CFR [muertes/casos]",
       size="Poblacion",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
f_savePlot(last_plot(),
           sprintf(file_name, "CFR_vs_LenaCons"), dpi=150)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
f_savePlot(last_plot(),
           sprintf(file_name, "CFR_vs_LenaCons_name"), dpi=150)

## Correlacion ISAPRE ----------
p1 <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(perc_isapre, perc_letalidad, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  ylim(0,6)+
  labs(x="% Población en previsión de salud Isapre", 
       y="% CFR [muertes/casos]",
       size="Poblacion",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
f_savePlot(last_plot(),
           sprintf(file_name, "CFR_vs_Isapre"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
f_savePlot(last_plot(),
           sprintf(file_name, "CFR_vs_Isapre_name"), dpi=300)


## Correlacion Ingreso ----------
p1 <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(ingresoAutonomo_media/1e3, perc_letalidad, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  ylim(0,6)+
  labs(x="Media ingreso autonomo [miles CLP/mes] per capita", 
       y="% CFR [muertes/casos]",
       size="Poblacion",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
f_savePlot(last_plot(),
           sprintf(file_name, "CFR_vs_Ingreso"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
f_savePlot(last_plot(),
           sprintf(file_name, "CFR_vs_Ingreso_name"), dpi=300)



## EoF