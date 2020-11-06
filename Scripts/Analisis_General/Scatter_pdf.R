### Analisis-COVID-MP2.5
## Analisis generales de los datos comunales
## Scatter  LOop PDF
## PBH Noviembre 2020


source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_General/Figuras/%s.png"

# Zonas color
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

df_modelo <- df_modelo %>% 
  mutate(tasa_cardioPulmonar=def_cardioPulmonar/poblacion*1e5,
         tasa_cardio=def_cardio/poblacion*1e5,
         tasa_pulmonar=def_pulmonar/poblacion*1e5,
         tasa_allcauses=def_allCauses/poblacion*1e5,
         tasa_total=def_total/poblacion*1e5,
         tasa_cancer=def_cancer/poblacion*1e5)



## Loops a recorrer
eje_y <- c("tasa_mortalidad_covid", "perc_letalidad", "tasa_cardioPulmonar", 
           "tasa_cardio", "tasa_pulmonar", "tasa_cancer", "tasa_allcauses",
           "tasa_total")

labels_y <- c("Tasa Mortalidad COVID [muertes/100mil hab]",
              "% CFR COVID-19 [muertes/casos]",
              "Tasa Mortalidad Cardio Pulmonar \n 2017-2019 [muertes/100mil hab]",
              "Tasa Mortalidad Cardio \n 2017-2019 [muertes/100mil hab]",
              "Tasa Mortalidad Pulmonar \n 2017-2019 [muertes/100mil hab]",
              "Tasa Mortalidad Cancer \n 2017-2019 [muertes/100mil hab]",
              "Tasa Mortalidad All Causes \n 2017-2019 [muertes/100mil hab]",
              "Tasa Mortalidad Total (+ External) \n 2017-2019 [muertes/100mil hab]")


eje_x <- c("mp25", "perc_lenaCalefaccion", "ingresoAutonomo_media", "perc_isapre")
labels_x <- c("Concentración MP2.5 2017-2019 [ug/m3]",
              "% Uso leña como combustible principal en Calefacción",
              "% Población en previsión de salud Isapre",
              "Media ingreso autonomo [miles CLP/mes] per capita")


# Loop
pdf("Scripts/Analisis_General/Figuras/scatter_name.pdf",
    width = 14.87, height = 9.30)
for (y in 1:length(eje_y)){
  for (x in 1:length(eje_x)){
    p1 <- df_modelo %>% 
      filter(Zona!="s/i" & !is.na(mp25)) %>% 
      mutate(nombre_comuna=if_else(poblacion>1e5|tasa_mortalidad_covid>200|mp25>30,
                                   nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
      ggplot(aes_string(eje_x[x], eje_y[y], col="Zona"))+
      geom_point(alpha=.7, aes(size=poblacion))+
      # scale_color_viridis_d()+
      expand_limits(y=0,x=0)+
      scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
      guides(colour = guide_legend(override.aes = list(size=4)))+
      labs(x=labels_x[x], 
           y=labels_y[y],
           size="Poblacion",
           color="")+
      theme_bw(20)+theme(panel.grid.major = element_blank())
    # print(p1)
    print(p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8))
  }
}
dev.off()


## EoF