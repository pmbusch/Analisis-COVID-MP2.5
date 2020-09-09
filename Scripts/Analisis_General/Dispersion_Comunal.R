### Analisis-COVID-MP2.5
## Analisis generales de los datos comunales
##  Graficos Jitter
## PBH Julio 2020

## Carga Datos a nivel de comuna-----
# source("Scripts/01-LoadAllData", encoding = "UTF-8") 
source("Scripts/00-Funciones.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_General/Figuras/%s.png"


## JITTER COMUNAS -------------
## Porcentajes ----
# Grafico Boxplot y Jitter de variables en percent (misma escala)
df_modelo %>% names()
df_box <- df_modelo %>% 
  mutate(tiene_mp=!is.na(mp25),
         `0-14`=100-`15-44`-`45-64`-`65+`) %>% 
  select(codigo_comuna,perc_letalidad, tiene_mp,
         perc_lenaCocina,perc_lenaCalefaccion,perc_lenaAgua,
         `0-14`,`15-44`,`45-64`,`65+`,perc_mujer,perc_rural,
         perc_puebloOrig,perc_material_irrecuperable,
         perc_menor_media,perc_ocupado,
         perc_fonasa_A,perc_fonasa_B,perc_fonasa_C, 
         perc_fonasa_D,perc_isapre,perc_FFAA,
         hr_anual)

# Aplano y genero columna para orden
df_box <- df_box %>% gather(var,value,-codigo_comuna,-tiene_mp) %>% filter(!is.na(value)) %>% 
  rowid_to_column()
df_box$value %>% range()

# Clasifico por tipo de variable
df_box <- df_box %>% 
  mutate(tipo=case_when(
    var=="perc_letalidad" ~ "COVID-19",
    var %in% c("0-14","15-44","45-64","65+","perc_mujer",
               "perc_rural","perc_puebloOrig",
               "perc_material_irrecuperable") ~ "Demografía",
    var %in% c("perc_menor_media","perc_ocupado",
               "perc_isapre","perc_FFAA","perc_fonasa_A","perc_fonasa_B",
               "perc_fonasa_C", "perc_fonasa_D")  ~ "Socioeconómico",
    var %in% c("perc_lenaCocina","perc_lenaCalefaccion",
               "perc_lenaAgua")  ~ "Leña",
    var %in% c("hr_anual") ~ "Meteorología",
    T ~ "s/i") %>% 
      factor(levels=c("COVID-19","Leña","Demografía","Socioeconómico","Meteorología")))

# Grafico jitter
df_box <- df_box %>% 
  mutate(var=var %>% f_replaceVar())
df_box %>% 
  filter(tiene_mp==T) %>% 
  ggplot(aes(x=reorder(var,desc(rowid)), y=value, col=tipo))+
  # geom_boxplot()+
  geom_jitter(data=filter(df_box, tiene_mp==F) , alpha=.5,col="gray",height= 0)+
  geom_jitter(alpha=.5,height= 0)+ #height 0: remove jitter on Y axis (value)
  # ggforce::facet_col(~tipo, scales="free", space="free")+
  coord_flip(expand = F)+
  # scale_color_viridis_d()+
  labs(x="",y="",col="", 
       caption = "Se muestran en color comunas con MP2.5, y en gris todas")
f_savePlot(last_plot(), sprintf(file_name,"jitter_perc"),dpi=300)
rm(df_box)

## Escala numerica distinta --------
df_box <- df_modelo %>% 
  mutate(tiene_mp=!is.na(mp25)) %>% 
  select(codigo_comuna,tiene_mp,
         tasa_mortalidad_covid, covid_fallecidos, 
         tasa_contagios,casos_confirmados,
         dias_primerContagio,dias_primerMuerte,dias_cuarentena,tasa_camas,
         mp25,
         cons_lena_cocina_pp, cons_lena_calefactor_pp,
         poblacion, densidad_pob,densidad_pob_censal,
         ingresoTotal_media, ingresoAutonomo_media,
         ingresoTotal_mediana, ingresoAutonomo_mediana,
         tmed_anual, heating_degree_15_anual, heating_degree_18_anual) %>% 
  mutate(poblacion=poblacion/1e3,
         ingresoTotal_media=ingresoTotal_media/1e3,
         ingresoAutonomo_media=ingresoAutonomo_media/1e3,
         ingresoTotal_mediana=ingresoTotal_mediana/1e3,
         ingresoAutonomo_mediana=ingresoAutonomo_mediana/1e3)

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
  mutate(tipo=f_addTypeVar(var))

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
rm(df_box, df_label)

## Escala numerica distinta Facet --------
df_modelo %>% names() 
df_box <- df_modelo %>% 
  mutate(tiene_mp=!is.na(mp25)) %>% 
  select(codigo_comuna,tiene_mp,
         tasa_mortalidad_covid, covid_fallecidos, 
         tasa_contagios,casos_confirmados,
         dias_primerContagio,dias_primerMuerte,dias_cuarentena,tasa_camas,
         mp25, mp25_winter,
         densidad_pob,densidad_pob_censal,
         densidad_pob_manzana_p90,
         ingresoTotal_media, ingresoAutonomo_media,
         ingresoTotal_mediana, ingresoAutonomo_mediana,
         tmed_anual, 
         heating_degree_15_anual, heating_degree_18_anual) %>% 
  mutate(ingresoTotal_media=ingresoTotal_media/1e3,
         ingresoAutonomo_media=ingresoAutonomo_media/1e3,
         ingresoTotal_mediana=ingresoTotal_mediana/1e3,
         ingresoAutonomo_mediana=ingresoAutonomo_mediana/1e3,
         # poblacion=poblacion/1e3,
         tasa_contagios=tasa_contagios/1e3,
         casos_confirmados=casos_confirmados/1e3,
         heating_degree_15_anual=heating_degree_15_anual/24,
         heating_degree_18_anual=heating_degree_18_anual/24)

# Aplano y genero columna para orden
df_box <- df_box %>% gather(var,value,-codigo_comuna,-tiene_mp) %>% 
  filter(!is.na(value)) %>% rowid_to_column()
df_box$value %>% range()

df_box <- df_box %>% 
  mutate(tipo=case_when(
    var %in% c("tasa_mortalidad_covid", "covid_fallecidos",
               "tasa_contagios","casos_confirmados",
               "dias_primerContagio","dias_primerMuerte","dias_cuarentena",
               "tasa_camas") ~ "COVID-19",
    var %in% c("mp25","mp25_fall", "mp25_winter", 
               "mp25_spring", "mp25_summer") ~ "MP2.5",
    var %in% c("poblacion","densidad_pob","densidad_pob_censal",
               "densidad_pob_manzana_mediana",
               "densidad_pob_manzana_p90") ~ "Demografía",
    var %in% c("ingresoTotal_media", "ingresoAutonomo_media",
               "ingresoTotal_mediana", "ingresoAutonomo_mediana")  ~ "Socioeconómico",
    var %in% c("tmed_anual", "hr_anual", 
               "heating_degree_15_anual", "heating_degree_18_anual")  ~ "Meteorología",
    T ~ "s/i") %>% 
      factor(levels=c("COVID-19","MP2.5","Demografía","Socioeconómico","Meteorología")))

# Clasficacion para Facet
df_box <- df_box %>% 
  mutate(tipo_facet=case_when(
    var %in% c("tasa_mortalidad_covid", "covid_fallecidos") ~ "Muertes COVID-19",
    var %in% c("tasa_contagios","casos_confirmados") ~ "Miles Casos COVID-19",
    var %in% c("dias_primerContagio","dias_primerMuerte",
               "dias_cuarentena") ~ "Dias pandemia",
    var %in% c("tasa_camas") ~ "Camas",
    var %in% c("mp25","mp25_fall", "mp25_winter", 
               "mp25_spring", "mp25_summer") ~ "MP2.5",
    var %in% c("densidad_pob","densidad_pob_censal",
               "densidad_pob_manzana_mediana",
               "densidad_pob_manzana_p90") ~ "Densidad",
    var %in% c("ingresoTotal_media", "ingresoAutonomo_media",
               "ingresoTotal_mediana", "ingresoAutonomo_mediana")  ~ "Ingresos",
    var %in% c("tmed_anual","heating_degree_15_anual", 
               "heating_degree_18_anual")  ~ "Temperatura",
    T ~ "s/i") %>% 
      factor())
# Grafico
df_box <- df_box %>% 
  mutate(var=f_replaceVar(var))
df_box %>% filter(tiene_mp==T) %>% 
  ggplot(aes(x=reorder(var,desc(rowid)), y=value, col=tipo))+
  geom_jitter(data=filter(df_box, tiene_mp==T), alpha=0.5, col="gray", height= 0)+
  geom_jitter(alpha=.5, height= 0)+ #height 0: remove jitter on Y axis (value)
  geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~reorder(tipo_facet, rowid), scales="free",ncol = 2)+
  # ggforce::facet_col(~reorder(tipo_facet, rowid), scales="free",space="free")+
  coord_flip(expand = F)+
  labs(x="",y="",col="",
       caption = "Se muestran en color comunas con MP2.5, y en gris todas.")
f_savePlot(last_plot(), sprintf(file_name,"jitter_facet_num"),dpi=300)
rm(df_box, df_label)

## Meteorologia por Season -----
df_modelo %>% names()
df_box <- df_modelo %>% 
  mutate(tiene_mp=!is.na(mp25)) %>% 
  select(codigo_comuna, tiene_mp,
         tmed_fall,tmed_winter,tmed_spring,tmed_summer,
         hr_fall,hr_winter,hr_spring,hr_summer,
         heating_degree_15_fall,heating_degree_15_winter,
         heating_degree_15_spring,heating_degree_15_summer,
         heating_degree_18_fall,heating_degree_18_winter,
         heating_degree_18_spring,heating_degree_18_summer) %>% 
  mutate(heating_degree_15_fall=heating_degree_15_fall/24,
         heating_degree_15_winter=heating_degree_15_winter/24,
         heating_degree_15_spring=heating_degree_15_spring/24,
         heating_degree_15_summer=heating_degree_15_summer/24,
         heating_degree_18_fall=heating_degree_18_fall/24,
         heating_degree_18_winter=heating_degree_18_winter/24,
         heating_degree_18_spring=heating_degree_18_spring/24,
         heating_degree_18_summer=heating_degree_18_summer/24)

# Aplano y genero columna para orden
df_box <- df_box %>% gather(var,value,-codigo_comuna,-tiene_mp) %>% filter(!is.na(value)) %>% 
  rowid_to_column()
df_box$value %>% range()

# Clasifico por tipo de variable
df_box <- df_box %>% 
  mutate(tipo=case_when(
    str_detect(var,"tmed") ~ "Temperatura Media",
    str_detect(var,"hr") ~ "Humedad Relativa",
    str_detect(var,"_15") ~ "Heating Degree 15°C",
    str_detect(var,"_18") ~ "Heating Degree 18°C",
    T ~ "s/i") %>% 
      factor(levels=c("Temperatura Media","Humedad Relativa",
                      "Heating Degree 15°C","Heating Degree 18°C")),
    season=case_when(
      str_detect(var,"fall") ~ "Otoño",
      str_detect(var,"winter") ~ "Invierno",
      str_detect(var,"spring") ~ "Primavera",
      str_detect(var,"summer") ~ "Verano",
      T ~ "s/i") %>% 
      factor(levels=c("Otoño","Invierno",
                      "Primavera","Verano")
      ))

# Grafico jitter
df_box %>% 
  filter(tiene_mp==T) %>% 
  ggplot(aes(x=reorder(season,desc(rowid)), y=value, col=tipo))+
  # geom_boxplot()+
  geom_jitter(data=filter(df_box, tiene_mp==F) , alpha=.5,col="gray",height= 0)+
  geom_jitter(alpha=.5,height= 0)+ #height 0: remove jitter on Y axis (value)
  facet_wrap(~tipo, scales = "free")+
  coord_flip(expand = F)+
  labs(x="",y="",col="", 
       caption = "Se muestran en color comunas con MP2.5, y en gris todas")+
  theme(legend.position = "none")
f_savePlot(last_plot(), sprintf(file_name,"jitter_meteo_season"),dpi=300)
rm(df_box)


## EoF