### Analisis-COVID-MP2.5
## Agrega poblacion a nivel comunal
## PBH Julio 2020

# Carga datos brutos --------
source("Scripts/Load_Data/mapa_load.R", encoding = "UTF-8")

## Dividir en grupos etarios -----------
df_poblacion$edad %>% unique()
df_poblacion <- df_poblacion %>% 
  mutate(grupo_edad=case_when(
    edad %in% c("0 a 4","5 a 9","10 a 14") ~ "0-14",
    edad %in% c("15 a 19","20 a 24","25 a 29",
                "30 a 34","35 a 39","40 a 44") ~ "15-44",
    edad %in% c("45 a 49","50 a 54","55 a 59","60 a 64") ~ "45-64",
    T ~ "65+"))

df_edad <- df_poblacion %>% 
  group_by(codigo_comuna, grupo_edad) %>% 
  summarise(pob=sum(poblacion,na.rm=T)) %>% 
  mutate(perc_edad=pob/sum(pob)*100,
         pob=NULL) %>% ungroup() %>% 
  filter(grupo_edad!="0-14") %>% spread(grupo_edad,perc_edad)

## Dividir por sexo
df_sexo <- df_poblacion %>% 
  group_by(codigo_comuna, sexo) %>% 
  summarise(pob=sum(poblacion,na.rm=T)) %>% 
  mutate(porc=pob/sum(pob)*100,
         pob=NULL) %>% ungroup() %>% 
  filter(sexo!="hombre") %>% spread(sexo,porc) %>%
  rename(perc_mujer=mujer)


## Poblacion por comuna ----------
df_poblacion <- df_poblacion %>% 
  group_by(codigo_comuna) %>% 
  summarise(poblacion=sum(poblacion,na.rm=T))

# Juntar todo -----
df_poblacion <- left_join(df_poblacion, df_edad) %>% 
  left_join(df_sexo) %>% 
  left_join(codigos_territoriales)

# Limpiar WS
rm(df_edad,df_sexo)


## Agregar codigos regionales y datos de superficie ----------
df_poblacion <- df_poblacion %>% 
  left_join(mapa_regiones %>% select(codigo_region, region)) %>% 
  select(-geometry) %>% 
  left_join(mapa_comuna %>% 
              select(codigo_comuna, superficie, perimetro,superficie_censal,
                     zona,zona_termica))

## Curva acumulada poblacion ------
# df_poblacion$poblacion %>% sum()
# theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
# df_poblacion %>% 
#   arrange(desc(poblacion)) %>% 
#   rowid_to_column() %>% 
#   ggplot(aes(x=reorder(rowid,desc(poblacion)),y=cumsum(poblacion), group=1))+
#   geom_line(size=1)+
#   labs(x="N° Comunas", y="Población acumulada")+
#   scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
#   scale_x_discrete(breaks=seq(0,350,50))
# 
# source("Scripts/00-Funciones.R", encoding = "UTF-8")
# f_savePlot(last_plot(),"Figuras/Analisis_general/PobAcumulada.png",dpi=300)

## Densidad segun zonas censales -----------
# Parametros estadisticos de densidades segun zonas censales
df_zona <- censo_2017_zonas %>%
  left_join(mapa_zonas) %>% 
  left_join(codigos_territoriales) %>%
  mutate(superficie=st_area(geometry) %>% as.numeric(),
         perimetro=st_length(geometry) %>% as.numeric()) %>% 
  na.omit()
df_zona$poblacion %>% sum()

# Densidad
df_zona <- df_zona %>% 
  mutate(densidad_pob_manzana=poblacion/superficie*1e6)

## Agrupar a nivel de comuna
df_zona <- df_zona %>% 
  group_by(codigo_comuna) %>% 
  summarise(densidad_pob_manzana_media=mean(densidad_pob_manzana, na.rm=T),
            densidad_pob_manzana_mediana=median(densidad_pob_manzana, na.rm=T),
            densidad_pob_manzana_p90=quantile(densidad_pob_manzana,0.9, na.rm=T)) %>% 
  ungroup()

## Add to df_poblacion
df_poblacion <- df_poblacion %>% left_join(df_zona)
rm(df_zona)

## EoF