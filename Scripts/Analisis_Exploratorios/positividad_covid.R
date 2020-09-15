### Analisis-COVID-MP2.5
## Analisis de positividad a partir de datos regionales de PCR, y contagios
## PBH Septiembre 2020

theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_Exploratorios/Figuras/PCR/%s.png"
source("Scripts/00-Funciones.R", encoding = "UTF-8")

## Carga Datos -----
source("Scripts/Aggregate_Data/covidCasos_agg.R", encoding = "UTF-8") 
source("Scripts/Load_Data/covidPCR_load.R", encoding = "UTF-8") 
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")

## Agrupo a nivel de region
df_casos_tiempo_region <- df_casos_tiempo %>% 
  left_join(codigos_territoriales) %>% 
  group_by(fecha, codigo_region) %>% 
  summarise(casos=sum(casos, na.rm = T)) %>% ungroup()

## Join PCR + casos --------
dicc_region <- mapa_comuna %>% 
  group_by(codigo_region, region) %>% 
  summarise(count=n()) %>% ungroup() %>% 
  mutate(geometry=NULL, count=NULL)

df_pcr_tiempo <- df_pcr_tiempo %>% 
  left_join(df_casos_tiempo_region) %>% 
  select(-region) %>% left_join(dicc_region)
rm(dicc_region)

## Add poblacion
pob <- df_poblacion %>% group_by(region) %>% 
  summarise(poblacion=sum(poblacion, na.rm=T))
df_pcr_tiempo <- df_pcr_tiempo %>%
  left_join(pob)
rm(pob)

df_pcr_tiempo <- df_pcr_tiempo %>% na.omit()

# Positividad: Moving average 7 days centered
library(zoo)
df <- df_pcr_tiempo %>% 
  arrange(region, fecha) %>% 
  mutate(casos_cmean=rollmean(casos, k=7, fill=NA),
         numero_cmean=rollmean(numero, k=7, fill=NA),
         positividad=casos_cmean/numero_cmean*100)


## Serie tiempo region -------------
# Añado promedio nacional
pob_nac <- df_poblacion$poblacion %>% sum()
df_pcr_tiempo_nac <- df_pcr_tiempo %>%
  group_by(fecha) %>% 
  summarise(casos=sum(casos,na.rm=T),
            numero=sum(numero,na.rm=T)) %>% ungroup() %>% 
  arrange(fecha) %>% 
  mutate(casos_cmean=rollmean(casos, k=7, fill=NA),
         numero_cmean=rollmean(numero, k=7, fill=NA),
         positividad=casos_cmean/numero_cmean*100) %>% 
  mutate(region="Nacional", poblacion=pob_nac)
rm(pob_nac)

df_pcr_tiempo_reg <- df_pcr_tiempo %>%
  group_by(region, fecha) %>% 
  summarise(casos=sum(casos,na.rm=T),
            numero=sum(numero,na.rm=T),
            poblacion=max(poblacion, na.rm=T)) %>% ungroup() %>% 
  arrange(region, fecha) %>% 
  mutate(casos_cmean=rollmean(casos, k=7, fill=NA),
         numero_cmean=rollmean(numero, k=7, fill=NA),
         positividad=casos_cmean/numero_cmean*100) %>% 
  rbind(df_pcr_tiempo_nac)

# Grafico positividad
df_pcr_tiempo_reg %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(x=fecha, y=positividad))+
  geom_line()+
  facet_wrap(~region)+
  labs(x="", y="% Positividad")+
  ggtitle(" Promedio Móvil (7 días) % Positividad PCR")+
  theme(axis.text.x = element_text(angle = 90))
f_savePlot(last_plot(), sprintf(file_name,"PositividadRegion"))

# Grafico Numero test 
df_pcr_tiempo_reg %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(x=fecha, y=numero_cmean))+
  geom_line()+
  facet_wrap(~region)+
  labs(x="", y="N° PCR")+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  ggtitle(" Promedio Móvil (7 días) Número Test PCR")+
  theme(axis.text.x = element_text(angle = 90))
f_savePlot(last_plot(), sprintf(file_name,"PCRRegion"))

# Grafico Numero test por 100 mil hab
df_pcr_tiempo_reg %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(x=fecha, y=numero_cmean/poblacion*1e5))+
  geom_line()+
  facet_wrap(~region)+
  labs(x="", y="N° PCR")+
  ggtitle(" Promedio Móvil (7 días) Número Test PCR cada 100mil hab")+
  theme(axis.text.x = element_text(angle = 90))
f_savePlot(last_plot(), sprintf(file_name,"PCRRegion_hab"))


rm(df_pcr_tiempo_reg, df_pcr_tiempo_nac)


## EoF