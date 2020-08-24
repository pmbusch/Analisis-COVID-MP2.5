### Analisis-COVID-MP2.5
## Carga Datos Satelitales MP2.5
## Fuente: http://fizz.phys.dal.ca/~atmos/martin/?page_id=140
## PBH Agosto 2020

## Nota: codigo se debe modificar a mano para cargar un set u otro de datos
## Set disponibles: paper 2020, paper 2016, paper 2016 sin dust sin salt
# Por el momento los del paper 2016 dan mejor


## Librerias ------
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
file_name <- "Figuras/Satelital/%s.png"
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
library(raster)
library(RColorBrewer)


# Carga Datos -----
# Elegir uno
file_url <- "Data/Data_Original/Satelital/2020/%s"
file_url <- "Data/Data_Original/Satelital/2016_NoDust/%s"
file_url <- "Data/Data_Original/Satelital/2016/%s"

file_nc <- "ACAG_PM25_GWR_V4GL03_%s01_%s12_0p01.nc" #2020
file_nc <- "GlobalGWRwUni_PM25_GL_%s01_%s12-RH35_Median_NoDust_NoSalt.nc" #2016 No Dust
file_nc <- "GlobalGWRwUni_PM25_GL_%s01_%s12-RH35_Median.nc" #2016

file <- sprintf(file_url,sprintf(file_nc,2016,2016))

mp <- raster(file,
             crs="+init=EPSG:4326")

# transpose and flip the raster to have correct orientation (nc file of 2020 paper)
# mp<-mp %>% flip(direction='x') %>% flip(direction = 'y') %>% t()
mp
mp %>% class()
# plot(mp)
# mapview(mp)

# Filter Chile extent
mp_chile <- crop(mp, extent(-76,-66, -54.85, -17.5))

# View on leaflet
m1 <- mapview(mp_chile, legend=T, col.regions= brewer.pal(9, "YlOrRd"),
        # at = seq(0,50,5)
        maxpixels=3735000)
m1

## Extract data on monitor sites ----------
## Estaciones
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")
df_conc <- df_conc %>% 
  filter(year %in% c(2000:2016) & pollutant=="mp2.5")
estaciones <- df_conc %>% 
  group_by(codigo_comuna,site, longitud, latitud) %>% 
  summarise(avg=mean(valor, na.rm=T)) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)
rm(df_conc)
## Mapa con estaciones
estaciones <- estaciones %>% 
  left_join(mapa_comuna %>% as_tibble() %>% 
              dplyr::select(codigo_comuna, mapa_rm))
# Convertir a sf
estaciones <- st_as_sf(estaciones, 
                       coords = c("longitud","latitud"),
                       remove = F, 
                       crs="+proj=longlat +ellps=GRS80 +no_defs")


## Cruzar raster con latlong points
cruce <- extract(mp_chile, estaciones, method="bilinear")
estaciones <- estaciones %>% mutate(avg_satelite=cruce)


## Cruzar serie de tiempo 2000-2016 -----------
df_cruce <- data.frame()
for (y in 2000:2016){
  cat(y, "\n")
  file <- sprintf(file_url,sprintf(file_nc,y,y))
  mp <- raster(file, crs="+init=EPSG:4326")
  # mp<-mp %>% flip(direction='x') %>% flip(direction = 'y') %>% t()
  cruce <- extract(mp, estaciones, method="bilinear")
  df_aux <- tibble(avg_satelite=cruce, year=y,site=estaciones$site,
                   codigo_comuna=estaciones$codigo_comuna)
  df_cruce <- rbind(df_cruce, df_aux)
  rm(file, mp, cruce, df_aux)
}

##Save file
saveRDS(df_cruce,"Data/cruceSatelite_NoDust.rsd")
df_cruce <- read_rds("Data/cruceSatelite_NoDust.rsd")

## Monitor sites values
df_conc <- read_rds("Data/Data_Modelo/Datos_Concentraciones_raw.rsd")
df_conc <- df_conc %>% 
  filter(year %in% c(2000:2016) & pollutant=="mp2.5")
estaciones <- df_conc %>% 
  group_by(codigo_comuna,site,year, longitud, latitud) %>% 
  summarise(avg=mean(valor, na.rm=T),
            dias=n()) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)
rm(df_conc)
estaciones <- estaciones %>% 
  left_join(mapa_comuna %>% as_tibble())

estaciones <- left_join(estaciones, df_cruce)
estaciones <- estaciones %>% filter(!is.na(avg_satelite))

## Correlations ------
cor(x= estaciones$avg, y= estaciones$avg_satelite,
    use = "complete.obs",
    method = "pearson")
cor(x= estaciones$avg, y= estaciones$avg_satelite,
    use = "complete.obs",
    method = "spearman")

## rangos
estaciones$avg %>% range()
estaciones$avg_satelite %>% range()

## Regresion Monitor vs Satelite -------------
p <- estaciones %>% 
  mutate(rm=if_else(region=="M","RM","Resto Chile") %>% factor()) %>% 
  mutate(texto=paste("Estacion: ",site,"\nComuna: ",nombre_comuna,sep="")) %>% 
  ggplot(aes(avg, avg_satelite, label=texto))+
  geom_point(alpha=.5, aes(col=rm))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  facet_wrap(~year, scales = "free")+
  coord_cartesian(xlim=c(0,70),ylim=c(0,70), expand = F)+
  labs(x="Monitor [ug/m3]", 
       y="Satelite [ug/m3]",
       color="")
p
p_int <- plotly::ggplotly(p)
mapshot(p_int,sprintf(file_name, "Correlaciones_all_interactivo_NoDust") %>% 
          str_replace("png","html"))
rm(p_int)


## Add lm equation
lm_eqn <- function(df){
  m <- lm(y ~ x, data=df)
  eq <- "corr = %s \n y = %sx + %s \n R2 = %s \n N = %s \n Unc = N(%s, %s)"
  sprintf(eq, 
          format(cor(df$x,df$y, method="pearson"), digits=2),
          format(unname(coef(m)[2]), digits = 2),
          format(unname(coef(m)[1]), digits = 2),
          format(summary(m)$r.squared, digits = 3),
          format(nobs(m),digits=0),
          format(mean(summary(m)$residuals), digits=3),
          format(var(summary(m)$residuals), digits=3))
}


data_eq <- estaciones %>% 
  dplyr::rename(x=avg, y=avg_satelite) %>% 
  dplyr::select(x,y,year)
# library(plyr)
eq <- plyr::ddply(.data = data_eq, .variables = plyr::.(year), lm_eqn)
rm(data_eq)


p_eq <- p+geom_label(data=eq,  parse = F,
             # aes(x = 45, y = 11, label=V1),
             aes(x = 4, y = 55, label=V1),
            hjust="inward", size=2.8)+
  geom_smooth(method = "lm", se=T, col="black", formula = "y~x")+
  geom_point(alpha=.5, aes(col=rm))
p_eq
ggsave(sprintf(file_name,"Correlaciones_all_NoDust"), p_eq, dpi=900,
       width = 29.74, height = 18.6, units = "in")
rm(eq,p_eq,p)

## Regresion en el tiempo por monitor ------
n_estaciones <- estaciones %>% group_by(nombre_comuna,site) %>% dplyr::summarise(count=n()) %>% 
  arrange(desc(count))

p_tiempo <- estaciones %>% 
  # filter(site %in% c("Valdivia", "Las Condes","Rancagua I","Las Encinas Temuco")) %>%
  mutate(texto=paste("Estacion: ",site,"\nComuna: ",nombre_comuna,sep="")) %>% 
  mutate(year_label=if_else(year %in% seq(2000,2016,4),
                            year %>% as.character(),"")) %>% 
  ggplot(aes(avg, avg_satelite, label=texto))+
  geom_point(alpha=.5, aes(col=year))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  facet_wrap(~site, scales="free")+
  geom_label_repel(aes(label=year_label))+
  scale_color_distiller(palette = "YlOrRd", type = 'seq', na.value = "white", direction = 1)+
  coord_cartesian(xlim=c(0,70),ylim=c(0,70), expand = F)+
  labs(x="Monitor [ug/m3]", 
       y="Satelite [ug/m3]",
       color="")
p_tiempo
ggsave(sprintf(file_name,"Dispersion_Temporal_NoDust"), p_tiempo, dpi=900,
       width = 29.74, height = 18.6, units = "in")
# p_int <- plotly::ggplotly(p_tiempo)
# mapshot(p_int,sprintf(file_name, "Dispersion_Temporal_interactivo") %>% 
#           str_replace("png","html"))
rm(p_int,p_tiempo)

## Serie temporal
p_serieTiempo <- estaciones %>% 
  # filter(site %in% c("Valdivia", "Las Condes","Rancagua I","Las Encinas Temuco")) %>%
  mutate(texto=paste("Estacion: ",site,"\nComuna: ",nombre_comuna,sep="")) %>%
  rename(Monitor=avg, Satelite=avg_satelite) %>% 
  pivot_longer(cols=c("Monitor","Satelite")) %>% 
  ggplot(aes(year, value,col=factor(name), label=texto))+
  geom_line(aes(group=factor(name)))+
  geom_point(size=0.5)+
  facet_wrap(~site, scales="free")+
  coord_cartesian(ylim=c(0,70), xlim=c(2000,2016), expand = T)+
  labs(x="", y="MP2.5 [ug/m3]",color="")
p_serieTiempo

ggsave(sprintf(file_name,"SerieTemporal_NoDust"), p_serieTiempo, dpi=900,
       width = 29.74, height = 18.6, units = "in")
rm(p_serieTiempo)

# Otras pruebas
cor(estaciones$avg, estaciones$avg_satelite, method="pearson")
mod <- lm(avg_satelite~avg, estaciones)
nobs(mod)
mod <- summary(mod)
mod
mod$r.squared
mod$residuals
mod$residuals %>% mean()
mod$residuals %>% var()
rm(mod, p)


## Mapa estaciones satelite ------
m <- m1+mapview(estaciones, label=estaciones$site, col.region="red")
mapshot(m, url=sprintf(file_name, "Mapa_act_NoDust") %>% str_replace("png","html"),
        selfcontained=F)
rm(m, m1)


## EoF