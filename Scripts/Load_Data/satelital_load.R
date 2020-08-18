### Analisis-COVID-MP2.5
## Carga Datos Satelitales MP2.5
## Fuente: http://fizz.phys.dal.ca/~atmos/martin/?page_id=140
## PBH Agosto 2020

## Nota: codigo se debe modificar a mano para cargar un set u otro de datos
# Por el momento los del paper 2016 dan mejor


## Librerias ------
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
file_name <- "Figuras/Satelital/%s.png"
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
library(raster)
library(RColorBrewer)


# Carga Datos -----
file_url <- "Data/Data_Original/Satelital/2016/%s"
file_url <- "Data/Data_Original/Satelital/2020/%s"

file_nc <- "GlobalGWRwUni_PM25_GL_%s01_%s12-RH35_Median.nc"
file_nc <- "ACAG_PM25_GWR_V4GL03_%s01_%s12_0p01.nc"


file <- sprintf(file_url,
                "GlobalGWRnoGWRcwUni_PM25_GL_201601_201612-RH35-NoNegs.asc")
# Da cor 0.5
# file <- sprintf(file_url,
#                 "GlobalGWRwUni_PM25_GL_201601_201612-RH35-NoNegs.asc")
file <- sprintf(file_url,sprintf(file_nc,2013,2013))


## Actualizado 2020 
file <- sprintf(file_url,
                "ACAG_PM25_GWR_V4GL03_201601_201612_0p05.nc")
## Actualizado 2020 mayor resolucion (muy lento de cargar)
# da cor -0.11
file <- sprintf(file_url,
                "ACAG_PM25_GWR_V4GL03_201601_201612_0p01.nc")

# mp <- read_file(file)
mp <- raster(file,
             crs="+init=EPSG:4326")
# transpose and flip the raster to have correct orientation (nc file of 2020 paper)
mp<-mp %>% flip(direction='x') %>% flip(direction = 'y') %>% t()

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
  filter(year %in% c(2013:2016) & pollutant=="mp2.5")
estaciones <- df_conc %>% 
  group_by(codigo_comuna,site, longitud, latitud) %>% 
  summarise(avg=mean(valor, na.rm=T)) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)
# rm(df_conc)
## Mapa con estaciones
estaciones <- estaciones %>% 
  left_join(mapa_comuna %>% as_tibble() %>% 
              dplyr::select(codigo_comuna, mapa_rm))
# Convertir a sf
estaciones <- st_as_sf(estaciones, 
                       coords = c("longitud","latitud"),
                       remove = F, 
                       crs="+proj=longlat +ellps=GRS80 +no_defs")



## Cruzar raster con latlong points -------
cruce <- extract(mp_chile, estaciones, method="bilinear")

estaciones <- estaciones %>% mutate(avg_satelite=cruce)


## Cruzar serie de tiempo 2013-2016
df_cruce <- data.frame()
for (y in 2013:2016){
  cat(y, "\n")
  file <- sprintf(file_url,sprintf(file_nc,2016,2016))
  mp <- raster(file, crs="+init=EPSG:4326")
  # mp<-mp %>% flip(direction='x') %>% flip(direction = 'y') %>% t()
  cruce <- extract(mp, estaciones, method="bilinear")
  df_aux <- tibble(avg_satelite=cruce, year=y,site=estaciones$site,
                   codigo_comuna=estaciones$codigo_comuna)
  df_cruce <- rbind(df_cruce, df_aux)
  rm(file, mp, cruce, df_aux)
}

##Save file
saveRDS(df_cruce,"cruce_act.rsd")
df_cruce <- read_rds("cruce.rsd")

estaciones <- df_conc %>% 
  group_by(codigo_comuna,site,year, longitud, latitud) %>% 
  summarise(avg=mean(valor, na.rm=T)) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)

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
# Plot. Label lo incluyo para que se muestre en el grafico interactivo
p <- estaciones %>% 
  mutate(rm=if_else(region=="M","RM","Resto Chile") %>% factor()) %>% 
  mutate(texto=paste("Estacion: ",site,"\nComuna: ",nombre_comuna,sep="")) %>% 
  ggplot(aes(avg, avg_satelite, label=texto))+
  geom_point(alpha=.5, aes(col=rm))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  facet_wrap(~year)+
  coord_cartesian(xlim=c(0,70),ylim=c(0,70), expand = F)+
  labs(x="Monitor [ug/m3]", 
       y="Satelite [ug/m3]",
       color="")
p
p_int <- plotly::ggplotly(p)
mapshot(p_int,sprintf(file_name, "Correlaciones_all_interactivo") %>% 
          str_replace("png","html"))
rm(p_int)

# f_savePlot(p, sprintf(file_name, "Correlaciones2016",dpi=300))


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
library(plyr)
eq <- plyr::ddply(.data = data_eq, .variables = .(year), lm_eqn)
rm(data_eq)


p+geom_label(data=eq,  parse = F,
             # aes(x = 45, y = 11, label=V1),
             aes(x = 4, y = 55, label=V1),
            hjust="inward", size=2.8)+
  geom_smooth(method = "lm", se=F, col="red", formula = "y~x")+
  geom_point(alpha=.5, aes(col=rm))
rm(eq)

f_savePlot(last_plot(), sprintf(file_name, "Correlaciones_all_act",dpi=300))

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
mapshot(m, url=sprintf(file_name, "Mapa__act") %>% str_replace("png","html"),
        selfcontained=F)
rm(m, m1)


## EoF