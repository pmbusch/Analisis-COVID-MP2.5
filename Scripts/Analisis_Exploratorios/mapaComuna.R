### Analisis-COVID-MP2.5
## Mapas de datos comunales
## PBH Noviembre 2020

## Load -------
load(".RData")
file_name <- "Scripts/Analisis_Exploratorios/Figuras/Mapas/%s.png"
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")


# MP2.5 --------
fig_mapaChile_facet(df_modelo, mp25, limites=c(0,50),
                    titulo = "Promedio 2017-2019 \n MP2.5 [ug/m3]")
f_savePlot(last_plot(),
           file_path =sprintf(file_name,"MapaChileMP25Facet"),dpi=300)


# % Uso lena calefaccion --------
fig_mapaChile_facet(df_modelo, perc_lenaCalefaccion, limites=c(0,100),
                    titulo = "% Uso de leña como combustible \n 
                    principal en calefacción")
f_savePlot(last_plot(),
           file_path =sprintf(file_name,"MapaChileLenaFacet"),dpi=300)


# CFR --------
fig_mapaChile_facet(df_modelo, perc_letalidad, limites=c(0,12.5),
                    titulo = "% CFR Covid \n [muertes/casos]")
f_savePlot(last_plot(),
           file_path =sprintf(file_name,"MapaChileCFRFacet"),dpi=300)



## MP2.5 + Monitor --------
## Cargar datos distancia
df_dist <- read_rds("Data/Data_Modelo/distanciacomunaEstacionsinca.rsd")
corte_km <- 20
df_dist <- df_dist %>% filter(dist<corte_km*1e3)

df_dist$dist %>% range()
df_dist %>% names()

fig_mapaChile_facet_monitor(df_modelo, mp25, df_dist,
                            limites=c(0,50),
                            titulo = "Promedio 2017-2019 \n MP2.5 [ug/m3]")
f_savePlot(last_plot(),
           file_path =sprintf(file_name,"MapaChileMP25Facet_ExpMonitor"),dpi=300)


# Lena vs MP2.5 --------
# Nota: Facet no permite cambiar la escala, pero la diferencia es exacta de un 
# factor de 2 (MP2.5 es 0-50 y %Lena es 0-100). Creo una figura de 0-100 y despues
# una figura con la escala correcta de MP2.5, y las junto a mano en el PPT

df_aux1 <- df_modelo %>% mutate(var=perc_lenaCalefaccion, key="% Leña")
df_aux2 <- df_modelo %>% mutate(var=mp25*2, key="MP2.5")

df_aux <- rbind(df_aux1,df_aux2)

fig_mapaChile_facet(df_aux, var, facets=~key, limites=c(0,100),
                    titulo = "% Uso de leña como combustible \n 
                    principal en calefacción")
f_savePlot(last_plot(),
           file_path =sprintf(file_name,"MapaChileLenavsMP25"),dpi=600)


# EoF