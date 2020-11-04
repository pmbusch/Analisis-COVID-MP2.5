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


# EoF