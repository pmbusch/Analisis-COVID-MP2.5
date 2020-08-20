Códigos
================

**Códigos agrupados:**
* [`Load_Data`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/Load_Data): Carga de datos necesarios
* [`Analisis_Exploratorios`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/Analisis_Exploratorios): Análisis exploratorio datos recopilados 
* [`Aggregate_Data`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/Aggregate_Data): Resume datos a nivel comunal 

**Códigos de análisis principal:**
* [`00-CargaLibrerias.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/00-CargaLibrerias.R): Carga de librerias requeridas 
* [`00-Funciones.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/00-Funciones.R): Funciones transversales al proyecto.
* [`01-LoadAllData.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/01-LoadAllData.R): Consolida toda la información cargada a nivel comunal.
* [`02-FeatureData.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/02-FeatureData.R): Trabaja información levantada y genera nuevas feature para el modelo.
* [`03-TablasResumen.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/03-TablasResumen.R): Genera tablas resumen de los datos levantados.
* [`04-AnalisisGeneral.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/04-AnalisisGeneral.R): Análisis generales de los datos comunales: Indicadores, Scatter plot, Correlaciones y Densidades.
* [`05-AnalisisTransversal.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/05-AnalisisTransversal.R): Modelo Transversal *(cross-sectional analysis)*

**Otros códigos de análisis:**
* [`MapasInteractivos.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/MapasInteractivos.R): Genera mapas interactivos de datos comunales y de estaciones monitoreo.
* [`DistanciaMonitoreo.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/DistanciaMonitoreo.R): Análisis para expandir datos de MP2.5 y meteorología. Calcula la distancia entre centroides de comunas (zonas urbanas) y estaciones de monitoreo.
