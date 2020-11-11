Códigos
================

**Códigos agrupados:**
* [`Load_Data`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/Load_Data): Carga de datos necesarios
* [`Analisis_Exploratorios`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/Analisis_Exploratorios): Análisis exploratorio datos recopilados 
* [`Aggregate_Data`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/Aggregate_Data): Resume datos a nivel comunal 
* [`Analisis_General`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/Analisis_General): Análisis generales de los datos comunales: Scatter plot, Correlaciones, Densidades, PDF resumen.


**Códigos de análisis principal:**
* [`00-CargaLibrerias.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/00-CargaLibrerias.R): Carga de librerias requeridas 
* [`00-Funciones.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/00-Funciones.R): Funciones transversales al proyecto.
* [`01-LoadAllData.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/01-LoadAllData.R): Consolida toda la información cargada a nivel comunal.
	* [`01-UpdateCovidData.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/01-UpdateCovidData.R): Actualiza datos COVID-19 (alternativa más rápida que cargar todo nuevamente).
* [`02-FeatureData.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/02-FeatureData.R): Trabaja información levantada y genera nuevas feature para el modelo.
* [`03-TablasResumen.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/03-TablasResumen.R): Genera tablas resumen de los datos levantados.
* [`05-AnalisisTransversal.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/05-AnalisisTransversal.R): Modelos Transversales desarrollados *(cross-sectional analysis)*
	* [`05-FuncionesAnalisisTransversal.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/05-FuncionesAnalisisTransversal.R): Funciones para analizar resultados del Modelo Transversal
* [`06-PruebasModelos.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/06-PruebasModelos.R): Script para probar otros Modelos Transversales
* [`06-Stepwise_AnalisisTransversal.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/06-Stepwise_AnalisisTransversal.R): Implementación de algoritmo Stepwise para ajustar Modelo Transversal *(cross-sectional analysis)*
* [`07-AnalisisTransversal_AllCauses.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/07-AnalisisTransversal_AllCauses.R): Modelo Transversal para causas todas las causas de defunción en Chile *(cross-sectional analysis)*
* [`08-ExcelModelos.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/08-ExcelModelos.R): Genera un excel resumen para los modelos probados individualmente.
* [`09-Loop_ModeloTransversal.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/09-Loop_ModeloTransversal.R): Loop para probar sensibilidad de parametros escogidos en demografia, socioeconomico, meteorologia y leña. Guarda los modelos en un excel resumen.


**Otros códigos de análisis:**
* [`MapasInteractivos.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/MapasInteractivos.R): Genera mapas interactivos de datos comunales y de estaciones monitoreo.
* [`DistanciaMonitoreo.R`](https://github.com/pmbusch/Analisis-COVID-MP2.5/tree/master/Scripts/DistanciaMonitoreo.R): Análisis para expandir datos de MP2.5 y meteorología. Calcula la distancia entre centroides de comunas (zonas urbanas) y estaciones de monitoreo.
