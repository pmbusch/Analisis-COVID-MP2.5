Evaluación de los efectos de la exposición prolongada a MP2.5 derivado del uso de leña y las incidencias por COVID-19 en el centro sur de Chile
================
Análisis epidemiológico de la incidencia de COVID-19 y el uso intensivo de leña en las regiones sur y centro de Chile

Se busca determinar la relación entre la contaminación atmosférica de MP2.5 generada por la combustión de leña y 
los contagios y fatalidades del COVID-19

El proyecto se subdivide en las siguientes secciones:
* Carga de datos necesarios
* Análisis de la relación mediante series de tiempo y estudios transversales

**Nota:** Si se desea explorar los scripts por separado se recomienda ejecutar el siguiente código para cargar todas las librerías necesarias: *source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")*

**Fuente Datos:**
* Datos COVID-19: MinCiencia (https://github.com/MinCiencia/Datos-COVID19/)
	* Casos fallecidos por comuna **(producto 38)** o https://github.com/MinCiencia/Datos-COVID19/tree/master/output/producto50
	* Casos totales por comuna incremental **(producto 1)**
	* Exámenes PCR por región **(producto 7)**
	* Cuarentenas Activas e Históricas **(producto 29)**
* Tasa de Mortalidad: CENSO 2017 y DEIS (compilado por MinCiencia
	* Población 
	* Defunciones **(producto 32)**
* Concentración ambiental: SINCA (https://sinca.mma.gob.cl/)
	* MP2.5
	* CO
	* NO2
* Demografía: CENSO 2017 (https://www.censo2017.cl/)
	* Población
	* Urbano-Rural
	* Población originaria
	* Viviendas
	* Materialidad Viviendas
* Mapas de Chile: librería de R *chilemapas* (https://cran.r-project.org/web/packages/chilemapas/index.html)
* Meteorología: Dirección Meteorológica de Chile (https://climatologia.meteochile.gob.cl/)
	* Temperatura (media, mínima y máxima diaria)
	* Humedad
* Información Socioeconómica: CASEN 2017 (http://observatorio.ministeriodesarrollosocial.gob.cl/casen-multidimensional/casen/casen_2017.php)
	* Ingreso total e ingreso autónomo: *ytotcor* e *yautcor*
	* Nivel educacional: *e6a* 
	* Previsión de Salud: *s12* 
	* Ocupado: *o1*
	* Factor expansión: *expc*
* Consumo de Leña: 
	* CDT 2015. "Medición del consumo nacional de leña y otros combustibles sólidos derivados de la madera”. Tabla 136.
