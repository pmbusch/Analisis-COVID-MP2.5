Carga de Datos 
================
**Fuente Datos:**
* Datos COVID-19: MinCiencia (https://github.com/MinCiencia/Datos-COVID19/)
	* Casos fallecidos por comuna *(producto 38)*
	* Casos totales por comuna incremental *(producto 1)*
	* Exámenes PCR por región *(producto 7)*
	* Cuarentenas Activas e Históricas *(producto 29)*
* Concentración ambiental: SINCA (https://sinca.mma.gob.cl/)
	* MP2.5
	* CO
	* NO2
* Tasa de Mortalidad: CENSO 2017 y DEIS (compilado por MinCiencia)
	* Población 
	* Defunciones *(producto 32)*
* Infraestructura Sanitaria: DEIS (https://deis.minsal.cl/)
	* Número de Camas *(Listado de Establecimientos de Salud)*
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