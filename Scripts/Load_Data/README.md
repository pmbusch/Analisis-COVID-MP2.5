Carga de Datos 
================
**Fuente Datos:**
* Muertes COVID-19: DEIS (https://deis.minsal.cl/#datosabiertos)
	* Defunciones por causa de muerte 2016-2020
* Datos COVID-19: MinCiencia (https://github.com/MinCiencia/Datos-COVID19/)
	* Casos totales por comuna incremental *(producto 1)*
	* Exámenes PCR por región *(producto 7)*
	* Cuarentenas Activas e Históricas *(producto 29)*
	* Indice de Movilidad (viajes comuna por habitante) *(producto 33)*
* Concentración ambiental (2017-2019): SINCA (https://sinca.mma.gob.cl/)
	* MP2.5
* Infraestructura Sanitaria (2019): DEIS (https://deis.minsal.cl/)
	* Número de Camas *(Listado de Establecimientos de Salud)*
* Demografía: CENSO 2017 (https://www.censo2017.cl/)
	* Población
	* Urbano-Rural
	* Población originaria
	* Viviendas
	* Materialidad Viviendas
* Demografía Viviendas: Observatorio Urbano MINVU (https://www.observatoriourbano.cl/estadisticas-habitacionales/)
	* Indice de Hacinamiento en Viviendas
	* Permisos Edificación 2002-2017
* Mapas de Chile: librería de R *chilemapas* (https://cran.r-project.org/web/packages/chilemapas/index.html)
* Meteorología (2017-2019): Dirección Meteorológica de Chile (https://climatologia.meteochile.gob.cl/) y SINCA (https://sinca.mma.gob.cl/)
	* Temperatura (media, mínima y máxima diaria)
	* Humedad relativa
* Información Socioeconómica: CASEN 2017 (http://observatorio.ministeriodesarrollosocial.gob.cl/casen-multidimensional/casen/casen_2017.php)
	* Ingreso total e ingreso autónomo: *ytotcor* e *yautcor*
	* Nivel educacional: *e6a* 
	* Previsión de Salud: *s12* 
	* Tratamiento médico: *s28*
	* Ocupado: *o1*
	* Factor expansión: *expc*
* Consumo de Leña (2018): 
	* CDT (2015). "Medición del consumo nacional de leña y otros combustibles sólidos derivados de la madera”. Tabla 136.
	* CDT, In-Data SpA. (2019). Usos de energía de los Hogares en Chile 2018. Santiago.
		* Base de datos disponible en: https://www.energia.gob.cl/documentos/bbdd-estudio-caracterizacion-residencial-2018
	* CASEN 2017
		* Combustible principal utilizado para cocinar, calefacción, agua caliente: *v36a*, *v36b*, *v36c*, *expc*
	* CASEN 2013
		* Kilogramos consumidos anualmente: *v37*
