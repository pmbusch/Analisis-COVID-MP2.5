### Analisis-COVID-MP2.5
## Trabaja informacion levantada y genera nuevas feature para el modelo
## PBH Julio 2020

## Load all information ------
# source("Scripts/01-LoadAllData.R", encoding = "UTF-8")
source("Scripts/00-Funciones.R", encoding = "UTF-8")

df_modelo %>% names() %>% sort()
df_modelo %>% skim()
## Parametros -----------
# Superficie de m2 se pasa a km2
# Datos Covid: letalidad
# Proxys lena calefaccion: Heating degree x porcentaje uso leña
df_modelo <- df_modelo %>% 
  mutate(tasa_camas=camas/poblacion*1e5,
         dias_cuarentena=(fecha_muertes-fecha_cuarentena) %>% as.numeric(units="days"),
         densidad_pob=poblacion/superficie*1e6,
         densidad_pob_censal=poblacion/superficie_censal*1e6,
         perc_letalidad=covid_fallecidos/casos_confirmados*100,
         hdd15_winter_lenaCalefaccion=perc_lenaCalefaccion/100*heating_degree_15_winter) %>% 
  select(-fecha_cuarentena)

## Relleno NA ----------
# dias cuarentena, muerte y contagio: No todas las comunas tienen cuarentena: defecto=0
# camas: No todas las comunas tienen camas: defecto=0
# tasa_mortalidadAll: no hubo muertes en el periodo temporal elegido: defecto=0
# Superficie: faltan islas de Chile: pascua, juan fernandez y antartica
df_modelo <- df_modelo %>% 
  replace_na(list(dias_cuarentena=0, dias_primerMuerte=0, dias_primerContagio=0,
                  tasa_camas=0, tasa_mortalidad_all=0, covid_fallecidos_65=0,
                  pda=0))


## Add RM Factor
df_modelo <- df_modelo %>% 
  mutate(rm=if_else(region=="M","RM","Resto Chile"))

## Nombres regiones para output
df_modelo$nombre_region %>% unique()
df_modelo <- df_modelo %>% 
  mutate(region_num=region,
         region=nombre_region %>% 
           str_remove_all("Libertador General Bernardo | del General Carlos Ibanez del Campo| y de la Antartica Chilena| de Santiago| y Parinacota") %>% 
           factor(levels=c("Arica","Tarapaca","Antofagasta","Atacama","Coquimbo",
                         "Valparaiso","Metropolitana","OHiggins","Maule",
                         "Biobio","Nuble","La Araucania","Los Rios","Los Lagos","Aysen",
                         "Magallanes")))
df_modelo$region %>% unique()


## Densidad Poblacion en quintiles ------------
df_modelo <- df_modelo %>% mutate(quintil_dens_pob=qgroup(densidad_pob, 5))
df_modelo %>% group_by(quintil_dens_pob) %>% 
  summarise(count=n()) %>% arrange(desc(count))

df_modelo %>% skim()




## Guardar datos -------
cat('sep=; \n',file = "Data/Data_Modelo/Datos_Modelo.csv")
write.table(df_modelo %>% select(-geometry),"Data/Data_Modelo/Datos_Modelo.csv",
            sep=';',row.names = F, append = T)

saveRDS(df_modelo, "Data/Data_Modelo/Datos_Modelo.rsd")
save.image(".RData")

## Guardar datos aplanados_modelo (std: standard test data format)
df_modelo_std <- df_modelo %>% 
  gather(variable, valor, -codigo_comuna,-codigo_provincia,-codigo_region,
         -nombre_comuna,-nombre_provincia,-nombre_region,-region,-geometry) %>% 
  select(-geometry)


cat('sep=; \n',file = "Data/Data_Modelo/Datos_Modelo_std.csv")
write.table(df_modelo_std,"Data/Data_Modelo/Datos_Modelo_std.csv",
            sep=';',row.names = F, append = T)
rm(df_modelo_std)

## Diccionario variables
dicc_variables <- tibble(
  tipo=names(df_modelo) %>% f_addTypeVar(),
  descripcion=names(df_modelo) %>% f_replaceVar(),
  variable=names(df_modelo)) %>% arrange(tipo)

cat('sep=; \n',file = "Data/Data_Modelo/Diccionario_variables.csv")
write.table(dicc_variables,"Data/Data_Modelo/Diccionario_variables.csv",
            sep=';',row.names = F, append = T)
rm(dicc_variables)


## Separar y guardar segun tipo de dato. Todo se une con codigo de comuna. --------
df_modelo %>% names()

df_datosComuna <- df_modelo %>% 
  dplyr::select(codigo_comuna, nombre_comuna,
                codigo_provincia,  nombre_provincia,
                codigo_region, nombre_region,
                region,  
                region_num, 
                rm,
                superficie,
                perimetro, superficie_censal,
                zona,  zona_termica,
                poblacion,`15-44`,`45-64`,`65-74`,`65+`,
                `75+`,`30+`,
                perc_mujer,
                densidad_pob_manzana_media,
                densidad_pob_manzana_mediana, densidad_pob_manzana_p90,
                quintil_dens_pob,
                densidad_pob,
                densidad_pob_censal,
                hdd15_winter_lenaCalefaccion, 
                mp25,
                mp25_fall, mp25_winter,
                mp25_spring, mp25_summer,
                mp25_2017, mp25_2018,
                mp25_2019, mp25_2020,
                mp10,  mp10_fall,
                mp10_winter, mp10_spring,
                mp10_summer, mp10_2017,
                mp10_2018, mp10_2019,
                mp10_2020, camas,
                heating_degree_15_anual,
                heating_degree_15_fall,  heating_degree_15_spring,
                heating_degree_15_summer,  heating_degree_15_winter,
                heating_degree_18_anual, heating_degree_18_fall,
                heating_degree_18_spring,  heating_degree_18_summer,
                heating_degree_18_winter,  hr_anual,
                hr_fall, hr_spring,
                hr_summer, hr_winter,
                tmed_anual,  tmed_fall,
                tmed_spring, tmed_summer,
                tmed_winter, ingresoTotal_media,
                ingresoTotal_mediana,  ingresoAutonomo_media,
                ingresoAutonomo_mediana, perc_ocupado,
                perc_menor_media,  perc_FFAA,
                perc_fonasa_A, perc_fonasa_B,
                perc_fonasa_C, perc_fonasa_D,
                perc_isapre, perc_salud,
                perc_lenaCocina, perc_lenaCalefaccion,
                perc_lenaAgua, cons_lena_kg,
                perc_puebloOrig, viviendas,
                perc_rural,  perc_material_irrecuperable,
                perc_vivSinHac,  perc_vivHacMedio,
                perc_vivHacCritico,  perc_vivAntes2002,
                pcr_region,  consumo_lena_m3,
                penetracion_lena,  consumo_lena_pp,
                cons_lena_calefactor_pp, cons_lena_cocina_pp,
                cons_lena_urbana,  pda,
                movilidad, 
                tasaMorbilidad_ALL,
                tasaMorbilidad_ALL_65plus, tasaMorbilidad_CPM,
                tasaMorbilidad_CPM_65plus, tasaMorbilidad_CVD,
                tasaMorbilidad_CVD_65plus, tasaMorbilidad_RSP,
                tasaMorbilidad_RSP_65plus, tasa_camas
  )


df_datosCovid <- df_modelo %>% 
  dplyr::select(codigo_comuna,
                covid_fallecidos,  tasa_mortalidad_covid, perc_letalidad,
                dias_primerMuerte, covid_fallecidos_65,
                covid_fallecidos_75, 
                casos_confirmados, tasa_contagios,
                dias_cuarentena,
                dias_primerContagio, cfr_0_20,
                cfr_0_20_aplanados,  cfr_0_30,
                cfr_0_30_aplanados,  cfr_10_20,
                cfr_10_20_aplanados, cfr_raw_0,
                cfr_raw_0_aplanados, cfr_raw_10,
                cfr_raw_10_aplanados,  cfr_raw_20,
                cfr_raw_20_aplanados,  ifr_0_20,
                ifr_0_20_aplanados,  ifr_0_30,
                ifr_0_30_aplanados,  ifr_10_20,
                ifr_10_20_aplanados, ifr_raw_0,
                ifr_raw_0_aplanados, ifr_raw_10,
                ifr_raw_10_aplanados,  ifr_raw_20,
                ifr_raw_20_aplanados
                )


df_datosMortalidad <- df_modelo %>% 
  dplyr::select(codigo_comuna,
                defunciones,
                tasa_mortalidad_all, def_total,
                def_total_30,  def_total_65,
                def_total_75,  def_allCauses,
                def_allCauses_30,  def_allCauses_65,
                def_allCauses_75,  def_extCauses,
                def_extCauses_30,  def_extCauses_65,
                def_extCauses_75,  def_cancer,
                def_cancer_30, def_cancer_65,
                def_cancer_75, def_pulmonar,
                def_pulmonar_30, def_pulmonar_65,
                def_pulmonar_75, def_cardio,
                def_cardio_30, def_cardio_65,
                def_cardio_75, def_cardioPulmonar,
                def_cardioPulmonar_30, def_cardioPulmonar_65,
                def_cardioPulmonar_75
                )

# Save as RDS
saveRDS(df_datosComuna, "Data/Data_Modelo/Datos_Comuna.rsd")
saveRDS(df_datosCovid, "Data/Data_Modelo/Datos_Covid.rsd")
saveRDS(df_datosMortalidad, "Data/Data_Modelo/Datos_Mortalidad.rsd")

## EoF