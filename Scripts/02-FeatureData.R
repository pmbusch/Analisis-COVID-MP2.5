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
         proxy_lena_calefaccion=perc_lenaCalefaccion/100*heating_degree_15_winter) %>% 
  select(-fecha_cuarentena)

## Relleno NA ----------
# dias cuarentena, muerte y contagio: No todas las comunas tienen cuarentena: defecto=0
# camas: No todas las comunas tienen camas: defecto=0
# tasa_mortalidadAll: no hubo muertes en el periodo temporal elegido: defecto=0
# Superficie: faltan islas de Chile: pascua, juan fernandez y antartica
df_modelo <- df_modelo %>% 
  mutate(dias_cuarentena=if_else(is.na(dias_cuarentena),0, dias_cuarentena),
         dias_primerMuerte=if_else(is.na(dias_primerMuerte),0, dias_primerMuerte),
         dias_primerContagio=if_else(is.na(dias_primerContagio),0, dias_primerContagio),
         tasa_camas=if_else(is.na(tasa_camas),0,tasa_camas),
         tasa_mortalidad_all=if_else(is.na(tasa_mortalidad_all),0,tasa_mortalidad_all))

## Add RM Factor
df_modelo <- df_modelo %>% 
  mutate(rm=if_else(region=="M","RM","Resto Chile"))

df_modelo %>% skim()


## Densidad Poblacion en quintiles ------------
df_modelo <- df_modelo %>% mutate(quintil_dens_pob=qgroup(densidad_pob, 5))
df_modelo %>% group_by(quintil_dens_pob) %>% 
  summarise(count=n()) %>% arrange(desc(count))


## Muertes COVID DEIS ------------
## Muertes grupo etario 65
df_65 <- df_deis %>% 
  group_by(codigo_comuna,grupo_edad,tipo) %>% 
  summarise(covid_fallecidos_deis=n()) %>% ungroup()

df_65 <- df_65 %>% 
  filter(tipo=="confirmado" & grupo_edad=="65+") %>%
  select(-tipo, -grupo_edad) %>% 
  rename(covid_fallecidos_65=covid_fallecidos_deis)

# Add to df_modelo
df_modelo <- df_modelo %>% 
  mutate(covid_fallecidos_65=NULL) %>%  # Para actualizaciones sin duplicados
  left_join(df_65, by=c("codigo_comuna"))
rm(df_65)

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
dicc_variables <- tibble(variable=names(df_modelo),
                         descripcion=names(df_modelo) %>% f_replaceVar())

cat('sep=; \n',file = "Data/Data_Modelo/Diccionario_variables.csv")
write.table(dicc_variables,"Data/Data_Modelo/Diccionario_variables.csv",
            sep=';',row.names = F, append = T)
rm(dicc_variables)

## EoF