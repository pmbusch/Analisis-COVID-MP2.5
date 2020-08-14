### Analisis-COVID-MP2.5
## Tablas Resumen Info
## PBH Julio 2020

# Tablas Resumen general Promedio (sd) ---------------
df_modelo %>% names()
df <- df_modelo %>% 
  mutate(perc_fonasa=perc_fonasa_A+perc_fonasa_B+perc_fonasa_C+perc_fonasa_D,
         poblacion=poblacion/1e3,
         ingresoAutonomo_media=ingresoAutonomo_media/1e3) %>% 
  select(tasa_mortalidad, mp25, 
         poblacion,densidad_pob, `15-44`, `45-64`, `65+`,perc_mujer, 
         perc_rural, perc_puebloOrig, perc_material_irrecuperable, 
         tasa_contagios, perc_letalidad,dias_primerContagio, 
         dias_primerMuerte, dias_cuarentena, tasa_camas,tasa_mortalidad_all,
         ingresoAutonomo_media, perc_isapre, perc_fonasa,
         perc_menor_media, perc_ocupado, 
         cons_lena_calefactor_pp,cons_lena_cocina_pp,perc_lenaCocina,
         perc_lenaCalefaccion,perc_lenaAgua,
          hr_summer, hr_winter, tmed_summer, tmed_winter,
         heating_degree_15_summer, heating_degree_15_winter) %>% 
  rename(
    `Tasa Mortalidad COVID [por 100mil]`=tasa_mortalidad,
    `MP2.5 [ug/m3]`=mp25,
    `Poblacion [miles]`=poblacion,
    `Densidad [hab/km2]`=densidad_pob,
    `% 15-44`=`15-44`,
    `% 45-64`=`45-64`,
    `% 65+`=`65+`,
    `% Mujer`=perc_mujer,
    `% Rural`=perc_rural,
    `% Pueblo Originario`=perc_puebloOrig,
    `% Vivienda con Material irrecuperable`=perc_material_irrecuperable,
    `Tasa Contagios COVID [por 100mil]`=tasa_contagios,
    `% Letalidad COVID`=perc_letalidad,
    `Dias desde primer contagio`=dias_primerContagio,
    `Dias desde primera muerte`=dias_primerMuerte,
    `Dias desde cuarentena`=dias_cuarentena,
    `Camas hospitalarias [por 100mil]`=tasa_camas,
    `Tasa Mortalidad Total [por 100mil]`=tasa_mortalidad_all,
    `Media Ingreso autonomo mensual [miles CLP]`=ingresoAutonomo_media,
    `% Isapre`=perc_isapre,
    `% Fonasa`=perc_fonasa,
    `% Educación menor a media`=perc_menor_media,
    `% Ocupado laboral`=perc_ocupado,
    `Consumo anual leña calefactor [kWh per cápita]`=cons_lena_calefactor_pp,
    `Consumo anual leña cocina [kWh per cápita]`=cons_lena_cocina_pp,
    `% uso leña cocina`=perc_lenaCocina,
    `% uso leña calefaccion`=perc_lenaCalefaccion,
    `% uso leña agua caliente`=perc_lenaAgua,
    `Humedad relativa media Verano [%]`=hr_summer,
    `Humedad relativa media Invierno [%]`=hr_winter,
    `Temperatura media Verano [°C]`=tmed_summer, 
    `Temperatura media Invierno [°C]`=tmed_winter,
    `Heating Degree 15°C Verano [°C]`=heating_degree_15_summer,
    `Heating Degree 15°C Invierno [°C]`=heating_degree_15_winter)

# separo por la condicion de si tiene o no estacion
df_sep <- df %>% 
  mutate(tiene_estacion=if_else(!is.na(`MP2.5 [ug/m3]`),"si","no")) %>% 
  group_by(tiene_estacion) %>% skim() %>% 
  mutate(indicador=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep="")) %>% 
  select(skim_variable, tiene_estacion, indicador) %>% 
  spread(tiene_estacion, indicador)
  
df_skim <- df %>% skim() %>% 
  mutate(indicador=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep="")) %>% 
  select(skim_variable, indicador) %>% 
  left_join(df_sep)

n_mp25 <- df_modelo %>% filter(!is.na(mp25)) %>% nrow()
foot_note <- paste("n:",c(nrow(df_modelo),nrow(df_modelo)-n_mp25,
                          n_mp25),"comunas",sep=" ")

df_skim %>% 
  rename(Variable=skim_variable, Total=indicador, 
         `Sin MP2.5`=no, `Con MP2.5`=si) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "left", part="all") %>% 
  align(j=2, align = "center", part="all") %>% 
  footnote(j=2:4, value=as_paragraph(foot_note), part="header", inline=T) 
  # print(preview="pptx")

rm(foot_note,df_skim, df_sep, n_mp25)

## Tablas Rango -------------
# Incluyo coeficiente de variacion: ayuda a ver que variables tienen mayor variabilidad entre comunas
# Nota: Solo es valido para variables con escala de partida en 0 (ratios). T° no aplica
# https://en.wikipedia.org/wiki/Coefficient_of_variation
df_skim <- df %>% filter(!is.na(`MP2.5 [ug/m3]`)) %>% skim() %>% 
  mutate(cv=numeric.sd/numeric.mean) %>% 
  select(skim_variable, numeric.mean, cv,
         numeric.p0, numeric.p50, numeric.p100) %>% 
  rename(Variable=skim_variable,
         Promedio=numeric.mean,
         `C.V.`=cv,
         Min=numeric.p0, Mediana=numeric.p50, Max=numeric.p100)

df_skim %>% 
  flextable() %>% 
  colformat_num(big.mark=" ", digits=1, j=2:ncol(df_skim),
                na_str="s/i") %>%
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "left", part="all") %>% 
  footnote(j=4, value=as_paragraph("Coeficiente Variación"), 
           part="header", inline=T) 
  # print(preview="pptx")

rm(df_skim, df)
  

## Tablas Comuna con MP2.5 -------------
df_tabla <- df_modelo %>% group_by(region, nombre_comuna) %>% 
  filter(!is.na(mp25)) %>% 
  summarise(mp25=round(mp25,1)) %>% 
  rename(Region=region, Comuna=nombre_comuna, `MP2.5 [ug/m3]`=mp25) 

## Genero tres tablas
df_tabla %>% head(18) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3, align = "right", part="all") %>% 
  print(preview="pptx")

df_tabla %>% head(36) %>% tail(18) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3, align = "right", part="all") %>% 
  print(preview="pptx")

df_tabla %>% tail(15) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3, align = "right", part="all") %>% 
  print(preview="pptx")

rm(df_tabla)
comunas_mp <- df_modelo %>% select(codigo_comuna,nombre_comuna)

# cat('sep=; \n',file = "Data/Data_Modelo/Comunas_MP.csv")
# write.table(comunas_mp,"Data/Data_Modelo/Comunas_MP.csv",
#             sep=';',row.names = F, append = T)
# rm(comunas_mp)

## EoF