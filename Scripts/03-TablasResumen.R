### Analisis-COVID-MP2.5
## Tablas Resumen Info
## PBH Julio 2020


# Tablas Resumen general Promedio (sd) ---------------

df_comuna %>% names()

df <- df_comuna %>% 
  select(tasa_mortalidad, mp25, poblacion, `15-44`, `45-64`, `65+`,perc_mujer, 
         densidad_pob, perc_rural, perc_material_irrecuperable, 
         tasa_contagios, perc_letalidad,
         dias_primerContagio, dias_cuarentena, tasa_camas,
         ingresoAutonomo_media, ingresoAutonomo_mediana, perc_isapre, 
         perc_menor_media, perc_ocupado, penetracion_lena, tasa_mortalidad_all) %>% 
  rename(
    `Tasa Mortalidad COVID [por 100mil]`=tasa_mortalidad,
    `MP2.5 [ug/m3]`=mp25,
    Poblacion=poblacion,
    `% 15-44`=`15-44`,
    `% 45-64`=`45-64`,
    `% 65+`=`65+`,
    `% Mujer`=perc_mujer,
    `Densidad [hab/km2]`=densidad_pob,
    `% Rural`=perc_rural,
    `% Vivienda con Material irrecuperable`=perc_material_irrecuperable,
    `Tasa Contagios COVID [por 100mil]`=tasa_contagios,
    `% Letalidad COVID`=perc_letalidad,
    `Dias desde primer contagio`=dias_primerContagio,
    `Dias desde cuarentena`=dias_cuarentena,
    `Camas hospitalarias [por 100mil]`=tasa_camas,
    `Media Ingreso autonomo [CLP]`=ingresoAutonomo_media,
    `Mediana Ingreso autonomo [CLP]`=ingresoAutonomo_mediana,
    `% Isapre`=perc_isapre,
    `% Educación menor a media`=perc_menor_media,
    `% Ocupado laboral`=perc_ocupado,
    `% Penetracion leña`=penetracion_lena,
    `Tasa Mortalidad total [por 100mil]`=tasa_mortalidad_all
  )

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

foot_note <- paste("n:",c(nrow(df_comuna),nrow(df_comuna)-nrow(df_modelo),
                          nrow(df_modelo)),"comunas",sep=" ")

df_skim %>% 
  rename(Variable=skim_variable, Total=indicador, 
         `Sin MP2.5`=no, `Con MP2.5`=si) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "left", part="all") %>% 
  align(j=2, align = "center", part="all") %>% 
  footnote(j=2:4, value=as_paragraph(foot_note), part="header", inline=T) %>% 
  print(preview="pptx")

rm(foot_note,df_skim, df_sep)

## Tablas Rango -------------
df_skim <- df %>% filter(!is.na(`MP2.5 [ug/m3]`)) %>% skim() %>% 
  select(skim_variable, numeric.mean, numeric.sd,
         numeric.p0, numeric.p50, numeric.p100) %>% 
  rename(Variable=skim_variable,
         Promedio=numeric.mean,Desv=numeric.sd,
         Min=numeric.p0, Mediana=numeric.p50, Max=numeric.p100)

df_skim %>% 
  flextable() %>% 
  colformat_num(big.mark=" ", digits=1, j=2:ncol(df_skim),
                na_str="s/i") %>%
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "left", part="all") %>% 
  print(preview="pptx")

rm(df_skim, df)
  

## Tablas Comuna con MP2.5 -------------
df_tabla <- df_modelo %>% group_by(region, nombre_comuna) %>% 
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

df_tabla %>% tail(17) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3, align = "right", part="all") %>% 
  print(preview="pptx")

rm(df_tabla)
comunas_mp <- df_modelo %>% select(codigo_comuna,nombre_comuna)

cat('sep=; \n',file = "Data/Data_Modelo/Comunas_MP.csv")
write.table(comunas_mp,"Data/Data_Modelo/Comunas_MP.csv",
            sep=';',row.names = F, append = T)

## EoF