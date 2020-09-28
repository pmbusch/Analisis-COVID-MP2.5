### Analisis-COVID-MP2.5
## Tablas Resumen Info
## PBH Julio 2020
source("Scripts/00-Funciones.R", encoding = "UTF-8")


## Indicadores relevantes ------------
# Poblacion en comunas con datos MP2.5
df_modelo %>% filter(!is.na(mp25)) %>% nrow()
total_pob <- df_modelo$poblacion %>% sum()
pob_mp25 <- df_modelo %>% filter(!is.na(mp25)) %>% pull(poblacion) %>% sum()
cat(round(pob_mp25/total_pob*100,1),
    "% de poblacion en comunas con monitoreo de MP2.5")
# Poblacion en comunas con muertes ---------
total_comunas <- nrow(df_modelo)
comunas_muerte <- df_modelo %>% filter(covid_fallecidos>0) %>% nrow()
cat(round(comunas_muerte/346*100,1),
    "% de comunas con muertes COVID")
rm(total_pob, pob_mp25, total_comunas, comunas_muerte)


# Tablas Resumen general Promedio (sd) ---------------
df_modelo %>% names()
df <- df_modelo %>% 
  mutate(perc_fonasa=perc_fonasa_A+perc_fonasa_B+perc_fonasa_C+perc_fonasa_D,
         poblacion=poblacion/1e3,
         ingresoAutonomo_media=ingresoAutonomo_media/1e3) %>% 
  select(tasa_mortalidad_covid, mp25, 
         poblacion,densidad_pob_censal, `15-44`, `45-64`, `65+`,perc_mujer, 
         perc_rural, perc_puebloOrig, perc_material_irrecuperable,perc_vivHacMedio,
         tasa_contagios, perc_letalidad, cfr_raw_0, cfr_0_20,
         dias_primerContagio, dias_primerMuerte, dias_cuarentena, 
         tasa_camas,
         ingresoAutonomo_media, perc_isapre, perc_fonasa_A,perc_fonasa_B,
         perc_fonasa_C, perc_fonasa_D,perc_menor_media, perc_ocupado, perc_salud,
         cons_lena_kg,perc_lenaCocina,perc_lenaCalefaccion,perc_lenaAgua,
         hr_summer, hr_winter, tmed_summer, tmed_winter,
         heating_degree_15_summer, heating_degree_15_winter)

# separo por la condicion de si tiene o no estacion
df_sep <- df %>% 
  mutate(tiene_estacion=if_else(!is.na(mp25),"si","no")) %>% 
  group_by(tiene_estacion) %>% skim() %>% 
  mutate(indicador=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep="")) %>% 
  select(skim_variable, tiene_estacion, indicador) %>% 
  spread(tiene_estacion, indicador)
  
df_skim <- df %>% skim() %>% 
  mutate(n=complete_rate*nrow(df),
         indicador=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep="")) %>% 
  select(skim_variable,n, indicador) %>% 
  left_join(df_sep)

n_mp25 <- df_modelo %>% filter(!is.na(mp25)) %>% nrow()
foot_note <- paste("n:",c(nrow(df_modelo),nrow(df_modelo)-n_mp25,
                          n_mp25),"comunas",sep=" ")

# Cambio nombre variables
df_skim <- df_skim %>% 
  mutate(Tipo=f_addTypeVar(skim_variable),
         skim_variable=f_replaceVar(skim_variable) %>% 
          str_replace("Poblacion","Poblacion [miles]") %>% 
           str_replace("Media Ingreso autonomo mensual",
                       "Media Ingreso autonomo mensual [miles CLP]")) %>% 
  arrange(Tipo)
df_skim <- df_skim[,c(6,1,2,3,4,5)] # Reorder columns

df_skim %>% 
  rename(Variable=skim_variable, Total=indicador, 
         `Sin MP2.5`=no, `Con MP2.5`=si) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% bold(j=1:2, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3, align = "center", part="all") %>% 
  merge_v(j = 1) %>%   fix_border_issues(part = "all") %>% 
  flextable::border(j=1, part="body",
         border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  flextable::border(j=2:6, part="body",i=c(8,9,13,23,33),
         border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  footnote(j=4:6, value=as_paragraph(foot_note), part="header", inline=T) 
  # print(preview="pptx")
# print(preview="docx")

rm(foot_note,df_skim, df_sep, n_mp25)

## Tablas Rango -------------
# Incluyo coeficiente de variacion: ayuda a ver que variables tienen mayor variabilidad entre comunas
# Nota: Solo es valido para variables con escala de partida en 0 (ratios). T° no aplica
# https://en.wikipedia.org/wiki/Coefficient_of_variation
df_skim <- df %>% filter(!is.na(mp25)) %>% 
  skim() %>% 
  mutate(cv=numeric.sd/numeric.mean) %>% 
  select(skim_variable, numeric.mean, cv,
         numeric.p0, numeric.p50, numeric.p100) %>% 
  rename(Variable=skim_variable,
         Promedio=numeric.mean,
         `C.V.`=cv,
         Min=numeric.p0, Mediana=numeric.p50, Max=numeric.p100)

# Cambio nombre variables
df_skim <- df_skim %>% 
  mutate(Tipo=f_addTypeVar(Variable),
         Variable=f_replaceVar(Variable) %>% 
           str_replace("Poblacion","Poblacion [miles]") %>% 
           str_replace("Media Ingreso autonomo mensual",
                       "Media Ingreso autonomo mensual [miles CLP]")) %>% 
  arrange(Tipo)
df_skim <- df_skim[,c(7,1,2,3,4,5,6)] # Reorder columns


df_skim %>% 
  flextable() %>% 
  colformat_num(big.mark=" ", digits=1, j=3:ncol(df_skim),
                na_str="s/i") %>%
  bold(bold=T, part="header") %>% bold(j=1:2, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  merge_v(j = 1) %>%   fix_border_issues(part = "all") %>% 
  flextable::border(j=1, part="body",
         border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  flextable::border(j=2:7, part="body",i=c(8,9,13,23,33),
         border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  footnote(j=4, value=as_paragraph("Coeficiente Variación"), 
           part="header", inline=T)
  # print(preview="docx")
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