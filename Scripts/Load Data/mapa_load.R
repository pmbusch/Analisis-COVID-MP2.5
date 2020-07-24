### Analisis-COVID-MP2.5
## Carga Poblacion, codigos comunales y mapas con superficies
## PBH Julio 2020


## Carga Poblacion --------------

# Usamos libreria de Chilemapas
df_poblacion <- left_join(censo_2017_comunas, codigos_territoriales)
df_poblacion %>% names()
df_poblacion$poblacion %>% sum()


## Dividor en grupos etarios -----------
df_poblacion$edad %>% unique()
df_poblacion <- df_poblacion %>% 
  mutate(grupo_edad=case_when(
    edad %in% c("0 a 4","5 a 9","10 a 14") ~ "0-14",
    edad %in% c("15 a 19","20 a 24","25 a 29",
                "30 a 34","35 a 39","40 a 44") ~ "15-44",
    edad %in% c("45 a 49","50 a 54","55 a 59","60 a 64") ~ "45-64",
    T ~ "65+"))

df_edad <- df_poblacion %>% 
  group_by(codigo_comuna, grupo_edad) %>% 
  summarise(pob=sum(poblacion,na.rm=T)) %>% 
  mutate(porc_edad=pob/sum(pob),
         pob=NULL) %>% ungroup() %>% 
  filter(grupo_edad!="0-14") %>% spread(grupo_edad,porc_edad)

## Dividir por sexo
df_sexo <- df_poblacion %>% 
  group_by(codigo_comuna, sexo) %>% 
  summarise(pob=sum(poblacion,na.rm=T)) %>% 
  mutate(porc=pob/sum(pob),
         pob=NULL) %>% ungroup() %>% 
  filter(sexo!="hombre") %>% spread(sexo,porc)


## Poblacion por comuna ----------
df_poblacion <- df_poblacion %>% 
  group_by(codigo_comuna) %>% 
  summarise(poblacion=sum(poblacion,na.rm=T))


df_poblacion <- left_join(df_poblacion, df_edad) %>% 
  left_join(df_sexo) %>% 
  left_join(codigos_territoriales)

rm(df_edad,df_sexo)


## MAPAS ---------
# Saco del mapa a Isla de Pascua y Juan Fernandez
mapa_comuna <- mapa_comunas %>% 
  filter(!(codigo_comuna %in% c("05201","05104"))) %>% 
  mutate(superficie=st_area(geometry),
         perimetro=st_length(geometry))


# Levels regiones
levels_region <- c("XV","I","II","III","IV","V","M","VI","VII","VIII",
                   "IX","XIV","X","XI","XII")

mapa_regiones <- generar_regiones(mapa_comunas %>% filter(codigo_comuna!="05201" &
                                                       codigo_comuna!="05104"))
mapa_regiones$codigo_region %>% unique()
mapa_regiones <- mapa_regiones %>% mutate(
  region=case_when(
    codigo_region=="01" ~ "I",
    codigo_region=="02" ~ "II",
    codigo_region=="03" ~ "III",
    codigo_region=="04" ~ "IV",
    codigo_region=="05" ~ "V",
    codigo_region=="06" ~ "VI",
    codigo_region=="07" ~ "VII",
    codigo_region=="08" ~ "VIII",
    codigo_region=="09" ~ "IX",
    codigo_region=="10" ~ "X",
    codigo_region=="11" ~ "XI",
    codigo_region=="12" ~ "XII",
    codigo_region=="13" ~ "XIII",
    codigo_region=="14" ~ "XIV",
    codigo_region=="15" ~ "XV",
    codigo_region=="16" ~ "VIII",
    T ~ "otro") %>% factor(levels = levels_region),
  superficie=st_area(geometry),
  perimetro=st_length(geometry))

## EoF