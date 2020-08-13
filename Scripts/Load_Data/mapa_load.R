### Analisis-COVID-MP2.5
## Carga Poblacion, codigos comunales y mapas con superficies
## PBH Julio 2020


## Carga Poblacion --------------
# Usamos libreria de Chilemapas
df_poblacion <- left_join(censo_2017_comunas, codigos_territoriales)
df_poblacion %>% names()
df_poblacion$poblacion %>% sum()

## MAPAS ---------
## Comunas --------------
# Saco del mapa a Isla de Pascua y Juan Fernandez
mapa_comuna <- mapa_comunas %>% st_as_sf() %>% 
  filter(!(codigo_comuna %in% c("05201","05104"))) %>% 
  mutate(superficie=st_area(geometry) %>% as.numeric(),
         perimetro=st_length(geometry) %>% as.numeric())

# Levels regiones
levels_region <- c("XV","I","II","III","IV","V","M","VI","VII","VIII",
                   "IX","XIV","X","XI","XII")

mapa_comuna <- mapa_comuna %>% mutate(
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
    codigo_region=="13" ~ "M",
    codigo_region=="14" ~ "XIV",
    codigo_region=="15" ~ "XV",
    codigo_region=="16" ~ "VIII",
    T ~ "otro") %>% factor(levels = levels_region))

## Añado variable boolean para filtar mapa de RM customizado
# Contiene todas las comunas en la Provincia de Santiago, menos Lo Barnechea
# Contiene ademas las comunas de San Bernardo, Puente Alto, Padre Hurtado, calera de tango, 
mapa_comuna <- mapa_comuna %>% 
  mutate(mapa_rm=if_else((codigo_provincia=="131" & codigo_comuna!="13115")|
                           codigo_comuna %in% c("13201","13401","13403","13604"),
                         1,0))

## Regiones ------------
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
    codigo_region=="13" ~ "M",
    codigo_region=="14" ~ "XIV",
    codigo_region=="15" ~ "XV",
    codigo_region=="16" ~ "VIII",
    T ~ "otro") %>% factor(levels = levels_region),
  superficie=st_area(geometry) %>% as.numeric(),
  perimetro=st_length(geometry) %>% as.numeric())


## Superficie censal por comuna ----
## Notas: existen zonas sin mapa, por lo que no tendria la superficie
censo_2017_comunas$poblacion %>% sum()
censo_2017_zonas$poblacion %>% sum()
cat(sum(censo_2017_zonas$poblacion)/sum(censo_2017_comunas$poblacion)*100,
    " % de poblacion identificada en zonas censales")

# Superficie por zonas
df_zona <- mapa_zonas %>%
  left_join(codigos_territoriales) %>%
  mutate(superficie=st_area(geometry) %>% as.numeric(),
         perimetro=st_length(geometry) %>% as.numeric())

# Superficie de manzana censal por comuna
df_zona <- df_zona %>% group_by(codigo_comuna) %>%
  summarise(superficie_censal=sum(superficie, na.rm=T)) %>% ungroup() %>%
  right_join(mapa_comuna) %>%
  mutate(perc_censal=superficie_censal/superficie*100)

# Nota: algunos valores de superficie sobre 100 (ej:100.2), 
# puede deberse a diferencias en la aproximacion

# Add superficie zona censal to mapa comuna
mapa_comuna <- mapa_comuna %>% 
  left_join(df_zona %>% select(codigo_comuna,superficie_censal)) %>% 
  mutate(superficie_censal=if_else(is.na(superficie_censal),
                                   superficie,superficie_censal))

## EoF