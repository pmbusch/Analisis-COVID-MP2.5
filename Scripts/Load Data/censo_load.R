### Analisis-COVID-MP2.5
## Carga datos del Censo 2017
## PBH Julio 2020

## Lectura CENSO -------------
df_censo <- read_delim("Data/Data Modelo/Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Manzanas.csv",
                       delim = ";", na = c("NA","*"),
                       col_types = "cccccccccddddddddddddddddddddddddddddddddddddddddddddddcccc",
                       locale = locale(encoding = "windows-1252"))
df_censo %>% names()

## Para homologar codigos comunales, debo agregar un 0 a las regiones (ej: 01)
df_censo <- df_censo %>% 
  mutate(COMUNA=paste(if_else(str_length(REGION)==1,"0",""),
                      COMUNA,sep=""))

## Nota: Poblacion por edad la obtengo de Chilemapas


## URBANO-RURAL -------------
df_rural <- df_censo %>% 
  group_by(COMUNA, AREA) %>% 
  summarise(poblacion=sum(PERSONAS,na.rm=T)) %>% 
  mutate(perc_rural=poblacion/sum(poblacion)) %>%
  filter(AREA=="2") %>% # 2: Area rural
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("COMUNA"="codigo_comuna")) %>% 
  rename(codigo_comuna=COMUNA)


## Porcentaje Pueblo Originario ---------
# PUEBLO: Total de personas que se consideran pertenecientes a un pueblo indígena u originario
df_puebloOrig <- df_censo %>% 
  group_by(COMUNA) %>% 
  summarise(poblacion=sum(PERSONAS,na.rm=T),
            pueblo=sum(PUEBLO,na.rm=T),
            perc_puebloOrig=pueblo/poblacion) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("COMUNA"="codigo_comuna")) %>% 
  rename(codigo_comuna=COMUNA)

## Viviendas ------------
# Vivienda: corresponden a los lugares de alojamiento, estructuralmente separados e independientes, en los que pueden residir las personas. Estas pueden ser viviendas particulares o viviendas colectivas.

df_viviendas <- df_censo %>% 
  group_by(COMUNA) %>% 
  summarise(viviendas=sum(TOTAL_VIV,na.rm=T)) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("COMUNA"="codigo_comuna")) %>% 
  rename(codigo_comuna=COMUNA)
df_viviendas$viviendas %>% sum()


## Tipo Material Viviendas -----------
# MATIRREC: total de viviendas con materialidad irrecuperable

## Nota MATACEP+MATAREC+MATARREC != TOTAL_VIV por alguna razon
df_material <- df_censo %>% 
  group_by(COMUNA) %>% 
  summarise(viv=sum(TOTAL_VIV,na.rm=T),
            mat=sum(MATIRREC,na.rm=T),
            perc_material_irrecuperable=mat/viv) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("COMUNA"="codigo_comuna")) %>% 
  rename(codigo_comuna=COMUNA)
df_material$perc_material_irrecuperable %>% range()


# P03A: Material de los muros exteriores
# P03B: Material en la cubierta del techo
# P03C: Material de construcción del piso
## NO UTILIZADO
# df_codigoMaterial <- read_excel("Data/Data Modelo/Censo2017_16R_ManzanaEntidad_CSV/Codigo_Material.xlsx")
# df_material <- df_censo %>% 
#   select(COMUNA,P03A_1, P03A_2, P03A_3, P03A_4, P03A_5, P03A_6, P03B_1, 
#          P03B_2, P03B_3, P03B_4, P03B_5, P03B_6, P03B_7, P03C_1, P03C_2, 
#          P03C_3, P03C_4, P03C_5) %>% 
#   gather(Codigo,p03, -COMUNA) %>% 
#   left_join(df_codigoMaterial, by=c("Codigo")) %>% 
#   group_by(COMUNA,material) %>% 
#   summarise(p03=sum(p03,na.rm=T)) %>%
#   ungroup() %>% 
#   left_join(codigos_territoriales, by = c("COMUNA"="codigo_comuna")) %>% 
#   rename(codigo_comuna=COMUNA)

## EoF