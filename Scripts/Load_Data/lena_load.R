### Analisis-COVID-MP2.5
## Carga de datos de consumo de leña
## PBH Julio 2020

# Fuente datos: ----------
# CDT 2015. MEDICIÓN DEL CONSUMO NACIONAL DE LEÑA Y OTROS COMBUSTIBLES SÓLIDOS DERIVADOS DE LA MADERA”. Tabla 136
df_lena <- read_excel("Data/Data_Original/Lena/ConsumoLena_CDT.xlsx", sheet="Consumo")


## Fuente alternativa datos ----------
# CDT, In-Data SpA. (2019). Usos de energía de los Hogares en Chile 2018. Santiago.
# Bases de datos disponible en: https://www.energia.gob.cl/documentos/bbdd-estudio-caracterizacion-residencial-2018
df_energia_lena <- read_excel("Data/Data_Original/Lena/bbdd_estudio_caracterizacion_residencial_2018_0.xlsx",
                              sheet="BBDD Sindicado") %>% 
  .[,498:551]


## Demanda Leña Urbana ------------
# Fuente: Ministerio de Energía - trabajo sobre transición energética
df_lena_urbana <- read_excel("Data/Data_Original/Lena/DemandaLenaUrbana.xlsx", 
                             sheet="Lena_Urbana")
df_lena_urbana$demanda_urbana_lena_2018_MWh %>% sum()

# Fix Codigo Comuna and add dummy PDF
df_lena_urbana <- df_lena_urbana %>% 
  select(codigo_comuna, demanda_urbana_lena_2018_MWh) %>% 
  mutate(codigo_comuna=as.character(codigo_comuna),
         codigo_comuna=paste(if_else(str_length(codigo_comuna)==4,"0",""),
                             codigo_comuna,sep="")) %>% 
  mutate(pda=1) %>% 
  rename(cons_lena_urbana=demanda_urbana_lena_2018_MWh)


## Manejo datos
# Borro col 4
df_energia_lena <- df_energia_lena[,-4]

# names
names(df_energia_lena) <- df_energia_lena %>% names() %>% str_to_lower()

df_energia_lena <- df_energia_lena %>% gather(uso, valor, -zt, -fe, -nse) %>% 
  filter(valor>0)

## Separo en usos y energetico
parametros <- df_energia_lena$uso %>% str_split(' (?=[^ ]+$)', n = 2, simplify = T)
df_energia_lena <- df_energia_lena %>% mutate(uso=parametros[,1],
                    energetico=parametros[,2])
rm(parametros)

## Chequeos:Valor esta en kWh. 
# Total GWh 50763 (Tabla 175)
df_energia_lena$valor %>% sum()/1e6
# Uso total leña: 20085 GWh
df_energia_lena %>% filter(energetico=="leña") %>% pull(valor) %>% sum()/1e6
# Uso total calefactores: 25444 GWh
df_energia_lena %>% filter(uso=="calefactores") %>% pull(valor) %>% sum()/1e6
# Uso total ZT-3: 17052 GWh
df_energia_lena %>% filter(zt==3) %>% pull(valor) %>% sum()/1e6

## EoF