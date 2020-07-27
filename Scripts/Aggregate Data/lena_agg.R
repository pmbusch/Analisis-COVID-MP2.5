### Analisis-COVID-MP2.5
## Agrego datos de leña a nivel de comuna
## PBH Julio 2020

# Carga datos brutos --------
source("Scripts/Load Data/lena_load.R", encoding = "UTF-8")


## Bajamos los datos regiones de consumo de leña al 2018 a nivel comunal mediante la poblacion
## Penetracion de lena es igual a nivel de region (no se tiene mayor detalle)
# Uso datos de poblacion de chilemapas
df_poblacionZona <- left_join(censo_2017_comunas, codigos_territoriales) %>% 
  mutate(zona=case_when(
    codigo_region %in% c("15","01","02","03","04") ~ "ZN",
    codigo_region == "05" ~ "V",
    codigo_region == "13" ~ "RM",
    codigo_region == "06" ~ "VI",
    codigo_region == "07" ~ "VII",
    codigo_region %in% c("08","16") ~ "VIII",
    codigo_region == "09" ~ "IX",
    codigo_region == "14" ~ "XIV",
    codigo_region == "10" ~ "X",
    codigo_region == "11" ~ "XI",
    codigo_region == "12" ~ "XII"))
df_poblacionZona$zona %>% unique()

df_poblacionZona <- df_poblacionZona %>% group_by(zona, codigo_comuna) %>%
  summarise(pob=sum(poblacion,na.rm=T)) %>%
  mutate(porc_pob=pob/sum(pob)) %>%
  ungroup()


df_lena <- left_join(df_poblacionZona, df_lena, by=c("zona")) %>%
  mutate(consumo_lena_m3=consumo_lena_m3*porc_pob) %>%
  rename(penetracion_lena=porcentaje_viviendas_lena) %>% 
  select(codigo_comuna, consumo_lena_m3, penetracion_lena)

## Check: Total debe ser 11,962,058
df_lena$consumo_lena_m3 %>% sum()

# Limpio WS
rm(df_poblacionZona)

## EoF