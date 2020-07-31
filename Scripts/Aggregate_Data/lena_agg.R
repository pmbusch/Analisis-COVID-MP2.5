### Analisis-COVID-MP2.5
## Agrego datos de leña a nivel de comuna
## PBH Julio 2020

# Carga datos brutos --------
source("Scripts/Load_Data/lena_load.R", encoding = "UTF-8")


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


df_poblacionRegion <- df_poblacionZona %>% group_by(codigo_region) %>% 
  summarise(pob=sum(poblacion, na.rm=T)) %>% ungroup()
  
df_poblacionZona <- df_poblacionZona %>% group_by(zona, codigo_comuna) %>%
  summarise(pob=sum(poblacion,na.rm=T)) %>%
  mutate(porc_pob=pob/sum(pob)) %>%
  ungroup()


df_lena <- left_join(df_poblacionZona, df_lena, by=c("zona")) %>%
  mutate(consumo_lena_m3=consumo_lena_m3*porc_pob,
         penetracion_lena=porcentaje_viviendas_lena*100,
         consumo_lena_pp=consumo_lena_m3/pob) %>%
  select(codigo_comuna, consumo_lena_m3, penetracion_lena, consumo_lena_pp)

## Check: Total debe ser 11,962,058
df_lena$consumo_lena_m3 %>% sum()

# Limpio WS
rm(df_poblacionZona, df_poblacionRegion)

## Energia lena CDT ------------
# Zonas termicas a region
df_zonificacion <- read_excel("Data/Data_Original/Zonificacion_Termica.xlsx") %>% 
  mutate(zt=as.numeric(zona_principal),
         codigo_comuna=paste(if_else(str_length(codigo_comuna)==4,"0",""),
                             codigo_comuna, sep=""))
# Agrego datos de poblacion por zona
df_zona <- df_zonificacion %>% 
  left_join(censo_2017_comunas, by=c("codigo_comuna")) %>% 
  group_by(zt) %>% 
  summarise(poblacion=sum(poblacion,na.rm=T)) %>% ungroup()

## Obtengo datos de interes
# Por el momento: Leña, y Calefactores, Cocina_Horno
df_energia_lena$uso %>% unique()
df_energia_lena$energetico %>% unique()
df <- df_energia_lena %>% 
  filter(uso %in% c("calefactores","cocina + horno")&energetico %in% c("leña")) %>% 
  mutate(uso=uso %>% str_remove_all(" |\\+"),
         tipo=paste(uso,energetico,sep="_")) %>% 
  group_by(zt, tipo) %>% 
  summarise(consumo_kWh=sum(valor,na.rm=T)) %>% ungroup()

df <- left_join(df, df_zona) %>% 
  mutate(cons_energia_pp=consumo_kWh/poblacion)

df %>% group_by(tipo) %>% summarise(count=sum(poblacion,na.m=T))

# Expando
df <- df %>% select(zt, tipo, cons_energia_pp) %>% 
  spread(tipo, cons_energia_pp) %>% 
  rename(cons_lena_calefactor_pp=calefactores_leña,
         cons_lena_cocina_pp=cocinahorno_leña)

df_zonificacion <- df_zonificacion %>% select(zt, codigo_comuna) %>% 
  left_join(df)

# Junto a df lena
df_lena <- left_join(df_lena, df_zonificacion %>% select(-zt))

rm(df_energia_lena, df, df_zonificacion, df_zona)
## EoF