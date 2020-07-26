### Analisis-COVID-MP2.5
## Numero de Camas Hospitalarias por Comuna
# Fuente: https://deis.minsal.cl/#datosabiertos Listado de Establecimientos de Salud
## PBH Julio 2020


## Camas Publicas -------------
df_camas_pub <- read_excel("Data/Data Modelo/Establecimientos_ChileDEIS_MINSAL(10-07-2020).xlsx",
                       sheet = "Dotación de Camas Públicas")

# linea 2 tiene los colnames
names(df_camas_pub) <- df_camas_pub[2,] %>% str_to_lower() %>% str_replace_all(" ","_") %>% 
  str_replace_all("á","a") %>% str_replace_all("ó","o")

# borro 3 primeras lineas
df_camas_pub <- df_camas_pub[-1:-3,]

# borro info de Area funcional de la cama
df_camas_pub <- df_camas_pub %>% filter(!is.na(codigo_establecimiento)) %>% 
  mutate(total=as.numeric(total),
         tipo_est="Publico")

# total camas publico 26,382
df_camas_pub$total %>% sum()

## Camas Privadas -------------
df_camas_priv <- read_excel("Data/Data Modelo/Establecimientos_ChileDEIS_MINSAL(10-07-2020).xlsx",
                           sheet = "Dotación de Camas Privadas")

# linea 2 tiene los colnames
names(df_camas_priv) <- df_camas_priv[2,] %>% str_to_lower() %>% str_replace_all(" ","_") %>% 
  str_replace_all("á","a") %>% str_replace_all("ó","o")

# borro 3 primeras lineas
df_camas_priv <- df_camas_priv[-1:-3,]

# borro info de Area funcional de la cama
df_camas_priv <- df_camas_priv %>% filter(!is.na(codigo_establecimiento)) %>% 
  mutate(total=as.numeric(total_2019),
         total_2019=NULL,
         tipo_est="Privado")

# total camas privadas 11,916
df_camas_priv$total %>% sum()

## Codigos Establecimientos -----------
# Para determinar comuna del establecimiento
df_establecimiento <- read_excel("Data/Data Modelo/Establecimientos_ChileDEIS_MINSAL(10-07-2020).xlsx",
                            sheet = "Establecimientos Vigentes")

# linea 1 tiene los colnames
names(df_establecimiento) <- df_establecimiento[1,] %>% str_to_lower() %>% str_replace_all(" ","_") %>% 
  str_replace_all("á","a") %>% str_replace_all("ó","o")
# borro primera linea
df_establecimiento <- df_establecimiento[-1,]

# rename variables
df_establecimiento <- df_establecimiento %>% 
  rename(longitud=`longitud_[grados_decimales]`,
         latitud=`latitud______[grados_decimales]`) %>% 
  mutate(longitud=as.numeric(longitud),
         latitud=as.numeric(latitud))


# Junto camas
df_camas <- rbind(
  df_camas_pub %>% select(codigo_establecimiento, nombre_establecimiento,
                          total, tipo_est),
  df_camas_priv %>% select(codigo_establecimiento, nombre_establecimiento,
                           total, tipo_est))

df_camas$total %>% sum() # Total camas


## Cruze es por codigo del establecimiento
df_camas <- left_join(df_camas, 
                      df_establecimiento %>% select(codigo_nuevo, codigo_comuna,
                                                    longitud,latitud),
                      by=c("codigo_establecimiento"="codigo_nuevo"))


## Agrupo por comuna ----------
df_camas <- df_camas %>% 
  group_by(codigo_comuna) %>% 
  summarise(camas=sum(total,na.rm=T)) %>% ungroup() %>% 
  left_join(codigos_territoriales)


# Limpio WS
rm(df_camas_priv, df_camas_pub, df_establecimiento)
## EoF