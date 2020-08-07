### Analisis-COVID-MP2.5
## Datos Covid. Fuente: https://github.com/MinCiencia/Datos-COVID19
## Producto 51: Movilidad
## PBH Agosto 2020


## Descarga de datos ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

# Nota: Archivos _std vienen aplanados
df_movilidad <- read_csv(paste(url,"producto51","ISCI_std.csv", sep="/"))
names(df_movilidad) <- names(df_movilidad) %>% str_to_lower() %>% str_replace_all(" ","_")
df_movilidad <- df_movilidad %>% na.omit() # limpio NA

## Notas
# Diferencias son respecto a semana 10: 16 de Marzo
# Nota: Diferencias se presentan como rangos!!!

# Codigo comuna compatible con codigos territoriales
df_movilidad <- df_movilidad %>% 
  mutate(codigo_comuna=paste(if_else(str_length(codigo_comuna)==4,"0",""),
                             codigo_comuna,sep="")) %>% 
  select(-region,-codigo_region,-comuna)

# Rangos como factor
levels_dif <- c("[-100%,-61%]","[-60%,-41%,]","[-40%,-21%,]",
                "[-21%,+20%,]","[+21%,+40%,]","[+41%,+60%,]","[+61%,+100%,]") 

df_movilidad <- df_movilidad %>% 
  mutate(dif_salida=factor(dif_salida,levels = levels_dif),
         dif_entrada=factor(dif_entrada,levels = levels_dif))
rm(levels_dif)

# Fecha mas reciente
fecha_mov <- df_movilidad$fecha %>% max()
df <- df_movilidad %>% filter(fecha==fecha_mov)

# N comunas
df$codigo_comuna %>% unique() %>% length()  

## Mapa Santiago ---------

# Entradas
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")

# agrego datos a nivel de comuna, eligo el dato dif_entrada mas repetido en esa comuna
df_mapa <- df %>% group_by(codigo_comuna) %>% 
  slice(which.max(table(dif_entrada)) )

df_mapa <- df_mapa %>% 
  right_join(mapa_comuna) %>% 
  filter(mapa_rm==1)

df_mapa %>% 
  ggplot()+
  geom_sf(aes(geometry=geometry, fill=dif_entrada))+
  scale_fill_viridis_d()+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 14)



## EoF