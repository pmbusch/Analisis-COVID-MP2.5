### Analisis-COVID-MP2.5
## Carga Poblacion, codigos comunales y mapas con superficies
## PBH Julio 2020


## Carga Poblacion --------------
# Usamos libreria de Chilemapas
df_poblacion <- left_join(censo_2017_comunas, codigos_territoriales)
df_poblacion %>% names()
df_poblacion$poblacion %>% sum()

## MAPAS ---------
# Saco del mapa a Isla de Pascua y Juan Fernandez
mapa_comuna <- mapa_comunas %>% 
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
# # En proceso 
# df_pob <- censo_2017_zonas %>%
#   left_join(mapa_zonas) %>%
#   left_join(codigos_territoriales) %>% 
#   mutate(superficie=st_area(geometry) %>% as.numeric(),
#          perimetro=st_length(geometry) %>% as.numeric())
# 
# ## Mucha Poblacion sin NA
# df_pob %>% group_by(nombre_region) %>%
#   summarise(count=sum(poblacion)) %>% arrange(desc(count))
# 
# # Superficie de manzana censal por comuna
# df_pob <- df_pob %>% group_by(codigo_comuna) %>% 
#   summarise(superficie_censal=sum(superficie, na.rm=T)) %>% ungroup() %>% 
#   left_join(mapa_comuna) %>% 
#   mutate(perc_censal=superficie_censal/superficie*100)


# mapa_zonas %>% 
#   filter(codigo_region=="13") %>% 
#   ggplot()+
#   geom_sf(aes(geometry=geometry))
# 
# chilemapas::divisiones_electorales %>% view()
# divisiones_salud %>% view()

## EoF