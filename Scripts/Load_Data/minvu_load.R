### Analisis-COVID-MP2.5
## Carga Datos MINVU
## https://www.observatoriourbano.cl/estadisticas-habitacionales/
## Demanda y Deficit Habitacional. Índice de hacinamiento (viviendas, hogares, personas), por comuna y zona (Censo 2017)
## Permisos de Edificacion. Viviendas unidades y superficie según año y comuna
## PBH Septiembre 2020



## HACINAMIENTO ---------------
## Carga Datos --------------
## Categorias:
# "Viviendas sin Hacinamiento (menos de 2,5 personas por dormitorio)"	
# "Viviendas con Hacinamiento Medio (entre 2,5 y menos de 5 personas por dormitorio)"	
# "Viviendas con Hacinamiento Crítico (más de 5 personas por dormitorio o sin dormitorio)"	
# "Viviendas donde no se reporta cantidad de dormitorios (hacinamiento ignorado)"	

df_minvu <- read_excel("Data/Data_Original/Censo2017_16R_ManzanaEntidad_CSV/DD Hacinamiento Comuna Urbano y Rural CENSO 2017.xlsx",
                       sheet="Vivienda Hacinamiento",
                       range = "A26:R371",
                       col_names = c("region","comuna","codigo_comuna",
                                     "viv_urb","viv_rural","viv_total",
                                     "viv_sinHac_urb","viv_sinHac_rural","viv_sinHac_total",
                                     "viv_HacMedio_urb","viv_HacMedio_rural","viv_HacMedio_total",
                                     "viv_HacCritico_urb","viv_HacCritico_rural","viv_HacCritico_total",
                                     "viv_sinDorm_urb","viv_sinDorm_rural","viv_sinDorm_total"))


## Para df_minvu codigos comunales, debo agregar un 0 a las regiones (ej: 01)
df_minvu <- df_minvu %>% 
  mutate(codigo_comuna=paste(if_else(str_length(codigo_comuna)==4,"0",""),
                      codigo_comuna,sep=""))


## PERMISOS EDIFICACION ----------------
## Carga Datos --------------
source("Scripts/00-Funciones.R", encoding = "UTF-8")
df_permisos <- read_excel("Data/Data_Original/Censo2017_16R_ManzanaEntidad_CSV/PE_Viviendas.xlsx",
                          sheet="número_total",
                          range = "A27:T393",
                          col_names = c("region","comuna",2002:2019))

# Ajusto cruce por comuna
# Hay comunas que se repite por la serie temporal (cmabio de region)
# Al sumar por la totalidad de años no hay problema
df_permisos <- df_permisos %>% 
  mutate(comuna=f_remover_acentos(comuna) %>% 
           str_replace("Padre Las Casas","Padre las Casas") %>% 
           str_replace("Coyhaique","Coihaique") %>% 
           str_replace("Aysen","Aisen") %>% 
           str_replace("Ollagüe","Ollague") %>% 
           str_replace("O’Higgins","OHiggins") %>% 
           str_replace("Cabo de Hornos \\(Ex - Navarino\\)","Cabo de Hornos")) %>% 
  rename(nombre_comuna=comuna) %>% 
  left_join(codigos_territoriales, by=c("nombre_comuna")) 


## Aplano tabla
df_permisos <- df_permisos %>% 
  select(-codigo_region,-nombre_region,-codigo_provincia,-nombre_provincia,
         -nombre_comuna,-region) %>% 
  pivot_longer(-codigo_comuna,
               names_to="year", values_to="permisos")


## EoF