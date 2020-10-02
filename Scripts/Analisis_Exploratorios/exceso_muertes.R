### Analisis-COVID-MP2.5
## Exceso de MUertes: Serie de tiempo
## PBH Septiembre 2020

theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Scripts/Analisis_Exploratorios/Figuras/COVID/%s.png"

# Load Data --------
source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
source("Scripts/00-Funciones.R", encoding = "UTF-8")

# Deis
# lectura
fecha_deis <- "24-09-2020"
df_deis <- read_delim(paste(
  "Data/Data_Original/DEIS/DEFUNCIONES_FUENTE_DEIS_2016_2020_",
  fecha_deis %>% str_remove_all("-"),".csv",sep=""),
  delim = ";",col_names = T,
  col_types = "Dcddccccccccccccccccccccc",
  locale = locale(encoding = "windows-1252",
                  date_format = "%d-%m-%Y"))
spec(df_deis)
names(df_deis) <- c("date","sexo","edad_tipo","edad",
                    "codigo_comuna","comuna","region",
                    "diag1","cap_diag1","glosa_cap_diag1",
                    "grupo_diag1","glosa_grupo_diag1",
                    "categ_diag1","glosa_categ_diag1",
                    "subcateg_diag1","glosa_subcateg_diag1",
                    "diag2","cap_diag2","glosa_cap_diag2",
                    "grupo_diag2","glosa_grupo_diag2",
                    "categ_diag2","glosa_categ_diag2",
                    "subcateg_diag2","glosa_subcateg_diag2")

## Add year: antes estaba y lo borarron, como cambian el formato semana a semana....
df_deis <- df_deis %>% mutate(year=year(date))

# Factores
df_deis$year %>% unique()
df_deis <- df_deis %>% mutate(sexo=factor(sexo),
                              year=factor(year),
                              glosa_subcateg_diag1=factor(glosa_subcateg_diag1))
df_deis %>% group_by(year) %>% summarise(count=n())


## Cumulate over Date -------
df_deis %>% names()
df_deis_tiempo <- df_deis %>%
  group_by(year, date) %>%
  summarise(muertes=n())
df_deis_tiempo$muertes %>% sum()

df_deis_tiempo <- df_deis_tiempo %>%
  arrange(year, date) %>%
  mutate(muertes_acc=cumsum(muertes)) %>% ungroup()

## Plot cumulate deaths over time year --------
df_deis_tiempo %>% 
  mutate(fecha=paste(day(date),month(date),2000,sep="-") %>% 
           strptime("%d-%m-%Y") %>% as_date()) %>% 
  ggplot(aes(fecha, muertes_acc, col=year, group=year))+
  geom_line(size=1)+
  labs(x="", y="Muertes acumuladas en Chile", col="Año")+
  scale_color_viridis_d()+
  scale_x_date(labels = scales::date_format("%b"))+
  guides(colour = guide_legend(reverse=T))+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))
f_savePlot(last_plot(), sprintf(file_name,"ExcesoMuertes"))

## Por Mes
df_deis_tiempo %>% 
  mutate(month=month(date)) %>% 
  group_by(year,month) %>% 
  summarise(muertes_acc=max(muertes_acc, na.rm=T)) %>% ungroup() %>% 
  mutate(date=paste("1",month,year,sep="-") %>% strptime("%d-%m-%Y") %>% as_date()) %>% 
  ggplot(aes(month(date, label=TRUE, abbr=TRUE), muertes_acc, col=year, group=year))+
  geom_point()+
  geom_line(size=1)+
  labs(x="", y="Muertes acumuladas en Chile", col="Año")+
  scale_color_viridis_d()+
  guides(colour = guide_legend(reverse=T))+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))
f_savePlot(last_plot(), sprintf(file_name,"ExcesoMuertes_Mes"))

## Comparacion mes sin acumular ---------
# Solicitador por Guille
df_deis_tiempo %>% 
  filter(year %in% c(2019,2020)) %>% 
  mutate(month=month(date)) %>% 
  group_by(year,month) %>% 
  summarise(muertes=sum(muertes, na.rm=T)) %>% ungroup() %>% 
  mutate(date=paste("1",month,year,sep="-") %>% strptime("%d-%m-%Y") %>% as_date()) %>% 
  ggplot(aes(month(date, label=TRUE, abbr=TRUE), muertes, fill=year))+
  geom_col(stat="identity", position = "dodge")+
  labs(x="", y="Muertes en Chile", col="Año")+
  # scale_fill_viridis_d()+
  guides(fill = guide_legend(reverse=T))+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))
f_savePlot(last_plot(), sprintf(file_name,"ExcesoMuertes_Mes_sinAcc"))

## Comparacion mes delta sin acumular ---------
# Solicitador por Guille
df_deis_tiempo %>% 
  mutate(month=month(date)) %>% 
  filter(year %in% c(2019,2020) & month<9) %>% 
  group_by(year,month) %>% 
  summarise(muertes=sum(muertes, na.rm=T)) %>% ungroup() %>% 
  spread(year, muertes) %>% 
  mutate(dif=`2020`-`2019`) %>% 
  mutate(date=paste("1",month,"2020",sep="-") %>% strptime("%d-%m-%Y") %>% as_date()) %>% 
  ggplot(aes(month(date, label=TRUE, abbr=TRUE), dif))+
  geom_col(fill="brown")+
  geom_text(aes(label=dif), size=5, vjust=-0.8)+
  labs(x="", y="Diferencia muertes en Chile 2020-2019", col="Año")+
  # scale_fill_viridis_d()+
  guides(fill = guide_legend(reverse=T))+
  coord_cartesian(ylim=c(0,6500), expand=F)+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))
f_savePlot(last_plot(), sprintf(file_name,"ExcesoMuertes_Mes_sinAcc_delta"))


## Comparacion mes delta sin acumular porcentual ---------
df_deis_tiempo %>% 
  mutate(month=month(date)) %>% 
  filter(year %in% c(2019,2020) & month<9) %>% 
  group_by(year,month) %>% 
  summarise(muertes=sum(muertes, na.rm=T)) %>% ungroup() %>% 
  spread(year, muertes) %>% 
  mutate(dif=((`2020`-`2019`)/`2019`*100) %>% round(2)) %>% 
  mutate(date=paste("1",month,"2020",sep="-") %>% strptime("%d-%m-%Y") %>% as_date()) %>% 
  ggplot(aes(month(date, label=TRUE, abbr=TRUE), dif))+
  geom_col(fill="brown")+
  geom_text(aes(label=dif), size=5, vjust=-0.8)+
  labs(x="", y="% Aumento porcentual muertes \n por mes Chile 2020-2019", col="Año")+
  # scale_fill_viridis_d()+
  guides(fill = guide_legend(reverse=T))+
  coord_cartesian(ylim=c(0,60), expand=F)+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))
f_savePlot(last_plot(), sprintf(file_name,"ExcesoMuertes_Mes_sinAcc_delta_perc"))


## EoF