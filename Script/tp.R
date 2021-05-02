library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)
library('Hmisc')

# Primero leemos los archivos a utilizar
departamentos <- st_read("Datos/Codgeo_Pais_x_dpto_con_datos/pxdptodatosok.shp")
smn <- read.csv(file = 'Datos/estaciones_smn.csv', sep = ";")
temperaturas <- read.csv(file = 'Datos/temperaturas.csv')

#Primero miro un poco el de departamentos que utilizamos en clase.
summary(departamentos)
grafico1 <-ggplot() + geom_sf(data =departamentos)
grafico1
summary(departamentos$provincia)

#La Antartida es parte de T. del Fuego
departamentos_tf <- departamentos %>% 
  filter(provincia == "Tierra del Fuego") %>% mutate(personas = personas%>% as.numeric())
grafico2 <- ggplot() + geom_sf(data =departamentos_tf, aes(fill = personas))  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())
grafico2

#Ahora voy a ver la localización de las estaciones
summary(smn)
colnames(smn)
#Cambio este nombre para que se lea mejor
smn <- smn %>% rename(Estacion = NOMBRE) 
summary(smn$Provincia)
describe(smn)

# Vemos cómo se distribuyen las estaciones por provincia.
# La provincia con mayores estaciones es, por lejos, Buenos Aires. La siguen Córdoba y Santa Fe.
#View(smn %>% group_by(Provincia) %>% tally(sort = TRUE))

# Acá tengo que sumarle los minutos a la latitud porque vienen separados

smn$minutos_latitud <- smn$Latitud.min./60
smn$minutos_longitud <- smn$Longitud.min./60
head(smn)
smn$x <- smn$Longitud.gr.-smn$minutos_longitud
smn$y <- smn$Latitud.gr.-smn$minutos_latitud
head(smn)

ggplot() + geom_sf(data = departamentos) + geom_point(data = smn, aes(x=x, y=y),
                                                      colour = "dodgerblue1", size = 1)+ theme_classic()

departamentos_ba <- departamentos %>% filter(provincia == "Buenos Aires")
smn_ba <- smn %>% filter(Provincia == "BUENOS AIRES")

ggplot() + geom_sf(data = departamentos_ba) + 
  geom_point(data = smn_ba, aes(x=x, y=y),colour = "dodgerblue1", size = 1)+ 
  theme_classic() +geom_text(data= smn_ba, aes(x,y, label = Estacion), size = 2, fontface = "bold",vjust = -0.5)+
  ggtitle("Estaciones Meteorológicas", subtitle = "Provincia de Buenos Aires") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())

 # theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                       # size = 0.5), panel.background = element_rect(fill = "aliceblue"))

#Los de los partidos del Conurbano + CABA no se leen muy bien así que hago un mapa sólo con ellos.
departamentos_amba <- departamentos %>% filter(provincia == "Buenos Aires" | provincia =="Ciudad Autónoma de Buenos Aires")
smn_amba <- smn %>% filter(Provincia == "BUENOS AIRES" | Provincia == "CAPITAL FEDERAL")

bbox_new <- st_bbox(departamentos_ba)
bbox_new
bbox_new[1] <- -59
bbox_new[2] <- -35
bbox_new[3] <- -57.8
bbox_new[4] <- -34.4
bbox_new  
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc()

ggplot() + geom_sf(data = departamentos_amba) + 
  coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1],ylim = st_coordinates(bbox_new)[c(2,3),2])+ 
  geom_point(data = smn_amba, aes(x=x, y=y),colour = "dodgerblue1", size = 2)+ theme_classic() +
  geom_text(data= smn_amba, aes(x,y, label = Estacion), size = 3, fontface = "bold", vjust = -0.5)+
  ggtitle("Estaciones Meteorológicas", subtitle = "Area Metropolitana") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())
grafico2
  #theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),
    #    panel.background = element_rect(fill = "aliceblue"))

#Miro el dataset de temperaturas
#ACA ME FALTA MIRAR ESTO BIEN. TIENE NA, FALTA ANALISIS EXPLORATORIO, BOXPLOT DE 
#TEMPERATURAS, HISTOGRAMA, ETC.

head(temperaturas)
temperaturas <- temperaturas %>% rename(Estacion = NOMBRE) 
dim(temperaturas)[1]
colnames(temperaturas)
sort(unique(temperaturas$FECHA))

#Me quedo solo con 2020. Para no tener problemas de estacionalidad, si me quedo con 
#2 meses de enero dado que tengo 2020 y 2021 estaria teniendo temperaturas sesgadas.

for (i in 1:dim(temperaturas)[1]){
  if (nchar(temperaturas$FECHA[i]) == 8){
    temperaturas$YEAR[i] = substr(temperaturas$FECHA[i],5,nchar(temperaturas$FECHA[i]))
  }
  if (nchar(temperaturas$FECHA[i]) == 7){
    temperaturas$YEAR[i] = substr(temperaturas$FECHA[i],4,nchar(temperaturas$FECHA[i]))
  }
}

head(temperaturas)
temperaturas_2020 <- temperaturas[temperaturas$YEAR == 2020,]
dim(temperaturas_2020)

library(geoR)
library(spdep)
#Lo mergeo con el otro dataset
smn_temp <- merge(x = temperaturas_2020, y = smn, by = "Estacion", all.x = TRUE)
#agrego este dataset general para que no se rompa el graph de temperaturas
smn_temp_gral <- merge(x= temperaturas, y = smn, by="Estacion", all.x=TRUE)
#Filtro los na
smn_temp <- smn_temp %>% filter(!is.na(x) & !is.na(y) & !is.na(TMAX) & !is.na(TMIN))

smn_temp_gral <- smn_temp_gral %>% filter(!is.na(x) & !is.na(y) & !is.na(TMAX) & !is.na(TMIN))
class(smn_temp)
dim(smn_temp)
head(smn_temp)
colnames(smn_temp)

#Ahora agrupo.
temp_max_group <- smn_temp %>% 
  arrange(Estacion, x, y, TMAX) %>% 
  group_by(Estacion) %>%
  mutate(TMAX.med = median(TMAX)) %>%
  slice(1) %>%
  ungroup %>%
  select(x,y,TMAX.med ) 

#temp_max <- temp_max_group[,c(16,17,3)]


smn_temp_gral <- smn_temp_gral %>% filter(!is.na(x) & !is.na(y)) %>% st_as_sf(coords =c("x", "y"), crs= 4326)
#b <- temp_max_gral %>% filter(!is.na(x) & !is.na(y)) %>% st_as_sf(coords =c("x", "y"), crs=4326)
temp_max <- smn_temp_gral[,c(16,3)]
#temp_min <- temp_max_group[,c(16,17,4)]
temp_min <- smn_temp_gral[,c(16,4)]

#hago un analisis de la temperatura 
ggplot() + geom_histogram(data=temp_max, aes(x= TMAX)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank()) +
      ggtitle("Histograma de temperatura mediana") + labs(x= "temperatura máxima")
qqnorm(temp_max$TMAX)


b <- temp_max


ggplot() + geom_sf(data= temp_max, aes(x= temp_max))
plot(b)
class(b)

#Vemos que tenemos más observaciones con temperaturas maximas entre 20 y 30 grados centigrados
# Hay una clara tendencia mirando la latitud, lo que hace sentido en Argentina por tener 
# mayor variación climática de norte a sur que de este a oeste.
temp_max_geodata <- as.geodata(temp_max_group)
class(temp_max_geodata)
plot(temp_max_geodata)

# AUTOCORRELACION ESPACIAL

#PUNTOS
pares <- temp_max_group %>% select(x, y) %>% filter(!is.na(x), !is.na(y))
class(pares)
coordinates(pares) <- ~x+y
#Ahora voy a buscar los vecinos. Este objeto tiene para cada uno con cuantos se relaciona
# Acá habría que analizar bien que pasa con este último parámetro del vecindario que me dice
# que los vecinos están a una distancia menor a 10,15,20,25. Esto cambia mucho los vecindarios
pares_grilla <- dnearneigh(pares, 0, 15)
class(pares_grilla)
#Voy a ver todos los puntos en el mapa y sus relaciones
plot(pares_grilla, pares)

#Hace pesos para los vecinos dividiendo 1 por la cantidad de vecinos que ese punto tiene.
pesos_grilla <- nb2listw(pares_grilla, style = "W", zero.policy=TRUE )
#Vemos que estan autocorrelacionados espacialmente.
options(scipen=20)
moran.test(temp_max_group$TMAX.med, pesos_grilla)

# VARIOGRAMA