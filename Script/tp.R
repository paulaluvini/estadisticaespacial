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
grafico2 <- ggplot() + geom_sf(data =departamentos_tf, aes(fill = personas))
grafico2

#Ahora voy a ver la localización de las estaciones
summary(smn)
colnames(smn)
#Cambio este nombre para que se lea mejor
smn <- smn %>% rename(Estacion = ï..NOMBRE) 
summary(smn$Provincia)
describe(smn)

# Vemos cómo se distribuyen las estaciones por provincia.
# La provincia con mayores estaciones es, por lejos, Buenos Aires. La siguen Córdoba y Santa Fe.
View(smn %>% group_by(Provincia) %>% tally(sort = TRUE))

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
  ggtitle("Estaciones Meteorológicas", subtitle = "Provincia de Buenos Aires") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))

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
  ggtitle("Estaciones Meteorológicas", subtitle = "Area Metropolitana")+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),
        panel.background = element_rect(fill = "aliceblue"))

#Miro el dataset de temperaturas
#ACA ME FALTA MIRAR ESTO BIEN. TIENE NA, FALTA ANALISIS EXPLORATORIO, BOXPLOT DE 
#TEMPERATURAS, HISTOGRAMA, ETC.

head(temperaturas)
temperaturas <- temperaturas %>% rename(Estacion = NOMBRE) 
dim(temperaturas)
colnames(temperaturas)

library(geoR)
library(spdep)
#Lo mergeo con el otro dataset
smn_temp <- merge(x = temperaturas, y = smn, by = "Estacion", all.x = TRUE)
dim(smn_temp)
head(smn_temp)
temp_max <- smn_temp[,c(15,16,3)]
temp_min <- smn_temp[,c(15,16,4)]

b <- temp_max %>% filter(!is.na(x) & !is.na(y)) %>% st_as_sf(coords =c("x", "y"), crs=4326)
plot(b)
class(b)

#Vemos que tenemos más observaciones con temperaturas maximas entre 20 y 40 grados centigrados
c <- temp_max %>% filter(!is.na(x) & !is.na(y))
coordinates(c) <- ~x+y
temp_max_geodata <- as.geodata(c)
class(temp_max_geodata)
plot(temp_max_geodata)

# AUTOCORRELACION ESPACIAL
#Para poder mirar la autocorrelación espacial necesitamos trabajar con objetos sp (Spatial Polygon) 
b <- as_Spatial(b)
class(b)

class(c)
pares <- dat %>% 
  select(lng, lat)
#Es un df y lo tengo que pasar a que sea un data point
class(pares)
c <- temp_max %>% filter(!is.na(x) & !is.na(y))
pares <- c %>% select(x, y)
coordinates(pares) <- ~x+y

pares_grilla <- dnearneigh(pares, 0, .5)
plot(pares_grilla, pares)


###Acá no se si tenemos que trabajar con temperaturas agrupadas o no. Usamos raster??
temperaturas <- temperaturas %>% 
  group_by(Estacion) %>% 
  summarise(TMAX = median(TMAX),
            TMIN = median(TMIN))
dim(temperaturas)